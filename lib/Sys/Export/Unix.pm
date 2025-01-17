package Sys::Export::Unix;
use v5.36;
use Carp;
use Cwd 'abs_path';
use Fcntl qw( S_ISREG S_ISDIR S_ISLNK S_ISBLK S_ISCHR S_ISFIFO S_ISSOCK S_ISWHT );
require File::Temp;
my $map_file= eval { require File::Map; File::Map->can('map_file') };

=head1 SYNOPSIS

  use Sys::Export::Unix;
  my $exporter= Sys::Export::Unix->new(src => '/', dst => '/initrd');
  $exporter->add('bin/busybox');

=head1 DESCRIPTION

This object contains the logic for exporting unix-style systems.

=head1 CONSTRUCTOR

  Sys::Export::Unix->new(\%attributes); # hashref
  Sys::Export::Unix->new(%attributes);  # key/value list

Required attributes:

=over

=item src

The root of the system to export from (often '/', but you must specify this)

=item dst

The root of the exported system.  This directory must exist, and should be empty unless you
specify 'on_conflict'.

It can also be a coderef, which avoids the entire construction of a staging directory, and
doesn't require root permission to operate.  Your coderef could do something like write
directly into a CPIO archive:

  my $cpio= Sys::Export::CPIO->new($filename);
  my $exporter= Sys::Export->new(dst => sub { $cpio->append($_[1]) });

=back

Options:

=over

=item tmp

A temporary directory in the same filesystem as L</dst> where this module can prepare temporary
files, then C<rename> them into place.  This prevents any partially-prepared files from ending
up in the destination tree.  If you specify this, it is your responsibility to clean it up,
such as by passing an instance of C<< File::Temp->newdir >>.

By default, this module uses the normaal File::Temp location, unless that path is not on the
same volume as the destination, in which case it will create a temp directory within C<$dst>.

=item on_collision

Specifies what to do if there is a name collision in the destination.  The default (undef)
causes an exception unless the existing file is identical to the one that would be written.

Setting this to 'overwrite' will unconditionally replace files as it runs.  Setting it to
'ignore' will silently ignore collisions and leave the existing file in place.
Setting it to a coderef will provide you with the path and content thata was about to be
written to it:

  on_collision => sub ($exporter, $fileinfo, $prev_src_path) {
    # src_path is relative to $exporter->src
    # dst_path is relative to $exporter->dst
    # content_ref is a scalar ref with the new contents of the file, possibly rewritten

=cut

sub new {
   my $class= shift;
   my %attrs= @_ == 1 && ref $_[0] eq 'HASH'? %{$_[0]}
      : !(@_ & 1)? @_
      : croak "Expected hashref or even-length list";

   defined $attrs{src} or croak "Require 'src' attribute";
   my $abs_src= abs_path($attrs{src} =~ s,(?<=[^/])$,/,r)
      or croak "src directory '$attrs{src}' does not exist";
   $attrs{src}= $abs_src eq '/'? $abs_src : "$abs_src/";

   defined $attrs{dst} or croak "Require 'dst' attribute";
   unless (ref $attrs{dst} eq 'CODE') {
      my $abs_dst= abs_path($attrs{dst} =~ s,(?<=[^/])$,/,r)
         or croak "dst directory '$attrs{dst}' does not exist";
      length $abs_dst > 1
         or croak "cowardly refusing to export to '$abs_dst'";
      $attrs{dst}= "$abs_dst/";
   }

   $attrs{tmp} //= do {
      my $tmp= File::Temp->newdir;
      unless (ref $attrs{dst} eq 'CODE') {
         # Make sure can rename() from this $tmp to $dst
         my ($tmp_dev)= stat "$tmp/";
         my ($dst_dev)= stat $attrs{dst};
         $tmp= File::Temp->newdir(DIR => $attrs{dst})
            if $tmp_dev != $dst_dev;
      }
      $tmp;
   };

   bless \%attrs, $class;
}

=head1 ATTRIBUTES

=head2 src

The abs_path of the root of the source filesystem.  Always ends with '/'.

=head2 dst

The abs_path of the root of the destination filesystem (always ends with '/')
OR, a coderef which reseives files which are ready to be recorded.

=head2 tmp

The abs_path of a directory to use for temporary staging before renaming into L</dst>.

=head2 path_set

A hashref of all source paths which have been processed, and which destination path they were
written as.  All paths are stored as relative, without a leading slash.

=head2 path_rewrite_regex

A regex that matches the longest prefix of a source path having a rewrite rule.

=head2 uid_set

The set of numeric user IDs seen while copying paths.

=head2 gid_set

The set of numeric group IDs seen while copying paths.

=cut

sub src($self) { $self->{src} }
sub dst($self) { $self->{dst} }
sub dst_tmp($self) { $self->{dst_tmp} }
sub path_rewrite_regex($self) {
   $self->{path_rewrite_regex} //= do {
      my $alt= join '|', map quotemeta, reverse sort keys %{$self->{path_rewrite_map} // {}};
      length $alt? qr/($alt)/ : qr/(*FAIL)/;
   };
}
sub src_path_set($self) { $self->{src_path_set} //= {} }
sub dst_path_set($self) { $self->{dst_path_set} //= {} }
sub uid_set($self) { $self->{uid_set} //= {} }
sub gid_set($self) { $self->{gid_set} //= {} }
sub _link_map($self) { $self->{link_map} //= {} }
sub _elf_interpreters($self) { $self->{elf_interpreters} //= {} }

=head1 METHODS

=head2 rewrite_path

  $exporter->rewrite_path($src_prefix, $dst_prefix);

Add a path rewrite rule which replaces occurrences of $src_prefix with $dst_prefix.
Only one rewrite occurs per path; they don't cascade.  Paths must be absolute starting with '/'.

=cut

sub rewrite_path($self, $orig, $new) {
   my $rw= $self->{path_rewrite_map} //= {};
   croak "Conflicting rewrite supplied for '$orig'"
      if exists $rw->{$orig} && $rw->{$orig} ne $new;
   $orig =~ m,^/, && $new =~ m,^/,
      or croak "Paths for rewrite_path must be absolute ($orig => $new)";
   $rw->{$orig}= $new;
   delete $self->{path_rewrite_regex};
   $self;
}

sub _has_rewrites($self) {
   $self->{path_rewrite_map} && %{$self->{path_rewrite_map}}
}

# Resolve symlinks in paths within $root/ treating absolute links as references to $root.
# This returns undef if:
#   * a '..' component tries to exit the src/ root
#   * the path doesn't exist at any point during resolution
#   * 'stat' fails at any point in the path (maybe for permissions)
#   * it resolves more than 256 symlinks
#   * readlink fails
# Un-intuitively, this returns a string without a leading '/' because that's what I need below.
sub _chroot_abs_path($root, $path) {
   my @base= split '/', $root;
   my @abs= @base;
   my @parts= grep length && $_ ne '.', split '/', $path;
   my $lim= 256;
   while (@parts) {
      #use DDP; &p({ base => \@base, abs => \@abs, parts => \@parts });
      my $part= shift @parts;
      my $abs= join '/', @abs, $part;
      my (undef, undef, $mode)= lstat $abs
         or return undef;
      if ($part eq '..') {
         return undef if @abs <= @base;
         pop @abs;
      }
      elsif (S_ISDIR($mode)) {
         push @abs, $part;
      }
      elsif (S_ISLNK($mode)) {
         return undef if --$lim <= 0;
         defined (my $newpath= readlink $abs) or return undef;
         @abs= @base if $newpath =~ m,^/,;
         unshift @parts, grep length && $_ ne '.', split '/', $newpath;
      }
   }
   return join '/', @abs[scalar @base .. $#abs];
}

sub _src_abs_path($self, $path) {
   _chroot_abs_path($self->src, $path);
}

=head2 add

  $exporter->add($src_path);
  $exporter->add(\%file_attrs);

Add a source path to the export.  This immediately copies the file to the destination, possibly
rewriting paths within it, and then triggering a copy of any libraries or interpreters it
depends on.

If specified directly, file attributes are:

  name            # relative destination path
  src_path        # relative source path
  data            # literal data content of file (must be bytes, not unicode)
  data_path       # absolute path of file to load 'data' from
  dev             # device, from stat
  dev_major       # major(dev), if you know it and don't know 'dev'
  dev_minor       # minor(dev), if you know it and don't know 'dev'
  ino             # inode, from stat
  mode            # permissions and type, as per stat
  nlink           # number of hard links
  uid             # user id
  gid             # group id
  rdev            # referenced device, for device nodes
  rdev_major      # major(rdev), if you know it and don't know 'rdev'
  rdev_minor      # minor(rdev), if you know it and don't know 'rdev'
  size            # size, in bytes.  Can be ommitted if 'data' is present
  mtime           # modification time, as per stat

If you don't specify src_path, path rewrites will not be applied to the contents of the file or
symlink (on the assumption that you used paths relative to the destination).

=cut

our @add;
sub add {
   my $self= shift;
   push @add, @_;
   while (@add) {
      my $next= shift @add;
      my %file;
      if (ref $next eq 'HASH') {
         %file= %$next;
      } else {
         $next =~ s,^/,,;
         # Resolve symlinks within src/ to get the true identity of this file
         my $path= _chroot_abs_path($self->src, $next);
         # ignore repeat requests
         next if exists $self->{src_path_set}{$path};
         $file{src_path}= $path;
         $file{data_path}= $self->{src} . $path;
         $file{name}= $self->get_dst_path($path);
         $self->{src_path_set}{$path}= $file{name};
         @file{qw( dev ino mode nlink uid gid rdev size atime mtime ctime )}= lstat $file{data_path}
            or croak "stat '$file{data_path}': $!";
      }
      # Has this destination already been written?
      if (defined(my $orig= $self->{dst_path_set}{$file{name}})) {
         if (!$self->on_collision) {
            croak "Already wrote a file '$file{name}'".(length $orig? " which came from $orig":"");
         } elsif ($self->on_collision eq 'ignore') {
            next;
         } elsif ($self->on_collision eq 'overwrite') {
            unlink $self->dst . $file{name} unless ref $self->dst;
         } else {
            $self->on_collision->($self, \%file, $orig);
         }
      }
      $self->{dst_path_set}{$file{name}}= $file{src_path};

      my $mode= $file{mode};
      if (S_ISREG($mode)) { $self->_export_file(\%file) }
      elsif (S_ISDIR($mode)) { $self->_export_dir(\%file) }
      elsif (S_ISLNK($mode)) { $self->_export_symlink(\%file) }
      elsif (S_ISBLK($mode) || S_ISCHR($mode)) { $self->_export_devnode(\%file) }
      elsif (S_ISFIFO($mode)) { $self->_export_fifo(\%file) }
      else {
         croak "Can't export ".(S_ISSOCK($mode)? 'sockets' : S_ISWHT($mode)? 'whiteout entries' : '(unknown)')
            .': "'.($file{src_path} // $file{data_path} // $file{name}).'"'
      }
   }
   $self;
}

=head2 skip

  $exporter->skip($src_path);

Inform the exporter that it should *not* perform any actions for the specified source path,
presumably because you're handling that one specially in some other way.

=cut

sub skip($self, $path) {
   $path =~ s,^/,,;
   $self->{src_path_set}{$path} //= \'skipped';
   $self;
}

=head2 get_dst_path

  my $dst_path= $exporter->get_dst_path($src_path);

Returns the relative destination path for a relative source path, rewritten according to the
rewrite rules.  If no rewrites exist, this just returns C<$src_path>.

=cut

sub get_dst_path($self, $path) {
   my $rre= $self->path_rewrite_regex;
   return scalar($path =~ s/^$rre/$self->{path_rewrite_map}{$1}/er);
}

sub _map_file {
   if ($map_file) {
      $map_file->($_[0], $_[1], '<')
         or die "map_file($_[1]): $!";
   } else {
      open my $fh, '<:raw', $_[1]
         or die "open($_[1]): $!";
      my $size= -s $fh;
      sysread($fh, $_[0], $size) == $size
         or die "sysread($_[1], $size): $!";
   }
}

sub _syswrite_all($tmp, $content_ref) {
   my $ofs= 0;
   again:
   my $wrote= $tmp->syswrite($$content_ref, $ofs);
   if ($ofs+$wrote != length $$content_ref) {
      if ($wrote > 0) { $ofs += $wrote; goto again; }
      elsif ($!{EAGAIN} || $!{EINTR}) { goto again; }
      else { die "syswrite($tmp): $!" }
   }
   $tmp->close or die "close($tmp): $!";
}

sub _export_file($self, $file) {
   # If the file has a link count > 1, check to see if we already have it in the destination
   if ($file->{nlink} > 1) {
      if (my $already= $self->_link_map->{"$file->{dev}:$file->{ino}"}) {
         # Yep, make a link of that file instead of copying again
         $self->_log_action("LNK", $already, $file->{name});
         if (ref $self->dst eq 'CODE') { # CPIO stream
            $self->dst->($self, $file);
         } else {
            my $dst= $self->dst . $file->{name};
            link($already, $dst)
               or croak "link($already, $dst): $!";
         }
         return;
      }
   }
   # Load the data, unless already provided
   unless (exists $file->{data}) {
      defined $file->{data_path}
         or croak "For regular files, must specify ->{data} or ->{data_path}";
      _map_file($file->{data}, $file->{data_path});
   }
   my @notes;
   # Check for ELF signature
   if (substr($file->{data}, 0, 4) eq "\x7fELF") {
      $self->_export_elf_file($file, \@notes);
   } elsif ($file->{data} =~ m,^#!\s*/,) {
      $self->_export_script_file($file, \@notes);
   }
   # If writing to an API, load the file data
   if (ref($self->dst) eq 'CODE') {
      # reload temp file if one was used
      if (!exists $file->{data}) {
         _map_file($file->{data}, $file->{data_path});
      }
      $self->_log_action("CPY", $file->{src_path} // '(data)', $file->{name}, join ' ', @notes);
      $self->dst->($self, $file);
   }
   # else if building a staging directory of files, write data to a file
   else {
      # If a temp file was not used, create it now
      my $tmp= $file->{data_path};
      unless ($tmp && substr($tmp, 0, length $self->tmp) eq $self->tmp) {
         $tmp= File::Temp->new(DIR => $self->tmp, UNLINK => 0);
         _syswrite_all($tmp, \$file->{data});
      }
      # Apply matching permissions and ownership
      chown($file->{uid}, $file->{gid}, $tmp)
         or croak "chown($file->{uid}, $file->{gid}, $tmp): $!";
      chmod($file->{mode} & 0xFFF, $tmp)
         or croak sprintf("chmod(0%o, %s): %s", $file->{mode} & 0xFFF, $tmp, $!);
      # Rename the temp file into place
      $self->_log_action("CPY", $file->{src_path} // '(data)', $file->{name}, join ' ', @notes);
      my $dst= $self->dst . $file->{name};
      rename($tmp, $dst)
         or croak "rename($tmp, $dst): $!";
   }
}

sub _export_elf_file($self, $file, $notes) {
   require Sys::Export::ELF;
   my $elf= Sys::Export::ELF::unpack($file->{data});
   my ($interpreter, @libs);
   if ($elf->{dynamic}) {
      if ($elf->{needed_libraries}) {
         @libs= map $self->_resolve_src_library($_, $elf->{rpath}), @{$elf->{needed_libraries}};
         push @add, @libs;
      }
      if ($elf->{interpreter}) {
         $self->_elf_interpreters->{$elf->{interpreter}}= 1;
         $interpreter= $elf->{interpreter};
         push @add, $interpreter;
      }
   }
   # Is any path rewriting requested?
   if ($self->_has_rewrites && length $file->{src_path} && defined $interpreter) {
      # If any dep gets its path rewritten, need to modify interpreter and/or rpath
      my $rre= $self->path_rewrite_regex;
      if (grep m/^$rre/, $interpreter, @libs) {
         $interpreter= $self->get_dst_path($interpreter);
         my %rpath;
         for (@libs) {
            my $dst_lib= $self->get_dst_path($_);
            $dst_lib =~ s,[^/]+$,,; # path
            $rpath{$dst_lib}= 1;
         }
         my $rpath= join ':', keys %rpath;
         # Create a temporary file so we can run patchelf on it
         my $tmp= File::Temp->new(DIR => $self->tmp, UNLINK => 0);
         _syswrite_all($tmp, \$file->{data});
         _patchelf($tmp, '--set-interpreter' => $interpreter,
            length $rpath? ('--set-rpath' => $rpath) : ());
         delete $file->{data};
         $file->{data_path}= $tmp;
         push @$notes, '+patchelf';
      }
   }
}

sub _export_script_file($self, $file, $notes) {
   # Make sure the interpreter is added, and also rewrite its path
   my ($interp)= ($file->{data} =~ m,^#!\s*(/\S+),)
      or return;
   push @add, $interp;

   if ($self->_has_rewrites && length $file->{src_path}) {
      # rewrite the interpreter, if needed
      my $rre= $self->path_rewrite_regex;
      if ($interp =~ s/^$rre/$self->{path_rewrite_map}{$1}/e) {
         # note file->{data} could be a read-only memory map
         my $data= delete($file->{data}) =~ s/^(#!\s*)(\S+)/$1$interp/r;
         $file->{data}= $data;
      }
      # Scan the source for paths that need rewritten
      if ($file->{data} =~ $rre) {
         # Rewrite paths in shell scripts, but only warn about others.
         # Rewriting perl scripts would basically require a perl parser...
         if ($interp =~ m,/(bash|ash|dash|sh)$,) {
            my $rewritten= $self->_rewrite_shell(delete $file->{data});
            $file->{data}= $rewritten;
            push @$notes, '+rewrite paths';
         } else {
            warn "$file->{src_path} is a script referencing a rewritten path, but don't know how to process it\n";
            push @$notes, "+can't rewrite!";
         }
      }
   }
}

sub _rewrite_shell($self, $contents) {
   my $rre= $self->path_rewrite_regex;
   # only replace path matches when following certain characters which
   # indicate the start of a path.
   $contents =~ s/(?<=[ '"><\n#])$rre/$self->{path_rewrite_map}{$1}/ger;
}

sub _export_symlink($self, $file) {
   if (!exists $file->{data}) {
      exists $file->{data_path}
         or croak "Symlink must contain 'data' or 'data_path'";
      defined( $file->{data}= readlink($file->{data_path}) )
         or croak "readlink($file->{data_path}): $!";
   }

   if ($self->_has_rewrites && length $file->{src_path}) {
      my $target= $file->{data};
      my $is_relative= $target =~ m,^[^/],;
      my $rre= $self->path_rewrite_regex;
      my $src_path= $file->{src_path};
      my $new_path= $file->{name};

      # Is it a relative target?  then combine the original path of the symlink with its
      # relative suffix to determine the absolute target.
      if ($is_relative) {
         # consume any leading "./"
         while ($target =~ s,^[.]/,,) {}
         # for each '..', remove one path component
         ...
      }
      # Calculate the new path
      ...
   }

   $self->_log_action("SYM", $file->{data}, $file->{name});
   if (ref($self->dst) eq 'CODE') {
      $self->dst->($self, $file);
   } else {
      symlink($file->{data}, $self->dst . $file->{name})
         or croak "symlink($file->{data}, $self->dst . $file->{name}): $!";
   }
}

sub _capture_cmd {
   require IPC::Open3;
   require Symbol;
   my $pid= open3(undef, my $out_fh, my $err_fh= Symbol::gensym, @_)
      or die "running @_ failed";
   waitpid($pid, 0);
   my $wstat= $?;
   local $/= undef;
   my $out= <$out_fh>;
   my $err= <$out_fh>;
   return ($out, $err, $wstat);
}

our $patchelf;
sub _patchelf($self, $path, %attrs) {
   unless ($patchelf) {
      chomp($patchelf= `which patchelf`);
      croak "Missing tool 'patchelf'"
         unless $patchelf;
   }
   my ($out, $err, $wstat)= _capture_cmd($patchelf, %attrs, $path);
   $wstat == 0
      or croak "patchelf '$path' failed: $err";
   1;
}

1;
