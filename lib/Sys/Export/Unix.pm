package Sys::Export::Unix;
use v5.36;
use Carp;
use Cwd 'abs_path';
use Fcntl qw( S_ISREG S_ISDIR S_IFDIR S_ISLNK S_ISBLK S_ISCHR S_ISFIFO S_ISSOCK S_ISWHT );
require File::Temp;
our $have_file_map= eval { require File::Map; };
our $have_unix_mknod= eval { require Unix::Mknod; };

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
   $attrs{src_abs}= $abs_src eq '/'? $abs_src : "$abs_src/";

   defined $attrs{dst} or croak "Require 'dst' attribute";
   unless (ref $attrs{dst} eq 'CODE') {
      my $dst_abs= abs_path($attrs{dst} =~ s,(?<=[^/])$,/,r)
         or croak "dst directory '$attrs{dst}' does not exist";
      length $dst_abs > 1
         or croak "cowardly refusing to export to '$dst_abs'";
      $attrs{dst_abs}= "$dst_abs/";
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

The root of the source filesystem.

=head2 src_abs

The C<abs_path> of the root of the source filesystem, always ending with '/'.

=head2 dst

The root of the destination filesystem, OR a coderef which receives files which are ready to be
recorded.

=head2 dst_abs

The C<abs_path> of the root of the destination filesystem, always ending with '/'.
Only defined if L<dst> is not a coderef.

=head2 tmp

The C<abs_path> of a directory to use for temporary staging before renaming into L</dst>.

=head2 src_path_set

A hashref of all source paths which have been processed, and which destination path they were
written as.  All paths relative, without a leading slash.

=head2 dst_path_set

A hashref of all destination paths which have been created (as keys).  If the value of the key
is defined, it is the source path.  If not defined, it means the destination was created
without reference to a source path.

=head2 uid_set

The set of numeric user IDs seen while copying paths.

=head2 gid_set

The set of numeric group IDs seen while copying paths.

=cut

sub src($self)          { $self->{src} }
sub src_abs($self)      { $self->{src_abs} }
sub dst($self)          { $self->{dst} }
sub dst_abs($self)      { $self->{dst_abs} }
sub tmp($self)          { $self->{tmp} }
sub src_path_set($self) { $self->{src_path_set} //= {} }
sub dst_path_set($self) { $self->{dst_path_set} //= {} }
sub uid_set($self)      { $self->{uid_set} //= {} }
sub gid_set($self)      { $self->{gid_set} //= {} }

=head2 path_rewrite_regex

A regex that matches the longest prefix of a source path having a rewrite rule.

=cut

sub path_rewrite_regex($self) {
   $self->{path_rewrite_regex} //= do {
      my $alt= join '|', map quotemeta, reverse sort keys %{$self->{path_rewrite_map} // {}};
      length $alt? qr/($alt)/ : qr/(*FAIL)/;
   };
}

# a hashref tracking files with link-count higher than 1, so that hardlinks can be preserved.
# the keys are "$dev:$ino"
sub _link_map($self) { $self->{link_map} //= {} }

# a hashref listing all the interpreters that have been discovered for programs
# and scripts copied to dst.  The keys are the relative source path.
sub _elf_interpreters($self) { $self->{elf_interpreters} //= {} }

sub DESTROY($self, @) {
   $self->finish if $self->{_delayed_apply_stat};
}

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
   $orig =~ s,^/,,;
   $new =~ s,^/,,;
   $orig !~ m,^[.]+/, && $new !~ m,^[.]+/,
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
      elsif (S_ISLNK($mode)) {
         return undef if --$lim <= 0;
         defined (my $newpath= readlink $abs) or return undef;
         @abs= @base if $newpath =~ m,^/,;
         unshift @parts, grep length && $_ ne '.', split '/', $newpath;
      }
      else {
         push @abs, $part;
      }
   }
   return join '/', @abs[scalar @base .. $#abs];
}

sub _src_abs_path($self, $path) {
   _chroot_abs_path($self->{src_abs}, $path);
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

sub _log_action($self, $verb, $src, $dst, @notes) {
   printf "%3s %-20s -> %s\n", $verb, $src, $dst;
   printf "     %s\n", $_ for @notes;
}

sub add {
   my $self= shift;
   my $add= ($self->{add} //= []);
   push @$add, @_;
   while (@$add) {
      my $next= shift @$add;
      my %file;
      if (ref $next eq 'HASH') {
         %file= %$next;
      } else {
         $next =~ s,^/,,;
         @file{qw( dev ino mode nlink uid gid rdev size atime mtime ctime )}= lstat($self->{src_abs}.$next)
            or croak "lstat '$self->{src_abs}$next': $!";
         # If $next is itself a symlink, we want to export this name, and also the thing
         # it references.
         if (S_ISLNK($file{mode})) {
            # resolve symlinks in the path leading up to this symlink
            my $parent= $next =~ s,[^/]+$,,r;
            $next= $self->_src_abs_path($parent) . ($next =~ m,(/[^/]+)$,)[0]
               if length $parent;
            # add this symlink target to the paths to be exported
            my $target= readlink($self->{src_abs}.$next);
            $target= $parent . $target unless $target =~ m,^/,;
            unshift @$add, $target;
         } else {
            # Resolve symlinks within src/ to get the true identity of this file
            my $abs= $self->_src_abs_path($next);
            defined $abs or croak "Can't resolve absolute path of '$next'";
            $next= $abs;
         }
         # ignore repeat requests
         next if exists $self->{src_path_set}{$next};
         $file{src_path}= $next;
         $file{data_path}= $self->{src_abs} . $next;
         $file{name}= $self->get_dst_for_src($next);
         $self->{src_path_set}{$next}= $file{name};
      }

      # Has this destination already been written?
      if (defined(my $orig= $self->{dst_path_set}{$file{name}})) {
         if (!$self->on_collision) {
            croak "Already wrote a file '$file{name}'".(length $orig? " which came from $orig":"");
         } elsif ($self->on_collision eq 'ignore') {
            next;
         } elsif ($self->on_collision eq 'overwrite') {
            unlink $self->dst_abs . $file{name}
               if defined $self->dst_abs;
         } else {
            $self->on_collision->($self, \%file, $orig);
         }
      }
      # Else make sure the parent directory *has* been written
      else {
         my $dst_parent= $file{name} =~ s,/?[^/]+$,,r;
         if (length $dst_parent && !exists $self->{dst_path_set}{$dst_parent}) {
            # if writing to a real dir, check whether it already exists by some other means
            if (ref $self->dst ne 'CODE' && -d $self->dst_abs . $dst_parent) {
               # no need to do anything, but record that we have it
               $self->{dst_path_set}{$dst_parent}= undef;
            }
            else {
               # Determine which directory to copy permissions from
               my $src_parent= !defined $file{src_path}? undef
                  : $file{src_path} =~ s,/?[^/]+$,,r;
               # If no rewrites, src_parent is the same as dst_parent
               if (!$self->_has_rewrites) {
                  $src_parent //= $dst_parent;
               }
               elsif (!defined $src_parent || $self->get_dst_for_src($src_parent) ne $dst_parent) {
                  # No src_path means we don't have an origin for this file, so no official
                  # origin for its parent directory, either.  But, maybe a directory of the
                  # same name exists in src_path.
                  # If so, use it, else create a generic directory.
                  my %dir= ( name => $dst_parent );
                  if ((@dir{qw( dev ino mode nlink uid gid rdev size atime mtime ctime )}
                     = lstat $self->{src_abs} . $dst_parent)
                     && S_ISDIR($dir{mode})
                  ) {
                     $src_parent= \%dir;
                  } else {
                     $src_parent= { name => $dst_parent, mode => (S_IFDIR | 0755) };
                  }
               }
               unshift @$add, $src_parent, \%file;
               next;
            }
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
   $self->{src_path_set}{$path} //= undef;
   $self;
}

=head2 finish

Apply any postponed changes to the destination filesystem.  For instance, this applies mtimes
to directories since writing the contents to the directory would have changed the mtime.

=cut

sub finish($self) {
   if ($self->{_delayed_apply_stat}) {
      # Reverse sort causes child directories to be updated before parents,
      # which is required for updating mtimes.
      $self->_delayed_apply_stat(@$_)
         for sort { $b->[0] cmp $a->[0] } @{$self->{_delayed_apply_stat}};
   }
}

=head2 get_dst_for_src

  my $dst_path= $exporter->get_dst_for_src($src_path);

Returns the relative destination path for a relative source path, rewritten according to the
rewrite rules.  If no rewrites exist, this just returns C<$src_path>.

=cut

sub get_dst_for_src($self, $path) {
   my $rre= $self->path_rewrite_regex;
   return scalar($path =~ s/^$rre/$self->{path_rewrite_map}{$1}/er);
}

sub _syswrite_all($tmp, $content_ref) {
   my $ofs= 0;
   again:
   my $wrote= $tmp->syswrite($$content_ref, length($$content_ref) - $ofs, $ofs);
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
            my $dst= $self->dst_abs . $file->{name};
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
      _load_or_map_file($file->{data}, $file->{data_path});
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
         _load_or_map_data($file->{data}, $file->{data_path});
      }
      $self->_log_action("CPY", $file->{src_path} // '(data)', $file->{name}, @notes);
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
      $self->_apply_stat("$tmp", $file);
      # Rename the temp file into place
      $self->_log_action("CPY", $file->{src_path} // '(data)', $file->{name}, @notes);
      my $dst= $self->dst_abs . $file->{name};
      rename($tmp, $dst)
         or croak "rename($tmp, $dst): $!";
   }
}

sub _resolve_src_library($self, $libname, $rpath) {
   my @paths= (length $rpath? (grep length, split /:/, $rpath) : (), qw( lib lib64 usr/lib usr/lib64 ));
   for my $path (@paths) {
      return "$path/$libname" if -e "$path/$libname";
   }
   return ();
}

sub _export_elf_file($self, $file, $notes) {
   require Sys::Export::ELF;
   my $elf= Sys::Export::ELF::unpack($file->{data});
   my ($interpreter, @libs);
   if ($elf->{dynamic}) {
      if ($elf->{needed_libraries}) {
         for (@{$elf->{needed_libraries}}) {
            my $lib= $self->_resolve_src_library($_, $elf->{rpath}) // carp("Can't find lib $_ needed for $file");
            push @libs, $lib if $lib;
         }
         push @{$self->{add}}, @libs;
      }
      if ($elf->{interpreter}) {
         $self->_elf_interpreters->{$elf->{interpreter}}= 1;
         $interpreter= $elf->{interpreter};
         push @{$self->{add}}, $interpreter;
      }
   }
   # Is any path rewriting requested?
   if ($self->_has_rewrites && length $file->{src_path} && defined $interpreter) {
      # If any dep gets its path rewritten, need to modify interpreter and/or rpath
      my $rre= $self->path_rewrite_regex;
      if (grep m/^$rre/, $interpreter, @libs) {
         $interpreter= $self->get_dst_for_src($interpreter);
         my %rpath;
         for (@libs) {
            my $dst_lib= $self->get_dst_for_src($_);
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
   push @{$self->{add}}, $interp;

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

sub _export_dir($self, $dir) {
   $self->_log_action('DIR', $dir->{src_path} // '(default)', $dir->{name});
   if (ref $self->dst eq 'CODE') {
      $self->dst->($self, $dir);
   } else {
      my $dst_abs= $self->dst_abs . $dir->{name};
      mkdir($dst_abs)
         or croak "mkdir($dst_abs): $!";
      $self->_apply_stat($dst_abs, $dir);
   }
}

sub _export_symlink($self, $file) {
   if (!exists $file->{data}) {
      exists $file->{data_path}
         or croak "Symlink must contain 'data' or 'data_path'";
      defined( $file->{data}= readlink($file->{data_path}) )
         or croak "readlink($file->{data_path}): $!";
   }

   if ($self->_has_rewrites && length $file->{src_path}) {
      # Absolute links just need a simple rewrite on the target
      if ($file->{data} =~ m,^/,) {
         $file->{data}= $self->get_dst_for_src($file->{data});
      }
      # Relative links are tricky.  A "100%" solution might actually be impossible, because
      # users could intend for all sorts of different behavior with symlinks, but at least try
      # to DWIM here.
      else {
         # Example:  /usr/local/bin/foo -> ../../bin/bar, but both paths are being rewritten to /bin
         #   The correct symlink is then just /bin/foo -> bar
         # Example:  /usr/local/share/mydata -> ../../../opt/mydata, but /opt/mydata is a
         #   symlink to /opt/mydata-1.2.3, and /usr/local/share is getting rewritten to /share.
         #   The user may want this double redirection to remain so that mydata can be swapped
         #   for different versions, so can't just resolve everything to an absolute path.
         #   The correct symlink should probably be /share/mydata -> ../opt/mydata
         # Example:  /usr/local/share/mydata/lib -> ../../../../opt/mydata/current/../lib
         #   where /usr/local/share is getting rewritten and /opt/mydata is getting rewritten,
         #   and /opt/mydata/current is a symlink that breaks assumptions about '..'
         #   The correct symlink should probably be /share/mydata/lib -> ../../opt/mydata/current/../lib
         #   Note that /opt/mydata/current symlink might not even exist in dst yet (to be able
         #    to resolve it) and resolving the one in src might not be what the user wants.
         
         # I think the answer here is to consume all leading '..' in the symlink path
         # (src_path is already absolute, so no danger of '..' meaning something different)
         # then add all following non-'..' to arrive at a new src_target, then rewrite that to
         # the corresponding dst_target, then create a relative path from the dst symlink to
         # that dst_path, then append any additional portions of the original symlink as-is.
         my @src_parts= split '/', $file->{src_path};
         pop @src_parts; # discard name of symlink itself
         my @target_parts= grep $_ ne '.', split '/', $file->{data};
         while (@target_parts && $target_parts[0] eq '..') {
            shift @target_parts;
            pop @src_parts;
         }
         while (@target_parts && $target_parts[0] ne '..') {
            push @src_parts, shift @target_parts;
         }
         my @dst_target= split '/', $self->get_dst_for_src(join '/', @src_parts);
         # now construct a relative path from $file->{name} to $dst_target
         my @dst_parts= split '/', $file->{name};
         pop @dst_parts; # discard name of symlink itself
         # remove common prefix
         while (@dst_parts && @dst_target && $dst_parts[0] eq $dst_target[0]) {
            shift @dst_parts;
            shift @dst_target;
         }
         # assemble '..' for each remaining piece of dst_parts, then the path to dst-target,
         # then the remainder of original path components (if any)
         $file->{data}= join '/', (('..') x scalar @dst_parts), @dst_target, @target_parts;
      }
   }

   $self->_log_action('SYM', $file->{data}, $file->{name});
   if (ref($self->dst) eq 'CODE') {
      $self->dst->($self, $file);
   } else {
      my $dst_abs= $self->dst_abs . $file->{name};
      symlink($file->{data}, $dst_abs)
         or croak "symlink($file->{data}, $dst_abs): $!";
      $self->_apply_stat($dst_abs, $file);
   }
}

sub _export_devnode($self, $file) {
   my ($major,$minor)= _dev_major_minor($file->{rdev});
   $self->_log_action(S_ISBLK($file->{mode})? 'BLK' : 'CHR', "$major:$minor", $file->{name});
   if (ref $self->dst eq 'CODE') {
      $file->{rdev_major} //= $major;
      $file->{rdev_minor} //= $minor;
      $self->dst->($self, $file);
   } else {
      my $dst_abs= $self->dst_abs . $file->{name};
      _mknod_or_die($dst_abs, $file->{mode}, $file->{rdev});
      $self->_apply_stat($dst_abs, $file);
   }
}

sub _export_fifo($self, $file) {
   $self->_log_action("FIO", "(fifo)", $file->{name});
   if (ref $self->dst eq 'CODE') {
      $self->dst->($self, $file);
   } else {
      require POSIX;
      my $dst_abs= $self->dst_abs . $file->{name};
      POSIX::mkfifo($dst_abs, $file->{mode})
         or croak "mkfifo($dst_abs): $!";
      $self->_apply_stat($dst_abs, $file);
   }
}

# Apply permissions and mtime to a path
sub _apply_stat($self, $abs_path, $stat) {
   my ($mode, $uid, $gid, $atime, $mtime)= (lstat $abs_path)[2,4,5,8,9];
   my $change_uid= defined $stat->{uid} && $stat->{uid} != $uid;
   my $change_gid= defined $stat->{gid} && $stat->{gid} != $gid;
   if ($change_uid || $change_gid) {
      # only UID 0 can change UID, and only GID 0 or GID in supplemental groups can change GID.
      $uid= -1 unless $change_uid && $> == 0;
      $gid= -1 unless $change_gid && ($) == 0 || grep $stat->{gid}, split / /, $) );
      # Only attempt change if able
      POSIX::lchown($uid, $gid, $abs_path) or croak "lchown($uid, $gid, $abs_path): $!"
         if $uid >= 0 || $gid >= 0;
   }
   $self->uid_set->{$uid}++ if $uid >= 0;
   $self->gid_set->{$gid}++ if $gid >= 0;

   my @delayed;

   # Don't change permission bits on symlinks
   if (!S_ISLNK($mode) && ($mode & 0xFFF) != ($stat->{mode} & 0xFFF)) {
      # If changing permissions on a directory to something that removes our ability
      # to write to it, delay this change until the end.
      if (S_ISDIR($mode) && !(($stat->{mode} & 0222) && ($stat->{mode} & 0111))) {
         push @delayed, 'chmod';
      }
      else {
         chmod $stat->{mode}&0xFFF, $abs_path
            or croak sprintf "chmod(0%o, %s): $!", $stat->{mode}&0xFFF, $abs_path;
      }
   }

   if (!S_ISLNK($mode) && (defined $stat->{mtime} || defined $stat->{atime})) {
      if (S_ISDIR($mode)) {
         # No point in applying mtime to a directory now, because it will get
         # changed when sub-entries get written.
         push @delayed, 'utime';
      }
      else {
         utime $stat->{atime}, $stat->{mtime}, $abs_path
            or warn "utime($abs_path): $!";
      }
   }

   push @{$self->{_delayed_apply_stat}}, [ $abs_path, $stat, @delayed ]
      if @delayed;
}
sub _delayed_apply_stat($self, $abs_path, $stat, @delayed) {
   if (grep $_ eq 'chmod', @delayed) {
      chmod $stat->{mode}&0xFFF, $abs_path
         or croak sprintf "chmod(0%o, %s): $!", $stat->{mode}&0xFFF, $abs_path;
   }
   if (grep $_ eq 'utime', @delayed) {
      utime $stat->{atime}, $stat->{mtime}, $abs_path
         or warn "utime($abs_path): $!";
   }
}

# _load_file(my $buffer, $filenme)
sub _load_file {
   open my $fh, "<:raw", $_[1]
      or die "open($_[1]): $!";
   my $size= -s $fh;
   sysread($fh, ($_[0] //= ''), $size) == $size
      or die "sysread($_[1], $size): $!";
}
*_load_or_map_file= $have_file_map? sub { File::Map::map_file($_[0], $_[1], "<") }
   : *_load_file;

sub _linux_major_minor($dev) {
   use integer;
   ( (($dev >> 8) & 0xfff) | (($dev >> 31 >> 1) & 0xfffff000) ),
   ( ($dev & 0xff) | (($dev >> 12) & 0xffffff00) )
}

*_dev_major_minor= $have_unix_mknod? sub { Unix::Mknod::major($_[0]), Unix::Mknod::minor($_[0]) }
   : *_linux_major_minor;

sub _system_mknod($path, $mode, $dev) {
   my @args= ("mknod", "-m", sprintf("0%o", $mode & 0xFFF),
      $path, S_ISBLK($mode)? "b":"c", _dev_major_minor($dev));
   system(@args) == 0
      or croak "mknod @args failed";
}

*_mknod_or_die= $have_unix_mknod? sub { Unix::Mknod::mknod(@_) or croak "mknod($_[0]): $!" }
   : *_system_mknod;

sub _capture_cmd {
   require IPC::Open3;
   require Symbol;
   my $pid= open3(undef, my $out_fh, my $err_fh= Symbol::gensym(), @_)
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
