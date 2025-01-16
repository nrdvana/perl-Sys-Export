package Sys::Export::Unix;
use v5.36;
use Carp;
use Cwd 'abs_path';
my $map_file= eval { require File::Map; File::Map->can('map_file') }

=head1 SYNOPSIS

  use Sys::Export::Unix;
  my $exporter= Sys::Export::Unix->new(src => '/', dst => '/initrd');
  $exporter->add('/bin/busybox');

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

=back

Options:

=over

=item dst_tmp

A temporary directory in the same filesystem as L</dst> where this module can prepare temporary
files, then C<rename> them into place.  This prevents any partially-prepared files from ending
up in the destination tree.  If you specify this, it is your responsibility to clean it up,
such as by passing an instance of C<< File::Temp->newdir >>.

By default, this module uses the normaal File::Temp location, unless that path is not on the
same volume as the destination, in which case it will create a temp directory within C<$dst>.

=item on_conflict

Specifies what to do if there is a name collision in the destination.  The default (undef)
causes an exception unless the existing file is identical to the one that would be written.

Setting this to 'overwrite' will unconditionally replace files as it runs.  Setting it to
'ignore' will silently ignore collisions and leave the existing file in place.
Setting it to a coderef will provide you with the path and content thata was about to be
written to it:

  on_conflict => sub ($exporter, $src_path, $dst_path, $content_ref) {
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
   my $abs_dst= abs_path($attrs{dst} =~ s,(?<=[^/])$,/,r)
      or croak "dst directory '$attrs{dst}' does not exist";
   length $abs_dst > 1
      or croak "cowardly refusing to export to '$abs_dst'";
   $attrs{dst}= "$abs_dst/";

   $attrs{dst_tmp} //= do {
      my $tmp= File::Temp->newdir;
      my ($tmp_dev)= stat "$tmp/";
      my ($dst_dev)= stat $attrs{dst};
      $tmp= File::Temp->newdir(DIR => $attrs{dst})
         if $tmp_dev != $dst_dev;
      $tmp;
   };

   bless \%attrs, $class;
}

=head1 ATTRIBUTES

=head2 src

The abs_path of the root of the source filesystem.  Always ends with '/'.

=head2 dst

The abs_path of the root of the destination filesystem.  Always ends with '/'.

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
sub path_set($self) { $self->{path_set} //= {} }
sub uid_set($self) { $self->{uid_set} //= {} }
sub gid_set($self) { $self->{gid_set} //= {} }
sub _link_map($self) { $self->{link_map} //= {} }
sub _elf_interpreters($self) { $self->{elf_interpreters} //= {} }

=head1 METHODS

=head2 rewrite_path

  $exporter->rewrite_path($src_prefix, $dst_prefix);

Add a path rewrite rule which replaces occurrences of $src_prefix with $dst_prefix.
Only one rewrite occurs per path; they don't cascade.

=cut

sub rewrite_path($self, $orig, $new) {
   my $rw= $self->{path_rewrite_map} //= {};
   croak "Conflicting rewrite supplied for '$orig'"
      if exists $rw->{$orig} && $rw->{$orig} ne $new;
   $rw->{$orig}= $new;
   delete $self->{path_rewrite_regex};
   $self;
}

=head2 add

  $exporter->add($src_path);

Add a source path to the export.  This immediately copies the file to the destination, possibly
rewriting paths within it, and then triggering a copy of any libraries or interpreters it
depends on.

=cut

our @also_add;
sub add {
   my $self= shift;
   push @also_add, @_;
   while (@also_add) {
      my $path= shift @also_add;
      $path =~ s,^/,,;
      if (!exists $self->{path_set}{$path}) {
         my $src= $self->{src} . $path;
         my %stat;
         @stat{qw( dev ino mode nlink uid gid rdev size atime mtime ctime )}= lstat $src
            or croak "stat '$src': $!";
         if (-f _) { $self->_export_file($path, \%stat) }
         elsif (-d _) { $self->_export_dir($path, \%stat) }
         elsif (-l _) { $self->_export_symlink($path, \%stat) }
         elsif (-b _ || -c _) { $self->_export_devnode($path, \%stat) }
         elsif (-p _) { $self->_export_fifo($path, \%stat) }
         elsif (-S _) { croak "Can't export sockets: '$src'" }
         else { croak "Can't identify type of '$src'" }
      }
   }
   $self;
}

=head2 skip

  $exporter->skip($path);

Inform the exporter that it should *not* perform any actions for the specified path, presumably
because you're handling that one specially in some other way.

=cut

sub skip($self, $path) {
   $path =~ s,^/,,;
   $self->{path_set}{$path} //= \'skipped';
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
      $map_file->($_[0], $_[1], '<') or die "map_file($_[1]): $!";
   } else {
      open my $fh, '<:raw', $_[1] or die "open($_[1]): $!";
      my $size= -s $fh;
      sysread($fh, $_[0], $size) == $size or die "sysread($_[1], $size): $!";
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

sub _export_file($self, $path, $stat) {
   my $dst_path= $self->get_dst_path($path);
   my $dst= $self->dst . $dst_path;
   # If the file has a link count > 1, check to see if we already have it in the destination
   if ($stat->{nlink} > 1) {
      if (my $already= $self->_link_map->{"$stat->{dev}:$stat->{ino}"}) {
         # Yep, make a link of that file instead of copying again
         $self->_log_action("LNK", $already, $dst_path);
         if (!link($already, $dst)) {
            my $err= $!;
            die "link($already, $dst): $err" unless -e $dst_path;
            $self->_resolve_collision($already, $dst, $err);
         }
         return;
      }
   }
   # Prepare a destination handle
   my $tmp= File::Temp->new(DIR => $self->dst_tmp, UNLINK => 0);
   # Inspect the contents of the file
   _map_file my $contents, $self->src . $path;
   # Check for ELF signature
   my $details= undef;
   if (substr($contents, 0, 4) eq "\x7fELF") {
      my $elf= Sys::Export::ELF::unpack($contents);
      my @deps;
      if ($elf->{dynamic}) {
         if ($self->{needed_libraries}) {
            push @deps, map $self->_resolve_src_library($elf, $_), @{$self->{needed_libraries}};
         }
         if ($self->{interpreter}) {
            $self->_elf_interpreters->{$self->{interpreter}}= 1;
            push @deps, $self->{interpreter};
         }
      }
      _syswrite_all($tmp, \$contents);
      # If any dep gets its path rewritten, also need to add an rpath for this binary
      if (...) {
         ... 
         say "  +patchelf";
         `patchelf --set-interpreter $interp --set-rpath $rpath '$dst'`;
         $? == 0 or die "patchelf on '$dst' failed";
      }
   } elsif (my ($interp, $args)= ($contents =~ /^#!\s*(\S*)\s*(.*)/)) {
      # Rewrite paths in shell scripts, but only warn about others
      my $rre= $self->path_rewrite_regex;
      if ($contents =~ $rre) {
         if ($interp =~ /\b(bash|ash|dash|sh)$/) {
            _syswrite_all($tmp, \$self->_rewrite_shell($contents));
         } else {
            warn "$path is a script referencing a rewritten path, but don't know how to process it\n";
            _syswrite_all($tmp, \$contents);
         }
      } else {
         _syswrite_all($tmp, \$contents);
      }
   } else {
      # just copy it
      _syswrite_all($tmp, \$contents);
   }
   # Apply matching permissions and ownership
   chown($stat->{uid}, $stat->{gid}, $tmp) || die "chown($uid, $gid, $tmp): $!";
   chmod($stat->{mode} & 0xFFF, $tmp) || die sprintf("chmod(0%o, %s): %s", $stat->{mode} & 0xFFF, $tmp, $!);

   # Rename the temp file into place
   $self->_log_action("CPY", $path, $dst_path, $details);
   if (!rename("$tmp", $dst)) {
      my $err= $!;
      die "rename($tmp, $dst): $err" unless -e $dst_path;
      $self->_resolve_collision($tmp, $dst, $err);
   }
}

sub _export_symlink($self, $path) {
}

sub _rewrite_shell($self, $contents) {
   # only replace path matches when following certain characters which
   # indicate the start of a path.
   $contents =~ s/(?<=[ '"><\n#])$rre/$self->{path_rewrite_map}{$1}/ger;
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
   $patchelf
}

1;
