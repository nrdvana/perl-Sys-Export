package Sys::Export::CPIO;
use v5.36;
use Fcntl qw( S_IFDIR S_IFMT );
use Scalar::Util 'blessed';
use Carp;

=head1 SYNOPSIS

  my $cpio= Sys::Export::CPIO->new($file_name_or_handle, %attrs);
  $cpio->append(\%stat_name_and_data);
  $cpio->append(\%stat_name_and_data);
  $cpio->append(\%stat_name_and_data);
  ...
  # close the file yourself

=head1 DESCRIPTION

This module writes out the cpio "New ASCII Format", as required by Linux initrd.  It does very
little aside from packing the data, but has support for:

=over

=item hardlinks

If you append a file with C<< nlink > 1 >>, it will note the dev/ino and write the data.
If you record a second file also having C<< nlink > 1 >> with the same dev/ino, the size will
be written as zero and the data will be skipped. (this is the way cpio stores hardlinks)

=item rdev

You can pass the C<rdev> received from C<stat>, and this module makes a best-effort to break that
down into the major/minor numbers needed for cpio, but perl doesn't have access to the real
major/minor functions of your platform unless you install L<Unix::Mknod>.  If you were trying
to creaate a device node from pure configuration rather than the filesystem, just pass
C<rdev_major> and C<rdev_minor> instead of C<rdev>.

=item virtual_inodes

Since the original device and inode are not relevant to the initrd loading, this module can
replace the device with 0 and the inode with an incrementing sequence, which should compress
better.

Pass C<< (virtual_inodes => 0) >> to the constructor to disable this feature.

=back

=head1 ATTRIBUTES

=head2 virtual_inodes

This is enabled by default, and rewrites the device_major/device_minor with zeroes and generates
a linear sequence for a virtual inode on each file.

=cut

sub new($class, $f, %attrs) {
   my $fh= blessed $f && $f->can('print')? $f
      : do { open my $x, '>', $f or die "open($f): $!"; $x };
   bless { fh => $fh, seen_inode => {}, ino => 0, virtual_inodes => 1, %attrs }, $class;
}

sub virtual_inodes($self) { $self->{virtual_inodes} }

=head1 METHODS

=head2 append

  $cpio->append({
    dev   => # or, ( dev_major =>, dev_minor => )
    ino   => # 
    mode  => #
    nlink => # same as stat() 
    uid   => # 
    gid   => # 
    mtime => #
    rdev  => # or, ( rdev_major =>, rdev_minor => )
    name  => # full path name, no leading '/'
    data  => # full content of file or symlink
  });

This simply packs the file metadata into a CPIO header, then writes the header, filename, and
data to the stream, padding as necessary.

=cut

BEGIN {
   eval(
      eval { require Unix::Mknod; }
      ? 'sub _major_minor($dev) { Unix::Mknod::major($dev), Unix::Mknod::minor($dev) } 1'
      : 'sub _major_minor($dev) {
         use integer;
         ( (($dev >> 8) & 0xfff) | (($dev >> 31 >> 1) & 0xfffff000) ),
         ( ($dev & 0xff) | (($dev >> 12) & 0xffffff00) )
        } 1'
   ) or die "$@";
}

sub append($self, $fileinfo) {
   my ($dev, $dev_major, $dev_minor, $ino, $mode, $nlink, $uid, $gid, $rdev, $rdev_major, $rdev_minor, $mtime, $name)
      = @{$fileinfo}{qw( dev dev_major dev_minor ino mode nlink uid gid rdev rdev_major rdev_minor mtime name )};
   # best-effort to extract major/minor from dev and rdev, unless user specified them
   ($dev_major, $dev_minor)= _major_minor($dev)
      if defined $dev and !defined $dev_major || !defined $dev_minor;
   ($rdev_major, $rdev_minor)= _major_minor($rdev)
      if defined $rdev and !defined $rdev_major || !defined $rdev_minor;
   defined $mode or croak "require 'mode'";
   defined $name or croak "require 'name'";

   my $size= length($fileinfo->{data}) // 0;
   # Handle hard links
   if ($nlink && $nlink > 1 && ($mode & S_IFMT) != S_IFDIR) {
      my $hardlink_key= "$dev_major:$dev_minor:$ino";
      if ($self->virtual_inodes) {
         # the previous virtual inode is stored in the seen_inode hash
         if ($ino= $self->{seen_inode}{$hardlink_key}) {
            $size= 0;
         } else {
            $ino= $self->{seen_inode}{$hardlink_key}= ++$self->{ino};
         }
         ($dev_major, $dev_minor)= (0,0);
      }
      else {
         $size= 0 if $self->{seen_inode}{$hardlink_key}++;
      }
   }
   elsif ($self->virtual_inodes) {
      ($dev_major, $dev_minor, $ino)= (0,0);
   }
   $ino //= ++$self->{ino};

   my $header= sprintf "070701%08X%08X%08X%08X%08X%08X%08X%08X%08X%08X%08X%08X%08X%s\0%s",
      $ino, $mode, $uid//0, $gid//0, $nlink//1, $mtime//0, $size,
      $dev_major//0, $dev_minor//0, $rdev_major//0, $rdev_minor//0,
      1+length $name, 0, $name,
      "\0"x((4 - ((13*8+6+length($name)+1) & 3)) & 3); # pad to multiple of 4
   die "BUG" if length $header & 3;

   $self->{fh}->print($header) || die "write: $!";
   $self->{fh}->print($fileinfo->{data}) || die "write: $!"
      if $size;
   $self->{fh}->print("\0"x(4-($size & 3))) || die "write: $!"
      if $size & 3; # pad to multiple of 4
}

1;
