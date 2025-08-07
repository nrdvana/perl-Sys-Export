package Sys::Export::SquashFS;

# ABSTRACT: Write squashfs archives from a virtual list of files
# VERSION

=head1 SYNOPSIS

  my $sqsh= Sys::Export::SquashFS->new(filename => $filename, %opts);
  $sqsh->add(\%stat_name_and_data);
  $sqsh->add(\%stat_name_and_data);
  $sqsh->add(\%stat_name_and_data);
  ...
  $sqsh->commit;

=head1 DESCRIPTION

Creating a squashfs archive typically requires giving the mksquashfs tool direct access to a
tree of prepared files.  This module allows you to pass a virtual list of files, instead.

Currently this is implemented by telling C<mksquashfs> to compress an empty directory, and then
giving it a "pseudo file" listing.  A downside to this approach is that it cannot handle hard
links.  Hard links will get converted to symlinks, automatically.  Another downside is that
it doesn't begin compressing until you call C<commit>, since the mksquashfs tool doesn't begin
compressing until it reads the full pseudo file listing.

Some day I might write a version that can build the squashfs directly, in which case it would
store hardlinks correctly.

=head1 CONSTRUCTOR

=head2 new

  my $sqsh= Sys::Export::SquashFS->new(%opts);

=over

=item filename

The name of the squashfs file to be created.  If it already exists, this will crash unless you
request C<< append => 1 >>.   (The squashfs tool supports appending to an existing file.)

=item append

Boolean, whether to allow appending to a pre-existing C<filename>.

=back

=cut

use v5.26;
use warnings;
use experimental qw( signatures );

sub new($class, %opts) {
   ...
   $self;
}

=head1 ATTRIBUTES

=head1 METHODS

=head2 add

  $sqsh->add({
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

sub append($self, $fileinfo) {
   if ($type eq 'f') {
      $self->_pseudofiles->print("$filename f $perms $uid $gid perl -e 'print pack q{H*}, shift' $hex\n");
   }
}

1;
