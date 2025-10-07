package Sys::Export::ISO9660::File;

# VERSION
# ABSTRACT: Represents a file in ISO9660, including packed encodings of directories

use v5.26;
use warnings;
use experimental qw( signatures );
use parent 'Sys::Export::Extent';
use Sys::Export::ISO9660;
use Carp;
our @CARP_NOT= qw( Sys::Export::ISO9660 );

=constructor new

  $file= Sys::Export::ISO9660::File->new(%attributes);

Represents file (or directory) data to be encoded into the ISO image.

=attribute name

Unicode full path to file, for debugging.

=attribute block_size

Always 2048.

=attribute size

Size, in bytes.  See L<Sys::Export::Extent/size>.

=attribute device_offset

Byte offset from start of image.  See L<Sys::Export::Extent/device_offset>.

=attribute block_address

LBA number (device_offset / 2048) where this file is located on the device.
Reading this attribute returns C<undef> if C<device_offset> is undefined or negative.

=attribute data

Data to be written to extent.  See L<Sys::Export::Extent/data>.

=attribute mtime

Unix epoch time of file creation/modification, used as default for directory entries.
(every directory entry can override the mtime)

=attribute flags

Bit flags of file.  Constants come from C<< use Sys::Export::ISO9660 ':flags' >>.

=attribute is_dir

True if the flags include C<FLAG_DIRECTORY>

=cut

sub alignment($self) { 2048 }
sub mtime($self, @v) { @v? ($self->{mtime}= $v[0]) : $self->{mtime} }
sub flags($self, @v) { @v? ($self->{flags}= $v[0]) : $self->{flags} }
sub is_dir($self) { ($self->{flags}||0) & Sys::Export::ISO9660::FLAG_DIRECTORY() }

1;
