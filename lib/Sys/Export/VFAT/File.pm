package Sys::Export::VFAT::File;

# VERSION
# ABSTRACT: Represents a file in VFAT, including packed encodings of directories

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export::VFAT;
use Carp;
our @CARP_NOT= qw( Sys::Export::VFAT );

=head1 DESCRIPTION

Represents file (or directory) data to be encoded into the VFAT image.  This object functions
sort of like an 'inode', storing the attributes of the file, even though VFAT actually stores
the file attributes at the directory entry level.  This facilitates fun hacks like hard-linking
files in a VFAT filesystem even though VFAT doesn't permit that.

=constructor new

  $file= Sys::Export::VFAT::File->new(%attributes);

=cut

sub new($class, %attrs) {
   my $self= bless {}, $class;
   for (qw( name size data flags btime atime mtime align device_offset cluster )) {
      if (defined (my $v= delete $attrs{$_})) {
         $self->{$_}= $v
      }
   }
   croak "Unknown attribute: ".join(', ', keys %attrs) if keys %attrs;
   $self;
}

=attribute name

Unicode full path of file, for debugging

=attribute size

Size, in bytes

=attribute data

A reference to literal data of this file, which could be a scalar ref or
L<LazyFileData|Sys::Export::LazyFileData> object.

=attribute flags

Default directory listing flags

=attribute btime

Default creation ('born') unix epoch time

=attribute mtime

Default modification unix epoch time

=attribute atime

Default last-access unix epoch time

=attribute align

Request file be allocated on a power-of-two boundary from the start of the device.

=attribute device_offset

If initially set, request that file be placed at an absolute offset from start of the device.
If it can't be honored, encoding of the filesystem will fail.  After encoding, this will be
set to the location chosen for the file.

=attribute cluster

After encoding, this will be set to the cluster ID of the file.

=attribute is_dir

True if L</flags> indicate that this is a directory.

=cut

sub name      { $_[0]{name} }
sub size      { $_[0]{size} }
sub data      { $_[0]{data} }
sub flags     { $_[0]{flags} }
sub mtime     { $_[0]{mtime} }
sub atime     { $_[0]{atime} }
sub btime     { $_[0]{btime} }
sub align     { $_[0]{align} }
sub device_offset { $_[0]{device_offset} }
sub cluster   { $_[0]{cluster} }
sub is_dir    { $_[0]{flags} & Sys::Export::VFAT::ATTR_DIRECTORY() }

1;
