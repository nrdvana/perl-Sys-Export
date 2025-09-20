package Sys::Export::VFAT::File;

# VERSION
# ABSTRACT: Represents a file in VFAT

use v5.26;
use warnings;
use experimental qw( signatures );
use Carp;
our @CARP_NOT= qw( Sys::Export::VFAT );

=constructor new

  $file= Sys::Export::VFAT::File->new(%attributes);

Represents file (or directory) data to be encoded into the VFAT image.
See L<Sys::Export::VFAT::Directory::Entry|::Directory::Entry> for the file attributes seen in a
directory listing.

=cut

sub new($class, %attrs) {
   my $self= bless {}, $class;
   for (qw( name size flags btime atime mtime cluster align offset data_ref data_path )) {
      if (defined (my $v= delete $attrs{$_})) {
         $self->{$_}= $v
      }
   }
   croak "Unknown attribute: ".join(', ', keys %attrs) if keys %attrs;
   $self;
}

=attribute name

Unicode full path to file, for debugging.

=attribute size

Size, in bytes

=attribute data_ref

A scalar-ref to literal data of this file

=attribute data_path

Path string to file's data

=attribute flags

Default directory listing flags (can be overridden on directory entry)

=attribute btime

Default creation ('born') unix epoch time (can be overridden on directory entry)

=attribute mtime

Default modification unix epoch time (can be overridden on directory entry)

=attribute atime

Default last-access unix epoch time (can be overridden on directory entry)

=attribute align

Request file be allocated on a power-of-two boundary from the start of the device.

=attribute offset

If initially set, request that file be placed at an absolute offset from start of the device.
If it can't be honored, encoding of the filesystem will fail.  After encoding, this will be
set to the location chosen for the file.

=attribute cluster

After encoding, this will be set to the cluster ID of the file.

=attribute is_dir

False, unless this object was created as the subclass L<Sys::Export::VFAT::Directory>.

=cut

sub name      { $_[0]{name} }
sub size      { $_[0]{size} }
sub data_ref  { $_[0]{data_ref} }
sub data_path { $_[0]{data_path} }
sub flags     { $_[0]{flags} }
sub mtime     { $_[0]{mtime} }
sub atime     { $_[0]{atime} }
sub btime     { $_[0]{btime} }
sub align     { $_[0]{align} }
sub offset    { $_[0]{offset} }
sub cluster   { $_[0]{cluster} }
sub is_dir    { 0 }

1;
