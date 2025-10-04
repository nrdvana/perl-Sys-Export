package Sys::Export::ISO9660::File;

# VERSION
# ABSTRACT: Represents a file in ISO9660, including packed encodings of directories

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export::VFAT;
use Carp;
our @CARP_NOT= qw( Sys::Export::ISO9660 );

=constructor new

  $file= Sys::Export::ISO9660::File->new(%attributes);

Represents file (or directory) data to be encoded into the ISO image.

=cut

sub new($class, %attrs) {
   my $self= bless {}, $class;
   if (defined (my $o= delete $attrs{device_offset})) {
      croak "offset must be a multiple of 2048" if $o & 0x7FF;
      $attrs{extent_lba}= $o >> 11;
   }
   for (qw( name size data mtime flags extent_lba )) {
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

=attribute data

A reference to literal data of this file, which could be a scalar ref or
L<LazyFileData|Sys::Export::LazyFileData> object.

=attribute mtime

Unix epoch time of file creation/modification

=attribute flags

Bit flags of file.  Constants come from C<< use Sys::Export::ISO9660 ':flags' >>.

=attribute extent_lba

LBA number (device_offset / 2048) where this file is located on the device.  If this is
initially set and the L</data> is undefined, it is assumed the file data already exists at this
location.

=attribute device_offset

Byte offset where this file is located on the device.  Always C<< extent_lba * 2048 >>.

=attribute is_dir

True if the flags include FLAG_DIRECTORY

=cut

sub name          { $_[0]{name} }
sub size          { @_ > 1? ($_[0]{size}= $_[1]) : $_[0]{size} }
sub data          { @_ > 1? ($_[0]{data}= $_[1]) : $_[0]{data} }
sub mtime         { @_ > 1? ($_[0]{mtime}= $_[1]) : $_[0]{mtime} }
sub flags         { @_ > 1? ($_[0]{flags}= $_[1]) : $_[0]{flags} }
sub extent_lba    { @_ > 1? ($_[0]{extent_lba}= $_[1]) : $_[0]{extent_lba} }
sub device_offset { $_[0]{extent_lba} && $_[0]{extent_lba} << 11 }
sub is_dir        { ($_[0]{flags}||0) & Sys::Export::ISO9660::FLAG_DIRECTORY() }

1;
