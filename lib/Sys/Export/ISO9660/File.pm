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
      croak "device_offset must be a multiple of 2048" if $o & 0x7FF;
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

Size, in bytes.  After setting L</extent_lba> to a positive number, attempts to change this
to a value occupying a different number of sectors will croak.

=attribute extent_lba

LBA number (device_offset / 2048) where this file is located on the device.  A File having
C<< extent_lba >= 0 >> and undefined C<data> is assumed to represent data that the caller has
written or will write to this extent.  A file with C<< extent_lba <= 0 >> representa a file
whose extent will be decided later by the caller and should not be automatically positioned.

=attribute device_offset

Byte offset where this file is located on the device.  Always C<< extent_lba * 2048 >>.
If you write to this attribute it will croak unless a multiple of 2048.

=attribute data

A reference to literal data of this file, which could be a scalar ref or
L<LazyFileData|Sys::Export::LazyFileData> object.

=attribute mtime

Unix epoch time of file creation/modification, used as default for directory entries.
(every directory entry can override the mtime)

=attribute flags

Bit flags of file.  Constants come from C<< use Sys::Export::ISO9660 ':flags' >>.

=attribute is_dir

True if the flags include C<FLAG_DIRECTORY>

=cut

sub name($self)     { $self->{name} }
sub data($self, @v) { @v? ($self->{data}= $v[0]) : $self->{data} }
sub mtime($self, @v) { @v? ($self->{mtime}= $v[0]) : $self->{mtime} }
sub flags($self, @v) { @v? ($self->{flags}= $v[0]) : $self->{flags} }
sub is_dir($self) { ($self->{flags}||0) & Sys::Export::ISO9660::FLAG_DIRECTORY() }

sub extent_lba($self, @v) {
   @v? ($self->{extent_lba}= $v[0]) : $self->{extent_lba}
}

sub device_offset($self, @v) {
   if (@v) {
      croak "device_offset must be a multiple of 2048" if defined $v[0] && $v[0] & 2047;
      $self->{extent_lba}= $v[0] && ($v[0] >> 11);
   }
   $self->{extent_lba} && $self->{extent_lba} << 11
}

sub size($self, @v) {
   if (@v) {
      croak "Sector length of ".$self->name." changed after choosing LBA"
         if $self->extent_lba && $self->extent_lba > 0
            && (($v[0]+2047) >> 11) != ((($self->{size}||0)+2047) >> 11);
      $self->{size}= $v[0];
   }
   $self->{size}
}

1;
