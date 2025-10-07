package Sys::Export::Extent;

# VERSION
# ABSTRACT: Represents a range of bytes and data that needs written there

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export qw( round_up_to_multiple :isa );
use Carp;

=head1 DESCRIPTION

This object is used as a base class for various types of data structure that are based around
writing data to a range of bytes within a device image.

Semantics:

=over

=item device_offset >= 0, size > 0, defined data

The data needs written to the range of bytes described.  It will be padded with NUL bytes if it
is shorter than the range, and truncated if it is longer than the range.

=item device_offset >= 0, size > 0, no data

The extent is a known range of the device image, and the data is either already there, or will
be written by the user at a later time.

=item undefined device_offset

We don't know where the extent will be yet, and the export modules should decide where to put
it automatically.

=item device_offset < 0

We don't know where the extent will be yet, and export modules should ignore it for now.
The user will update device_offset later.

=back

Device offsets are restricted to 'block_size' (512 by default).

Modifying size to a different multiple of 'block_size' after device_offset is set is an error.
Be sure to set C<size> before C<device_offset>.

=constructor new

  $file= Sys::Export::Extent->new(%attributes);

Represents file (or directory) data to be encoded into the ISO image.

=cut

sub new($class, %attrs) {
   my $self= bless { name => delete $attrs{name} }, $class;
   # Some fields need to be initialized in a specific order:
   $self->block_size(delete $attrs{block_size}) if defined $attrs{block_size};
   $self->size(delete $attrs{size}) if defined $attrs{size};
   $self->data(delete $attrs{data}) if defined $attrs{data};
   $self->device_offset(delete $attrs{device_offset}) if defined $attrs{device_offset};
   $self->start_lba(delete $attrs{start_lba}) if defined $attrs{start_lba};
   # The rest have no interdependencies
   for (keys %attrs) {
      my $m= $self->can($_) or croak "Unknown attribute '$_'";
      $m->($self, $attrs{$_});
   }
   $self;
}

=attribute name

Informative name of extent, for debugging.  Default name is 'extent'.

=attribute block_size

Power-of-2 restriction on device_offset, 512 by default.

=cut

sub name($self) { $self->{name} // 'extent' }

sub block_size($self, @v) {
   if (@v) {
      croak "Not a power of 2" unless isa_pow2 $v[0];
      $self->{block_size}= $v[0];
   }
   $self->{block_size} // 512;
}

=attribute size

Size, in bytes.  If nonzero, cannot be changed to a different multiple of block_size after
device_offset has been set to a non-negative value.

(this is useful to catch bugs related to resizing things after they'be been allocated to a
location in the image)

=cut

sub size($self, @v) {
   if (@v) {
      croak "Sector length of ".$self->name." changed after choosing LBA"
         if ($self->{size} // 0) > 0 && ($self->device_offset // -1) >= 0
            && round_up_to_multiple($v[0], $self->block_size) != round_up_to_multiple($self->{size}, $self->block_size);
      $self->{size}= $v[0];
   }
   $self->{size}
}

=attribute device_offset

Byte offset of this extent within the device image.  When written to a non-negative value, the
value must be a multiple of C<block_size>.  C<undef> may be used to request an Export module to
choose a location for this extent, and C<-1> may be used to prevent the Export module from doing
that yet.

=cut

sub device_offset($self, @v) {
   if (@v) {
      croak "Not a multiple of ".$self->block_size
         if ($v[0]//0) > 0 && ($v[0] & ($self->block_size-1));
      $self->{device_offset}= $v[0];
   }
   $self->{device_offset}
}

=attribute start_lba

Logical Block Address; used to read or write device_address by its LBA.
When reading, a negative C<device_offset> is converted to an undefined LBA.

=attribute lba

Alias for C<start_lba>

=attribute end_lba

Used to read or write C<size> relative to C<device_address>, in terms of C<block_size>.

=cut

sub start_lba($self, @v) {
   if (@v) {
      $self->device_offset($v[0] && ($v[0] * $self->block_size));
   }
   my $ofs= $self->device_offset;
   use integer;
   return defined $ofs && $ofs >= 0? ($ofs / $self->block_size) : undef;
}

*lba= *start_lba;

sub end_lba($self, @v) {
   if (@v) {
      my $lba= $self->start_lba;
      croak "Can't set end_lba until start_lba is defined"
         unless defined $lba;
      # Using 'size' accessor would trigger the check for altering the size after setting
      # device offset, which is less likely to be a useful safeguard when someone is setting
      # start_lba/end_lba.
      $self->{size}= ($v[0] + 1 - $lba) * $self->block_size;
   }
   my ($ofs, $size)= ($self->device_offset, $self->size);
   use integer;
   return ($ofs//-1) < 0 || !$size ? undef : ($ofs + $size - 1) / $self->block_size;
}

=attribute data

A reference to literal data of this file, which could be a scalar ref or
L<LazyFileData|Sys::Export::LazyFileData> object.

=cut

sub data($self, @v) {
   if (@v) {
      croak "Not a scalar-ref or LazyFileData object"
         if defined $v[0] && !isa_data_ref $v[0];
      $self->{data}= $v[0];
   }
   $self->{data}
}

1;
