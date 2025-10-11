package Sys::Export::GPT::Partition;

# VERSION
# ABSTRACT: Describes a partition entry for a GPT partition table

use v5.26;
use warnings;
use experimental qw( signatures );
use parent 'Sys::Export::Extent';

=head1 DESCRIPTION

GPT entries are composed of Type GUID, Unique GUID, Starting LBA, Ending LBA, Flags, and
partition Name.  This object inherits C<start_lba> and C<end_lba> from L<Sys::Export::Extent>
which are actually setting C<device_offset> and C<size>.  As a result, it is not possible to
set C<end_lba> before C<start_lba>.

=constructor new

  $partition= Sys::Export::GPT::Partition->new(%attrs);

=constructor coerce

  $partition= Sys::Export::GPT::Partition->new($x);

If C<$x> is a hashref, construct a new Partition object.  If C<$x> is already a partition
object, return it.

=attribute name

Unicode string label for the partition.

=attribute type

The GUID of the type of the partition.

=attribute guid

A GUID unique to this partition.

=attribute start_lba

See L<Sys::Export::Extent/start_lba>.

=attribute end_lba

See L<Sys::Export::Extent/end_lba>.  Note that you must set start_lba before end_lba.

=attribute flags

Bitwise-or of flags.

=cut

sub type($self, @v) { @v? ($self->{type}= $v[0]) : $self->{type} }
sub guid($self, @v) { @v? ($self->{guid}= $v[0]) : $self->{guid} }
sub flags($self, @v) { @v? ($self->{flags}= $v[0]) : $self->{flags} }

1;
