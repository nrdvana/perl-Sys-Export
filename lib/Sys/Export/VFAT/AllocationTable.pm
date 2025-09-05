package Sys::Export::VFAT::AllocationTable;

# ABSTRACT: Track which FAT clusters are used, and by what
# VERSION

=head1 SYNOPSIS

  $alloc= Sys::Export::VFAT::AllocationTable->new();
  $alloc->alloc_file(\%file_attrs);

=head1 DESCRIPTION

This module manages an allocation table for a FAT filesystem.  The allocation table is an array
with one element per disk cluster (aside from index 0 and 1 which do not have a cluster
allocated), which acts like a linked list.  The value of each element says whether the cluster
is used (>0), what cluster follows it, and whether it is the final cluster in the chain.
For FAT32, it also has bits to flag clusters with disk errors.

This module can also pair file/directory metadata with the starting cluster of each chain.

=cut

# Element 0 of the FAT is used for an inversion list of which sectors are allocated.
# It's not as good as a tree, but should perform well when the typical use case is
# to pack files end to end without fragmentation.
use v5.26;
use warnings;
use experimental qw( signatures );
use Scalar::Util 'refaddr';
use Carp;
use Sys::Export qw( :isa );
use Sys::Export::VFAT::Geometry qw( FAT12_MAX_CLUSTERS FAT16_MAX_CLUSTERS FAT32_MAX_CLUSTERS );

=attribute fat

Direct access to the allocation table array.  Don't modify it directly.

=attribute free

An inversion list describing free clusters.  Don't modify it directly.
The list is open-ended if max_cluster is set to C<undef>, which is useful for finding out how
many clusters you need.

=attribute chains

Hash keyed by starting-cluster ID which holds metadata about what is stored there.
The metadata may include C<invlist> which is an inversion-list representation of the chain,
or C<dir> which is an unpacked directory.

=attribute max_cluster_id

The maximum value for a cluster ID, which is also the maximum element of the L</fat> array.
This may be C<undef> to avoid exceptions while sizing up your data.

=attribute max_used_cluster_id

The maximum cluster number which was allocated so far.

=cut

sub fat            { $_[0]{fat} }
sub free           { $_[0]{free} }
sub chains         { $_[0]{chains} }
sub max_cluster_id { @_ > 1? ($_[0]{max_cluster_id}= $_[1]) : $_[0]{max_cluster_id} }
sub max_used_cluster_id($self) {
   my $free= $self->free;
   # if free list is odd length, it means open-ended free space, so return cluster before that.
   # otherwise, the free list was specified according to max_cluster_id
   # if even length and ends at max_cluster_id, max used is start of that range - 1
   @$free & 1? $free->[-1]-1
   : @$free && $free->[-1] == $self->max_cluster_id+1? $free->[-2]-1
   : $self->max_cluster_id
      // die "BUG: 'free' invlist cannot be empty if max_cluster_id is not set"
}
   
=method get_chain

  $metadata= $self->get_chain($cluster_id);

Returns the metadata associaed with the head cluster of a cluster chain.
Clusters which are not the heads of chains return nothing.

=cut

sub get_chain($self, $cl_id) {
   $self->{chains}{$cl_id};
}

=constructor new

  $at= Sys::Export::VFAT::AllocationTable->new(%attrs);

Currently the only attribute that can actually be set in the constructor is C<max_cluster_id>.

=cut

sub new($class, @attrs) {
   my %attrs= @attrs == 1 && ref $attrs[0] eq 'HASH'? %{$attrs[0]} : @attrs;
   my $max_cluster_id= delete $attrs{max_cluster_id};
   croak "Invalid max_cluster_id"
      unless !defined $max_cluster_id or isa_int $max_cluster_id && $max_cluster_id >= 2;
   carp "Unrecognized attributes ".join(', ', keys %attrs)
      if keys %attrs;

   my (@fat, @free);
   @free= (2); # first usable cluster is always 2
   # This object can be used open-ended for sizing up the table,
   # or with an end-cluster for the packing the final copy
   if ($max_cluster_id) {
      push @free, $max_cluster_id+1;
      $#fat= $max_cluster_id;
   }

   bless {
      max_cluster_id => $max_cluster_id,
      fat => \@fat,
      free => \@free,
      chains => {},
   }, $class;
}

=method alloc

  $cl_head= $at->alloc($cl_count);

Create a cluster chain of C<$cl_count> clusters from anywhere in the table and return the
cluster ID of the head of the chain.  C<$cl_count> must be an integer.
A request for 0 clusters returns cluster ID 0.

=cut

sub alloc($self, $count) {
   return 0 unless $count;
   croak "Cluster count must be an unsigned integer"
      unless isa_int $count && $count > 0;
   my $invlist= $self->{free};
   # If there are enough free sectors, this basically just chops some entries
   # off the free inversion list to become the allocated inversion list.
   for (my $i= 0; $i < @$invlist; $i+= 2) {
      my ($from, $upto)= @{$invlist}[$i,$i+1];
      my $n= defined $upto? ($upto - $from) : undef; # free list may be open-ended
      if (!defined $n || $n >= $count) {
         # Filled the request, so now modify the free list
         my @result;
         if (defined $n && $n == $count) {
            @result= splice(@$invlist, 0, $i+2);
         } else {
            @result= splice(@$invlist, 0, $i);
            $result[$i]= $from;
            $result[$i+1]= $from + $count;
            $invlist->[0]= $from + $count;
         }
         # and build the cluster chain in the FAT
         my $prev= 0;
         for (my $j= 0; $j < @result; $j += 2) {
            for ($result[$j] .. ($result[$j+1]-1)) {
               $self->{fat}[$prev]= $_ if $prev;
               $prev= $_;
            }
         }
         $self->{fat}[$prev]= 0x0FFFFFFF;
         $self->{chains}{$result[0]}{invlist}= \@result;
         return $result[0];
      }
      $count -= $n;
   }
   return undef; # not enough available
}

=method alloc_range

  $cl_head= $at->alloc_range($cl_id, $cl_count);

Create a cluster chain from a specific extent of clusters.  If any cluster was already allocated
this fails and returns false.  C<$cl_id> must be 2 or larger.
A request for 0 clusters returns cluster ID 0.

=cut

sub alloc_range($self, $cluster_id, $count) {
   return 0 unless $count;
   croak "Cluster count must be an unsigned integer"
      unless isa_int $count && $count > 0;
   croak "Invalid cluster id '$cluster_id'"
      unless isa_int $cluster_id && $cluster_id >= 2;
   my $cluster_lim= $cluster_id + $count;
   # Iterate the inversion list until the start $cluster_id, then ensure $count free
   my $invlist= $self->{free};
   for (my $i= 0; $i < @$invlist; $i += 2) {
      my ($from, $upto)= @{$invlist}[$i, $i+1];
      # Are we there yet?
      next if defined $upto && $upto < $cluster_id;
      # Range is occupied
      last if $from > $cluster_id || (defined $upto && $upto < $cluster_lim);
      return $self->_alloc_range($cluster_id, $cluster_lim, $i);
   }
   return undef;
}

=method alloc_contiguous

  $cl_head= $at->alloc_contiguous($cl_count, $align=1, $align_ofs=0);

Allocate the first available contiguous span of C<$cl_count> clusters, optionally restricted to
the cluster ID being a multiple of a power-of-two alignment, when offset by C<$align_ofs>.

See L<Sys::Export::VFAT::Geometry/get_cluster_alignment_of_device_alignment>.

=cut

sub alloc_contiguous($self, $count, $align=1, $align_ofs=0) {
   return 0 unless $count;
   croak "Cluster count must be an unsigned integer"
      unless isa_int $count && $count > 0;
   my $invlist= $self->{free};
   for (my $i= 0; $i < @$invlist; $i+=2) {
      my ($from, $upto)= @{$invlist}[$i, $i+1];
      my $start= $from;
      # Align start addr
      if ($align > 1) {
         my $remainder= ($start - $align_ofs) & ($align-1);
         $start += $align - $remainder if $remainder;
      }
      # Is the range large enough?
      next if defined $upto && $upto - $start < $count;
      return $self->_alloc_range($start, $start+$count, $i);
   }
   return undef;
}

sub _alloc_range($self, $cl_start, $cl_lim, $invlist_idx) { 
   # remove this range from the 'free' invlist
   my $from_edge= $self->{free}[$invlist_idx] == $cl_start;
   my $to_edge=  ($self->{free}[$invlist_idx+1]//0) == $cl_lim;
   if ($from_edge && $to_edge) {
      splice(@{$self->{free}}, $invlist_idx, 2);
   } elsif ($from_edge) {
      $self->{free}[$invlist_idx]= $cl_lim;
   } elsif ($to_edge) {
      $self->{free}[$invlist_idx+1]= $cl_start;
   } else {
      splice(@{$self->{free}}, $invlist_idx+1, 0, $cl_start, $cl_lim);
   }
   # Build the cluster chain in the FAT
   $self->{fat}[$_]= $_+1
      for $cl_start .. $cl_lim-2;
   $self->{fat}[$cl_lim-1]= 0x0FFFFFFF;
   # An allocation inversion list of one segment
   $self->{chains}{$cl_start}{invlist}= [ $cl_start, $cl_lim ];
   return $cl_start;
}

=method pack

  $buf= $at->pack

Pack the allocation table into bytes.  This selects FAT12/FAT16/FAT32 by the total number of
clusters.  Make sure you set L</max_cluster_id> correctly before calling this.

=cut

sub pack($self, $bits=undef) {
   my $fat= $self->fat;
   my $max= $self->max_cluster_id // $self->max_used_cluster_id;
   my $cl_count= $max-1; # excluding clusters 0 and 1
   croak "Max cluster ID exceeds FAT32 max" if $cl_count > FAT32_MAX_CLUSTERS;
   carp "Truncating table to cluster id $max" if $max < $#$fat;
   $#$fat= $max;
   $fat->[$_]= 0x0FFFFFFF for 0,1;
   $fat->[$_] //= 0 for 2..$max;   # prevent warnings in pack
   if ($cl_count > FAT16_MAX_CLUSTERS) {
      return pack 'V*', @$fat;
   } elsif ($cl_count > FAT12_MAX_CLUSTERS) {
      return pack 'v*', @$fat;
   } else {
      # 12 bits per entry, pack in groups of 3 bytes, little-endian
      my $buf= "\xFF\xFF\xFF";
      for (my $i= 2; $i+1 <= $max; $i+= 2) {
         my $v= ($fat->[$i] & 0xFFF) | ( ($fat->[$i+1] & 0xFFF) << 12 );
         $buf .= pack 'vC', $v, ($v >> 16);
      }
      $buf .= pack 'v', $fat->[$max] & 0xFFF unless $max & 1;
      return $buf;
   }
}

1;
