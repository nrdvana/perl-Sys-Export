package Sys::Export::VFAT::Geometry;

# ABSTRACT: Calculate addresses and sizes of structures within a FAT filesystem
# VERSION

=head1 SYNOPSIS

From an existing filesystem:

  open my $fh, '<', '/dev/sda1';
  local $/= 4096;
  my $boot_sector= <$fh>;
  my $geom= Sys::Export::VFAT::Geometry->unpack($boot_sector);

Calculate a filesystem sized to hold N clusters of data:

  my $geom= Sys::Export::VFAT::Geometry->new(
    bytes_per_Sector => 512,
    sectors_per_cluster => 8,
    fat_count => 1,
    align_clusters => 4096,
    cluster_count => 12345,
  );

=head1 DESCRIPTION

The goal of Sys::Export::VFAT is to be able to create filesystems where you have dictated the
location or alignment of specific files within the filesystem.  Since it is fairly difficult
to calculate this, it helps to be able to iterate through different filesystem size parameters
until one is found that meets your specification.  This module allows you to construct a
theoretical filesystem based on your parameters, and then query how the clusters line up with
the absolute byte offsets you care about.

This module can also pack and unpack these parameters from a boot sector.

Note that all the defaults in this module aim for B<minimum-sized read-only FAT filesystems>,
not the sensible defaults that would provide free space to add new files at runtime.

The math within is derived from the official published formulas in

  Microsoft Extensible Firmware Initiative
  FAT32 File System Specification
  FAT: General Overview of On-Disk Format
  Version 1.03, December 6, 2000
  Microsoft Corporation

=cut

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export qw( :isa round_up_to_pow2 round_up_to_multiple );
use Sys::Export::VFAT qw( FAT12 FAT16 FAT32 );
use POSIX 'ceil';
use Carp;
our @CARP_NOT= qw( Sys::Export );
use constant {
   FAT12_MAX_CLUSTERS       => 4085-1,
   FAT12_IDEAL_MAX_CLUSTERS => 4085-16,
   FAT16_MIN_CLUSTERS       => 4085,
   FAT16_IDEAL_MIN_CLUSTERS => 4085+16,
   FAT16_MAX_CLUSTERS       => 65525-1,
   FAT16_IDEAL_MAX_CLUSTERS => 65525-16,
   FAT32_MIN_CLUSTERS       => 65525,
   FAT32_IDEAL_MIN_CLUSTERS => 65525+16,
   FAT32_MAX_CLUSTERS       => (1<<28)-3,
};

=constructor new

  $geom= Sys::Export::VFAT::Geometry->new(%options);

There are two officially supported sets of options.  The first is when you know the geometry
parameters from the boot sector of an existing filesystem:

=over

=item bytes_per_sector

=item sectors_per_cluster

=item reserved_sector_count

=item fat_count

=item fat_sector_count

=item root_dirent_count

=item total_sector_count

=back

The second is when you want to to choose parameters for a new filesystem to hold a known number
of clusters:

=over

=item cluster_count

The desired count of usable data clusters for file and directory data.  This may be rounded
upward slightly if it is near a FAT16/FAT32 cutoff.

=item exact_cluster_count

Set this to a true value if you want to prevent rounding upward to a larger bit filesystem
for a cluster_count near the boundary.

=back

Without any further options, you will get 512 byte sectors, 4K clusters, 2 allocation tables,
512 root entries (for FAT12/16), and clusters aligned to 4K.  You may also specify any of the
following options to override those defaults:

=over

=item device_offset

If you know that this logical volume starts at a nonzero device address, specify this to get
alignments from the start of the device rather than alignments from the start of the volume.

=item align_clusters

Request power-of-2 alignment of device addresses of clusters.  This number is in bytes.
If the number is larger than the size of a cluster, it ensures that at least every Nth cluster
is aligned.  If the number is equal or smaller than the size of a cluster, it ensures that
every cluster has that alignment.

For instance, if you set this to 4096, and the C<device_offset> is C<512*3> (which would
normally be a poor choice for partition alignment), and the cluster size is also 4096, then
every cluster will have a device address with 0 in the low 12 bits, while having a volume offset
ending with C<512*5> in the low 12 bits.

=item min_bits

One of 12, 16, or 32.  Note that setting this forces a minimum cluster_count, because the
selection of FAT bits is based on number of clusters.  For instance, FAT32 cannot have fewer
than 65525 clusters, which is at least 32MB.

=item bytes_per_sector

The default is 512.

=item sectors_per_cluster

The default is C<4096 / bytes_per_sector>.

=item fat_count

Only one allocation table is required - the rest are backups in case a sector of the first one
is unreadable.  The default is 2, which is more common/compatible (but wastes space if you don't
need to worry about media errors)

=item reserved_sector_count

This allocates extra sectors at the start of the volume.  It must be at least 1 for the boot
sector, and higher on FAT32 for the additional free list and boot sector backup copy.

=item used_root_dirent_count

If you happen to know the exact number of directory entries of your root directory (including
any long filename entries) you can set this to get automatic minimal sizing of the root
directory.

=back

=cut

sub new($class, @attrs) {
   my %attrs= @attrs == 1 && isa_hash $attrs[0]? %{$attrs[0]} : @attrs;
   my ($bytes_per_sector, $sectors_per_cluster,   $fat_count,     $reserved_sector_count,
       $fat_sector_count, $root_dirent_count,     $cluster_count, $total_sector_count,
       $min_bits,         $device_offset,         $align_clusters
      ) = delete @attrs{qw(
        bytes_per_sector   sectors_per_cluster     fat_count       reserved_sector_count
        fat_sector_count   root_dirent_count       cluster_count   total_sector_count
        min_bits           device_offset           align_clusters
      )};
   !defined $align_clusters or isa_pow2($align_clusters)
      or croak "align_clusters must be a power of 2 (was $align_clusters)";
   $device_offset //= 0;
   isa_int($device_offset) && $device_offset >= 0
      or croak "device_offset must be a non-negative integer";

   $bytes_per_sector //= 512;
   isa_pow2($bytes_per_sector) && 512 <= $bytes_per_sector && $bytes_per_sector <= 4096
      or croak "Invalid bytes_per_sector $bytes_per_sector";

   # Default sectors_per_cluster to whatever makes 4K
   $sectors_per_cluster //= ($bytes_per_sector >= 4096? 1 : 4096 / $bytes_per_sector);
   isa_pow2($sectors_per_cluster) && $sectors_per_cluster <= 128 
      or croak "Invalid sectors_per_cluster $sectors_per_cluster";
   my $cluster_size= $bytes_per_sector * $sectors_per_cluster;
   $cluster_size <= 32*1024
      or carp "Warning: bytes_per_sector * sectors_per_cluster > 32KiB which is not valid for some drivers";

   # Default fat_count to 2 unless specified otherwise
   $fat_count //= 2;
   isa_int $fat_count && 0 < $fat_count && $fat_count <= 255
      or croak "Invalid fat_count $fat_count";

   my $self= bless {
      bytes_per_sector      => $bytes_per_sector,
      sectors_per_cluster   => $sectors_per_cluster,
      fat_count             => $fat_count,
      device_offset         => $device_offset,
   };
   
   # From here down, we are either determining cluster_count from other properties,
   # or deriving other properties from cluster_count.
   my $bits;
   if (defined $reserved_sector_count && defined $fat_sector_count
    && defined $root_dirent_count && defined $total_sector_count
   ) {
      # All main properties of the geometry are defined.
      my $root_sector_count= int(($root_dirent_count + ($self->dirent_per_sector-1)) / $self->dirent_per_sector);
      my $data_sectors= $total_sector_count - $reserved_sector_count - $fat_count * $fat_sector_count - $root_sector_count;
      my $calc_cluster_count= int($data_sectors / $sectors_per_cluster);
      croak "Supplied cluster_count disagrees with computed value"
         if defined $cluster_count && $cluster_count != $calc_cluster_count;
      $cluster_count //= $calc_cluster_count;
      $bits= $cluster_count < FAT16_MIN_CLUSTERS? FAT12
           : $cluster_count < FAT32_MIN_CLUSTERS? FAT16
           : FAT32;
   }
   elsif (defined $cluster_count) {
      isa_int $cluster_count && $cluster_count > 0
         or croak "Invalid cluster_count '$cluster_count'";
      # FAT docs recommend avoiding numbers near the boundary of FAT12/FAT16/FAT32 to avoid
      # other people's math errors.  But, allow the caller to disable this adjustment.
      unless (delete $attrs{exact_cluster_count}) {
         if ($cluster_count >= FAT12_IDEAL_MAX_CLUSTERS && $cluster_count < FAT16_IDEAL_MIN_CLUSTERS) {
            $cluster_count= FAT16_IDEAL_MIN_CLUSTERS;
         } elsif ($cluster_count >= FAT16_IDEAL_MAX_CLUSTERS && $cluster_count < FAT32_IDEAL_MIN_CLUSTERS) {
            $cluster_count= FAT32_IDEAL_MIN_CLUSTERS;
         }
      }
      # These are the official boundary numbers that determine the filesystem type
      $min_bits //= FAT12;
      $bits= $cluster_count < FAT16_MIN_CLUSTERS? FAT12
              : $cluster_count < FAT32_MIN_CLUSTERS? FAT16
              : FAT32;
      if ($bits < $min_bits) {
         $bits= $min_bits;
         # Increase to the minimum number of clusters if a specific number of bits
         # was requested.
         $cluster_count= ($bits == FAT16)? FAT16_IDEAL_MIN_CLUSTERS : FAT32_IDEAL_MIN_CLUSTERS;
      }
   }
   else {
      croak "Not enough attributes supplied to determine geometry";
   }
   $self->{cluster_count}= $cluster_count;
   $self->{bits}= $bits;

   # Check how many sectors are occupied by each allocation table
   my $fat_byte_count= ( ($cluster_count + 2) * $bits + 7 ) >> 3; # round up to bytes
   if (defined $fat_sector_count) {
      $fat_sector_count * $bytes_per_sector >= $fat_byte_count
         or croak "Invalid fat_sector_count, smaller than $fat_byte_count bytes";
   } else {
      $fat_sector_count= int(($fat_byte_count + ($bytes_per_sector - 1)) / $bytes_per_sector);
   }
   $self->{fat_sector_count}= $fat_sector_count;

   # Check how many sectors are occupied by root directory entries
   # For fat12/16, The FAT spec document suggests 512 as a good default
   # Allow the user to supply the actual number of root entries and then we round that.
   my $used_root_dirent_count= delete $attrs{used_root_dirent_count};
   if ($bits < FAT32) {
      if (defined $root_dirent_count) {
         $root_dirent_count >= 1 && $root_dirent_count < 0xFFFF
            or croak "Invalid root_dirent_count for FAT12/16";
      } else {
         $root_dirent_count= $used_root_dirent_count // 512;
         # Round up to as many as fit in this number of sectors
         my $remainder= ($root_dirent_count & ($self->dirent_per_sector - 1));
         $root_dirent_count += ($self->dirent_per_sector - $remainder)
            if $remainder;
      }
      
      ($reserved_sector_count //= 1) == 1
         or croak "reserved_sector_count should be 1 for FAT12/16";
   } else {
      ($root_dirent_count //= 0) == 0
         or croak "root_dirent_count must be zero for FAT32";

      $reserved_sector_count //= 32;
      isa_int $reserved_sector_count && $reserved_sector_count >= 2
         or croak "reserved_sector_count must be greater than 2 for FAT32";
   }

   # If caller requested alignment of clusters, figure that out
   if (defined $align_clusters && $align_clusters > $bytes_per_sector) {
      # there's a method for this, but avoid caching things yet
      my $data_addr= $device_offset + $bytes_per_sector * (
         $reserved_sector_count
         + ($fat_count*$fat_sector_count)
         + ceil($root_dirent_count / $self->dirent_per_sector)
      );
      # If the cluster size is greater or equal to the requested alignment, ensure the
      # data start falls on that boundary.
      # If the cluster size is smaller than the requested alignment, ensure the data start
      # falls on a cluster boundary so that some number of clusters will equal the alignment.
      my $align= ($cluster_size >= $align_clusters)? $align_clusters : $cluster_size;
      if (my $ofs= $data_addr & ($align-1)) {
         my $shift_n_sectors= ($align - $ofs) / $bytes_per_sector;
         #say sprintf "# remainder=0x%X, cluster_size=$cluster_size align_clusters=$align_clusters $align-$ofs=".($align-$ofs)." shift %d sectors", $ofs, $shift_n_sectors;
         if ($bits < FAT32) {
            # Reserved sectors should be 1, so expand number of root entries
            $root_dirent_count += $shift_n_sectors * $self->dirent_per_sector;
         } else {
            # Add however many reserved sectors we need
            $reserved_sector_count += $shift_n_sectors;
         }
      }
   }
   $self->{root_dirent_count}= $root_dirent_count;
   $self->{reserved_sector_count}= $reserved_sector_count;
   
   carp "Unused constructor parameters: ".join(' ', keys %attrs)
      if keys %attrs;
   $self;
}

=attribute device_offset

The device address at which this volume begins.  You must supply this to get meaningful values
from the various C<*_device_*> methods.

=attribute bytes_per_sector

Number of bytes in one disk sector, must be a power of 2 between 512 and 4096.

=attribute sectors_per_cluster

Number of sectors that make one cluster.  Must be a power of 2 between 1 and 128.

=attribute bytes_per_cluster

C<< bytes_per_sector * sectors_per_cluster >>

=attribute dirent_per_sector

Number of 32-byte directory entries in one sector.

=attribute dirent_per_cluster

Number of 32-byte directory entries in one cluster.

=cut

sub device_offset { $_[0]{device_offset} }

sub bytes_per_sector      { $_[0]{bytes_per_sector} }
sub sectors_per_cluster   { $_[0]{sectors_per_cluster} }
sub bytes_per_cluster     { $_[0]{bytes_per_sector} * $_[0]{sectors_per_cluster} }
sub dirent_per_sector     { $_[0]{bytes_per_sector} / 32 }
sub dirent_per_cluster    { $_[0]{bytes_per_sector} * $_[0]{sectors_per_cluster} / 32 }

=attribute bits

FAT12, FAT16, or FAT32, derived from number of clusters

=attribute reserved_sector_count

Number of sectors at start of volume before FAT tables begin

=attribute fat_count

Number of allocation tables (clones for redundancy)

=attribute fat_sector_count

Number of sectors required to hold each FAT (based on number of clusters and C<bits>).

=attribute cluster_count

Total number of clusters available for storage (starting from cluster id 2)

=attribute min_cluster_id

Lowest cluster id which can store data.  Always 2.

=attribute max_cluster_id

Highest cluster id which can store data.  C<< cluster_count + 1 >>.

=attribute root_sector_count

Total number of sectors required to hold the root directory entries.  0 for FAT32, which stores
the root dir in the clusters with everything else.

=attribute data_start_sector

Sector offset within the volume where the data clusters begin.

=attribute data_start_offset

The byte offset of the start of the data area.  C<< data_start_sector * bytes_per_sector >>.

=attribute data_start_device_offset

The start of the data area as an absolute device address.

=attribute data_sector_count

Total number of sectors available for data.

=attribute total_sector_count

Total number of sectors in the volume.

=attribute total_size

Returns the total size in bytes of the volume.  C<< total_sector_count * bytes_per_sector >>.

=cut

sub bits                  { $_[0]{bits} }
sub reserved_sector_count { $_[0]{reserved_sector_count} }
sub fat_count             { $_[0]{fat_count} }
sub fat_sector_count      { $_[0]{fat_sector_count} }
sub cluster_count         { $_[0]{cluster_count} }
sub min_cluster_id        { 2 }
sub max_cluster_id        { $_[0]->cluster_count + 1 }
sub root_dirent_count     { $_[0]{root_dirent_count} }
sub root_sector_count     { ceil($_[0]->root_dirent_count / $_[0]->dirent_per_sector) }

sub data_start_sector($self) {
   $self->{data_start_sector} //= $self->reserved_sector_count
      + $self->fat_count * $self->fat_sector_count
      + $self->root_sector_count;
}
sub data_start_offset($self) { $self->data_start_sector * $self->bytes_per_sector }
sub data_start_device_offset($self) { $self->data_start_offset + $self->device_offset }

sub data_sector_count($self) {
   $self->total_sector_count - $self->data_start_sector;
}

sub total_sector_count($self) {
   $self->{total_sector_count} //= $self->data_start_sector
      + $self->cluster_count * $self->sectors_per_cluster;
}
sub total_size($self) { $self->total_sector_count * $self->bytes_per_sector }

=method get_cluster_start_sector

  $sector= $geom->get_cluster_start_sector($cluster_id);

Return the sector (from start of volume) where the cluster begins.
Croaks if you pass an invalid cluster ID.

=method get_cluster_offset

Same as C<get_cluster_start_sector> but as bytes from start of volume.

=method get_cluster_device_offset

Same as C<get_cluster_start_sector> but as an absolute device address based on
L</device_offset>.

=method get_cluster_of_sector

  $cl= $geom->get_cluster_of_sector($sector_idx);

For a sector index (from the start of the volume), return which cluster, if any, contains that
sector.  Returns undef if it doesn't fall within a cluster.

=method get_cluster_of_offset

Same as C<get_cluster_of_sector> but from a volume byte offset.

=method get_cluster_of_device_offset

Same as C<get_cluster_of_sector> but from an absolute device address based on
L</device_offset>.

=cut

sub get_cluster_start_sector($self, $cluster_id) {
   croak "Cluster 0 and 1 are reserved" if $cluster_id < 2;
   croak "Cluster $cluster_id beyond end of volume" if $cluster_id > $self->max_cluster_id;
   return $self->data_start_sector + ($cluster_id-2) * $self->sectors_per_cluster;
}
sub get_cluster_offset($self, $cluster_id) {
   $self->get_cluster_start_sector($cluster_id) * $self->bytes_per_sector;
}
sub get_cluster_device_offset($self, $cluster_id) {
   $self->get_cluster_start_sector($cluster_id) * $self->bytes_per_sector + $self->device_offset;
}

sub get_cluster_of_sector($self, $sector_idx) {
   return undef if $sector_idx < $self->data_start_sector;
   my $cluster= int(($sector_idx - $self->data_start_sector) / $self->sectors_per_cluster);
   return undef if $cluster >= $self->cluster_count;
   return $cluster + 2;
}
sub get_cluster_of_offset($self, $offset) {
   $self->get_cluster_of_sector(int($offset / $self->bytes_per_sector));
}
sub get_cluster_of_device_offset($self, $addr) {
   $self->get_cluster_of_offset($addr - $self->device_offset);
}

=method get_cluster_extent_of_volume_extent

  ($cl_start, $cl_count)= $geom->get_cluster_extent_of_volume_extent($vol_offset, $size);

The caller supplies a range of bytes as an absolute offset from the start of the volume and
a size in bytes.  The offset must also match the start of a cluster, or this dies.
Size does not need to end at a cluster boundary.
This returns the starting cluster number count of clusters occupied (rounding up).
It dies if this range overflows the available clusters.  

=cut

sub get_cluster_extent_of_volume_extent($self, $offset, $size) {
   my $cl_start= $self->get_cluster_of_offset($offset);
   $self->get_cluster_offset($cl_start) == $offset
      or croak "FAT_offset not aligned to a cluster boundary";
   my $cl_cnt= ceil($size / $self->bytes_per_cluster);
   $cl_start + $cl_cnt <= $self->max_cluster_id+1
      or croak "byte range ($offset, $size) exceeds final cluster of volume";
   return ($cl_start, $cl_cnt);
}

=method get_cluster_extent_of_device_extent

Like C<get_cluster_extent_of_volume_extent> but specifies the byte range in terms of the device,
factoring in L</device_offset>.

=cut

sub get_cluster_extent_of_device_extent($self, $addr, $size) {
   $self->get_cluster_extent_of_volume_extent($addr - $self->device_offset, $size);
}

=method get_cluster_alignment_of_device_alignment

  ($mult, $offset)= $geom->get_cluster_alignment_of_device_alignment($align);

The caller supplies a power-of-2 device alignment (such as 4096), and this returns the
multiplier and offset for cluster numbers that will land on that device alignment.

This dies if the specified alignment dosn't fall on any cluster boundary.

=cut

sub get_cluster_alignment_of_device_alignment($self, $align) {
   my $cluster_size= $self->bytes_per_cluster;
   # If the cluster size is greater or equal to the requested alignment, verify that the
   # data start (plus the volume offset that was implicitly added to every volume address)
   # meets that alignment.  If so, every cluster is aligned and the return value is (1,0)
   if ($cluster_size >= $align) {
      croak "Clusters are not aligned to $align"
         if $self->data_start_device_offset & ($align-1);
      return (1,0);
   }
   # Otherwise, make sure the data_start (plus implied volume offset) is aligned to
   # cluster_size, so then some multiple of clusters will reach the requested alignment.
   croak "Clusters are not aligned to $cluster_size"
      if $self->data_start_device_offset & ($cluster_size-1);
   # The cluster alignment will be whatever multiple of clusters equals the byte alignment.
   # This will be at least 2.
   my $cl_align= $align / $cluster_size;
   # How many bytes away from alignment is the beginning of ficticious cluster 0?
   my $ofs_of_cl0= ($self->data_start_device_offset - ($cluster_size*2)) & ($align-1);
   # If not aligned, add the rest of the distance to the next alignment.
   my $cl_ofs= !$ofs_of_cl0? 0 : ($align - $ofs_of_cl0) / $cluster_size;
   return ($cl_align, $cl_ofs);
}

=method pack

   $buf_ref= $geom->pack(%attrs);
   $geom->pack(into => $buf_ref, %attrs);

Pack this geometry into FAT boot sectors.  One sector will always be written, but depending on
the supplied attributes it can also write the rest of the FAT32 structures in the reserved
area of the volume.

=cut

our @sector0_fields_common= (
   [ BS_jmpBoot     =>    0,  3, 'a3', '' ],
   [ BS_OEMName     =>    3,  8, 'a8', 'MSWIN4.1' ],
   [ BPB_BytsPerSec =>   11,  2, 'v' ],
   [ BPB_SecPerClus =>   13,  1, 'C' ],
   [ BPB_RsvdSecCnt =>   14,  2, 'v' ],
   [ BPB_NumFATs    =>   16,  1, 'C' ],
   [ BPB_RootEntCnt =>   17,  2, 'v' ],
   [ BPB_TotSec16   =>   19,  2, 'v' ],
   [ BPB_Media      =>   21,  1, 'C', 0xF8 ],
   [ BPB_FATSz16    =>   22,  2, 'v' ],
   [ BPB_SecPerTrk  =>   24,  2, 'v', 0 ],
   [ BPB_NumHeads   =>   26,  2, 'v', 0 ],
   [ BPB_HiddSec    =>   28,  4, 'V', 0 ],
   [ BPB_TotSec32   =>   32,  4, 'V'  ]
);
our @sector0_fat16_fields= (
   @sector0_fields_common,
   [ BS_DrvNum      =>   36,  1, 'C', 0x80 ],
   [ BS_Reserved1   =>   37,  1, 'C', 0 ],
   [ BS_BootSig     =>   38,  1, 'C', 0x29 ],
   [ BS_VolID       =>   39,  4, 'V' ],
   [ BS_VolLab      =>   43, 11, 'A11', 'NO NAME' ],
   [ BS_FilSysType  =>   54,  8, 'A8' ],
   [ _signature     =>  510,  2, 'v', 0xAA55 ],
);
our @sector0_fat32_fields= (
   @sector0_fields_common,
   [ BPB_FATSz32    =>   36,  4, 'V' ],
   [ BPB_ExtFlags   =>   40,  2, 'v', 0 ],
   [ BPB_FSVer      =>   42,  2, 'v', 0 ],
   [ BPB_RootClus   =>   44,  4, 'V' ],
   [ BPB_FSInfo     =>   48,  2, 'v' ],
   [ BPB_BkBootSec  =>   50,  2, 'v', 0 ],
   [ BPB_Reserved   =>   52, 12, 'a12', 0 ],
   [ BS_DrvNum      =>   64,  1, 'C', 0x80 ],
   [ BS_Reserved1   =>   65,  1, 'C', 0 ],
   [ BS_BootSig     =>   66,  1, 'C', 0x29 ],
   [ BS_VolID       =>   67,  4, 'V' ],
   [ BS_VolLab      =>   71, 11, 'A11', 'NO NAME' ],
   [ BS_FilSysType  =>   82,  8, 'A8' ],
   [ _signature     =>  510,  2, 'v', 0xAA55 ],
);

sub pack {
   my $self= shift;
   my %attrs= @_ == 1 && isa_hash $_[0]? %{$_[0]} : @_;
   my $buf_ref= delete $attrs{buf_ref} // \(my $buf= '');
   $attrs{BPB_BytsPerSec}= $self->bytes_per_sector;
   $attrs{BPB_SecPerClus}= $self->sectors_per_cluster;
   $attrs{BPB_RsvdSecCnt}= $self->reserved_sector_count;
   $attrs{BPB_NumFATs}=    $self->fat_count;
   $attrs{BPB_RootEntCnt}= $self->root_dirent_count;
   $attrs{BS_VolID} //= time & 0xFFFFFFFF;
   my $fields;
   if ($self->bits == FAT32) {
      $attrs{BPB_FATSz16}= 0;
      $attrs{BPB_FATSz32}= $self->fat_sector_count;
      $attrs{BPB_TotSec16}= 0;
      $attrs{BPB_TotSec32}= $self->total_sector_count;
      $attrs{BS_FilSysType}= "FAT";
      $fields= \@sector0_fat32_fields;
   } else {
      $attrs{BPB_FATSz16}= $self->fat_sector_count < 0x10000? $self->fat_sector_count : 0;
      $attrs{BPB_FATSz32}= $self->fat_sector_count < 0x10000? 0 : $self->fat_sector_count;
      $attrs{BPB_TotSec16}= $self->total_sector_count < 0x10000? $self->total_sector_count : 0;
      $attrs{BPB_TotSec32}= $self->total_sector_count < 0x10000? 0 : $self->total_sector_count;
      $attrs{BS_FilSysType}= $self->bits == 12? 'FAT12' : 'FAT16';
      $fields= \@sector0_fat16_fields;
   }
   $attrs{$_->[0]} //= $_->[4] // croak "No value supplied for $_->[0], and no default"
      for @$fields;
   my $packstr= join ' ', map '@'.$_->[1].$_->[3], @$fields;
   substr($$buf_ref, 0, 512, pack($packstr, @attrs{map $_->[0], @$fields}));
   # TODO: write fat32 extra structs, backup boot sector, boot loader, etc.
   return $buf_ref;
}

=constructor unpack

  $geom= Sys::Export::VFAT::Geometry->unpack($scalar_or_scalar_ref, %options);

This reads the geometry from a FAT boot sector.  The scalar must be at least the first 512 bytes
of the filesystem.  You should leave most options un-set so that they derive from the boot
sector values, but might choose to set:

=over

=item device_offset

If you know the device offset of the FAT volume, setting this lets you use the various
<*_device_offset> attributes and methods.

=back

=cut

sub unpack {
   my $class= shift;
   my $buf_ref= ref $_[0] eq 'SCALAR'? $_[0] : \$_[0];
   my %attrs= ref $_[1] eq 'HASH'? %{$_[1]} : @_[1..$#_];
   length($$buf_ref) >= 512 or croak "Pass at least the entire first sector to 'unpack'";
   # According to the official spec, the only way to know whether you have FAT32 or FAT16
   # is to calculate the count of clusters available in the data region, which this module
   # implements in the constructor.  However, in order to unpack all of the fields of
   # sector0, you have to know whether it is FAT16 or FAT32 because FAT32 moves some of the
   # fields.  But you need those extended fields to calculate whether it is FAT32 or not...
   # It's sort of a bullshit circular dependency when clearly you could use the
   # BPB_RootEntCnt to know whether it was FAT32 or not, since FAT32 will *always* be 0 and
   # the previous generations *can't* be 0.
   # Anyway, instead of uniform single-pass field unpacking, we get this:
   state %fields= qw(
      bytes_per_sector      @11v
      sectors_per_cluster   @13C
      reserved_sector_count @14v
      fat_count             @16C
      root_dirent_count     @17v
      fat_sector_count      @22v
      fat_sector_count32    @36V
      total_sector_count    @19v
      total_sector_count32  @32V
   );
   state @fields= keys %fields;
   state $packstr= join ' ', values %fields;

   @attrs{@fields}= unpack $packstr, $$buf_ref;
   for (qw( fat_sector_count total_sector_count )) {
      my $_32= delete $attrs{$_.'32'};
      $attrs{$_} ||= $_32;
   }
   return $class->new(%attrs);
}

# Avoiding dependency on namespace::clean
{  no strict 'refs';
   delete @{"Sys::Export::VFAT::Geometry::"}{qw(
      carp confess croak ceil isa_array isa_export_dst isa_group isa_hash isa_exporter isa_int
      isa_pow2 isa_user isa_userdb round_up_to_multiple round_up_to_pow2
   )}
}

1;
