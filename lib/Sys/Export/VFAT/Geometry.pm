package Sys::Export::VFAT::Geometry;

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export ':isa';
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
my sub _round_up_pow2($number) {
   $number |= $number >> 1;
   $number |= $number >> 2;
   $number |= $number >> 4;
   $number |= $number >> 8;
   $number |= $number >> 16;
   $number |= $number >> 32;
   return $number+1;
}
my sub isa_pow2($number) { $number == _round_up_pow2($number-1) }
my sub _round_up_to_alignment($number, $pow2) {
   my $mask= $pow2-1;
   return ($number + $mask) & ~$mask;
}

sub new($class, @attrs) {
   my %attrs= @attrs == 1 && isa_hash $attrs[0]? %{$attrs[0]} : @attrs;
   my ($bytes_per_sector, $sectors_per_cluster, $fat_count,     $reserved_sector_count,
       $fat_sector_count, $root_dirent_count,   $cluster_count, $total_sector_count, $bits,
       $align_clusters,   $volume_offset)
      = delete @attrs{qw(
        bytes_per_sector   sectors_per_cluster   fat_count       reserved_sector_count
        fat_sector_count   root_dirent_count     cluster_count   total_sector_count   bits
        align_clusters     volume_offset
      )};
   !defined $align_clusters or isa_pow2($align_clusters)
      or croak "align_clusters must be a power of 2 (was $align_clusters)";
   $volume_offset //= 0;
   isa_int($volume_offset) && $volume_offset >= 0
      or croak "volume_offset must be a non-negative integer";

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
      bytes_per_sector    => $bytes_per_sector,
      sectors_per_cluster => $sectors_per_cluster,
      fat_count           => $fat_count,
      volume_offset       => $volume_offset,
   };
   
   # From here down, we are either determining cluster_count from other properties,
   # or deriving other properties from cluster_count.
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
         
         # But also, increase to the minimum number of clusters if a specific number of bits
         # was requested.
         if (my $min_bits= delete $attrs{min_bits}) {
            $cluster_count= FAT32_IDEAL_MIN_CLUSTERS if $attrs{min_bits} == FAT32 && $cluster_count < FAT32_IDEAL_MIN_CLUSTERS;
            $cluster_count= FAT16_IDEAL_MIN_CLUSTERS if $attrs{min_bits} == FAT16 && $cluster_count < FAT16_IDEAL_MIN_CLUSTERS;
         }
      }
   }
   else {
      croak "Not enough attributes supplied to determine geometry";
   }
   $self->{cluster_count}= $cluster_count;

   # These are the official boundary numbers that determine the filesystem type
   my $calc_bits= $cluster_count < FAT16_MIN_CLUSTERS? FAT12
                : $cluster_count < FAT32_MIN_CLUSTERS? FAT16
                : FAT32;
   croak "cluster_count $cluster_count implies $calc_bits, which conflicts with requested $bits"
      if defined $bits && $bits != $calc_bits;
   $self->{bits}= $bits= $calc_bits;

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
      my $data_media_offset= $volume_offset + $bytes_per_sector * (
         $reserved_sector_count
         + ($fat_count*$fat_sector_count)
         + ceil($root_dirent_count / $self->dirent_per_sector)
      );
      # If the cluster size is greater or equal to the requested alignment, ensure the
      # data start falls on that boundary.
      # If the cluster size is smaller than the requested alignment, ensure the data start
      # falls on a cluster boundary so that some number of clusters will equal the alignment.
      my $align= ($cluster_size >= $align_clusters)? $align_clusters : $cluster_size;
      if (my $ofs= $data_media_offset & ($align-1)) {
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

sub bytes_per_sector    { $_[0]{bytes_per_sector} }
sub sectors_per_cluster { $_[0]{sectors_per_cluster} }
sub bytes_per_cluster   { $_[0]{bytes_per_sector} * $_[0]{sectors_per_cluster} }
sub dirent_per_sector   { $_[0]{bytes_per_sector} / 32 }
sub dirent_per_cluster  { $_[0]{bytes_per_sector} * $_[0]{sectors_per_cluster} / 32 }
sub volume_offset       { $_[0]{volume_offset} }

sub bits                  { $_[0]{bits} }
sub reserved_sector_count { $_[0]{reserved_sector_count} }
sub fat_count             { $_[0]{fat_count} }
sub fat_sector_count      { $_[0]{fat_sector_count} }
sub cluster_count         { $_[0]{cluster_count} }

sub root_dirent_count     { $_[0]{root_dirent_count} }
sub root_sector_count($self) {
   $self->{root_sector_count} //= ceil($self->root_dirent_count / $self->dirent_per_sector);
}

sub data_first_sector($self) {
   $self->{data_first_sector} //= $self->reserved_sector_count
      + $self->fat_count * $self->fat_sector_count
      + $self->root_sector_count;
}
sub data_sector_count($self) {
   $self->total_sector_count - $self->data_first_sector;
}
sub total_sector_count($self) {
   $self->{total_sector_count} //= $self->data_first_sector
      + $self->cluster_count * $self->sectors_per_cluster;
}
sub min_cluster_idx { 2 }
sub max_cluster_idx { $_[0]->cluster_count + 1 }
sub get_cluster_first_sector($self, $cluster_idx) {
   croak "Cluster 0 and 1 are reserved" unless $cluster_idx > 1;
   croak "Cluster $cluster_idx beyond end of volume" if $cluster_idx > $self->max_cluster_idx;
   return $self->data_first_sector + ($cluster_idx-2) * $self->sectors_per_cluster;
}
sub get_cluster_of_sector($self, $sector_idx) {
   return undef if $sector_idx < $self->data_first_sector;
   my $cluster= int(($sector_idx - $self->data_first_sector) / $self->sectors_per_cluster);
   return undef if $cluster >= $self->cluster_count;
   return $cluster + 2;
}

# byte-based aliases
sub total_size($self) { $self->total_sector_count * $self->bytes_per_sector }
sub data_offset($self) { $self->data_first_sector * $self->bytes_per_sector }
sub get_cluster_offset($self, $cluster_id) {
   return $self->get_cluster_first_sector($cluster_id) * $self->bytes_per_sector;
}
sub get_cluster_of_offset($self, $offset) {
   return $self->get_cluster_of_sector(int($offset / $self->bytes_per_sector));
}

# media-byte-offset aliases
sub data_media_offset($self) { $self->data_offset + $self->volume_offset }
sub get_cluster_media_offset($self, $cl_id) { $self->get_cluster_offset($cl_id) + $self->volume_offset }
sub get_cluster_of_media_offset($self, $ofs) { $self->get_cluster_of_offset($ofs) - $self->volume_offset }

# The caller supplies a range of bytes as an absolute offset from the start of the volume and
# a size in bytes.  The offset must also match the start of a cluster, or this dies.
# This returns the cluster number and also the size in clusters.
sub get_cluster_range_of_byte_range($self, $offset, $size) {
   my $cl_id= $self->get_cluster_of_offset($offset);
   $self->get_cluster_offset($cl_id) == $offset
      or croak "FAT_offset not aligned to a cluster boundary";
   return ($cl_id, ceil($size / $self->bytes_per_cluster));
}
# The caller supplies a power-of-2 alignment, and optionally a different origin than the
# start of this volume.  This dies if the specified alignment dosn't fall on a cluster
# boundary.
# This returns the alignment in terms of cluster index, possibly also with an origin offset
# in terms of clusters.
sub get_cluster_alignment_of_media_alignment($self, $align) {
   my $cluster_size= $self->bytes_per_cluster;
   # If the cluster size is greater or equal to the requested alignment, verify that the
   # data start (plus the volume offset that was implicitly added to every volume address)
   # meets that alignment.  If so, every cluster is aligned and the return value is (1,0)
   if ($cluster_size >= $align) {
      croak "Clusters are not aligned to $align"
         if $self->data_media_offset & ($align-1);
      return (1,0);
   }
   # Otherwise, make sure the data_start (plus implied volume offset) is aligned to
   # cluster_size, so then some multiple of clusters will reach the requested alignment.
   croak "Clusters are not aligned to $cluster_size"
      if $self->data_media_offset & ($cluster_size-1);
   # The cluster alignment will be whatever multiple of clusters equals the byte alignment.
   # This will be at least 2.
   my $cl_align= $align / $cluster_size;
   # How many bytes away from alignment is the beginning of ficticious cluster 0?
   my $ofs_of_cl0= ($self->data_media_offset - ($cluster_size*2)) & ($align-1);
   # If not aligned, add the rest of the distance to the next alignment.
   my $cl_ofs= !$ofs_of_cl0? 0 : ($align - $ofs_of_cl0) / $cluster_size;
   return ($cl_align, $cl_ofs);
}

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
sub unpack {
   my $class= shift;
   my $buf_ref= ref $_[0] eq 'SCALAR'? $_[0] : \$_[0];
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

   my %attrs;
   @attrs{@fields}= unpack $packstr, $$buf_ref;
   for (qw( fat_sector_count total_sector_count )) {
      my $_32= delete $attrs{$_.'32'};
      $attrs{$_} ||= $_32;
   }
   return $class->new(%attrs);
}

sub pack {
   my $self= shift;
   my $buf_ref= !@_? \(my $buf= '') : ref $_[0] eq 'SCALAR'? shift : \shift;
   my %attrs= @_ == 1 && isa_hash $_[0]? %{$_[0]} : @_;
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
   return $buf_ref;
}

1;
