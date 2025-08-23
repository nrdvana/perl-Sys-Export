package Sys::Export::VFAT;
# ABSTRACT: Write CPIO archives needed for Linux initrd
# VERSION

use v5.26;
use warnings;
use experimental qw( signatures );
use Fcntl qw( S_IFDIR S_ISDIR );
use Scalar::Util qw( blessed dualvar );
use Encode;
use Carp;
our @CARP_NOT= qw( Sys::Export Sys::Export::Unix );
use constant {
   FAT12 => dualvar(12, "FAT12"),
   FAT16 => dualvar(16, "FAT16"),
   FAT32 => dualvar(32, "FAT32"),
   ATTR_READONLY  => dualvar(0x01, 'ATTR_READONLY'),
   ATTR_HIDDEN    => dualvar(0x02, 'ATTR_HIDDEN'),
   ATTR_SYSTEM    => dualvar(0x04, 'ATTR_SYSTEM'),
   ATTR_VOLUME_ID => dualvar(0x08, 'ATTR_VOLUME_ID'),
   ATTR_DIRECTORY => dualvar(0x10, 'ATTR_DIRECTORY'),
   ATTR_ARCHIVE   => dualvar(0x20, 'ATTR_ARCHIVE'),
   ATTR_LONG_NAME => dualvar(0x0F, 'ATTR_LONG_NAME'),
   ATTR_LONG_NAME_MASK => 0x3F,
};
use Exporter 'import';
our @EXPORT_OK= qw( FAT12 FAT16 FAT32 ATTR_READONLY ATTR_HIDDEN ATTR_SYSTEM ATTR_ARCHIVE );
use Sys::Export ':isa';

=head1 SYNOPSIS

  my $dst= Sys::Export::VFAT->new(filename => $path, volume_label => 'ESP');
  $dst->add(\%file_attrs);
  ...
  $dst->finish;
  # $path now contains a Fat32, Fat16, or Fat12 filesystem

=head1 DESCRIPTION

This module can be used as an export destination to build a FAT32/16/12 filesystem by directly
encoding your files into a very compact VFAT layout.  The generated filesystem has no
fragmentation and no free space.

This implementation caches all files in memory, and then chooses FAT parameters that result in
the smallest image.  If you know ahead of time the cluster size you want and how many clusters
to allocate, this module can instead stream your files into the destination.

This implementation also has some fun features intended to work together with the
L<ISOHybrid|Sys::Export::ISOHybrid> module, which can (on the assumption that the filesystem will
never be written) encode hardlinks, encode symlinks as hard-linked directories, and place files
at specific offsets within the generated image.

=head2 FAT Geometry

To understand the attributes of this module, it helps to first understand how FAT is structured.

FAT defines a Sector as some number of bytes (generally 512) and then a Cluster as a number of
Sectors.  The overall image consists of a header, two allocation tables, and then the data area.
The allocation table is essentially an array of cluster pointers, the second allocation table is
a backup copy of the first, and the data area is an array of clusters.
The allocation table is sized so that it has one cluster pointer per data-area cluster.
This forms a linked list of clusters, so for any file or directory larger than one cluster,
you consult that cluster's entry in the allocation table to find the next cluster.
All files and directories are rounded up to the cluster size when they are stored.  The size of
the cluster pointers depends on the total number of clusters in the filesystem, so for a total
cluster count that fits in 12 bits you get FAT12, if it fits in 16 bits you get FAT16, and up
to 28 bits uses FAT32.  This means that if you specifically need "FAT32" for interoperability
resons (such as badly written BIOSes), there is a minimum filesystem size of ~32MB, because the
selection of 12/16/32 is driven by the cluster count you specify in the header.

You can now see why it isn't possible to begin writing clusters until the cluster count is
known, because the first cluster will be located at the end of an array whose size is based on
the total number of clusters, and the encoding of directories will depend on whether it is
writing FAT12/16/32 directory entries which is also dependent on the total number of clusters.

The "V" in VFAT refers to the long file name support that Microsoft added with Windows 95.
The directory encoding for FAT only has 11 characters available for each file name.
Rather than inventing a new directory entry encoding, they store longer file names in one or
more hidden directory entries (each containing a fragment of the name) right before the visible
one that actually references the file.  This is backward compatible, so it applies equally well
to all the bit-widths.

The newer exFAT format (not supported by this module) is a completely different format, more
similar to modern filesystems which store variable-length directory entries and which describe
file data locations with "extents" (offset and length) rather than an awkward linked list of
cluster numbers.

=head2 Algorithm

When you call L</finish>, this module will scan over all the files and directories you have
added and total up the number of clusters required for each of the possible cluster sizes.
Incidentally, directories are encoded in the same number of bytes across all 3 bit-widths,
so this process also populates 'data' and 'size' of every directory node.
It then selects the cluster size and cluster count that results in the smallest total size.
Then it builds a flat array of files and directories to assign their start-cluster as it also
builds the allocation table.  It then makes a second pass across all the files and directories
to copy file data into the image and encode each directory now that the start-clusters are
known.

=constructor new

  $fat= Sys::Export::VFAT->new($filename);
  $fat= Sys::Export::VFAT->new(%attrs);
  $fat= Sys::Export::VFAT->new(\%attrs);

This takes a list of attributes as a hashref or key/value list.  If there is exactly one
argument, it is treated as the filename attribute.

=cut

sub new($class, @attrs) {
   @attrs= (isa_hash $attrs[0])? %{$attrs[0]} : ( filename => $attrs[0])
      if @attrs == 1;
   croak "Expected even-length key/value list of attributes" if @attrs & 1;

   my $self= bless { _root => undef }, $class;
   while (@attrs) {
      my ($attr, $val)= splice(@attrs, 0, 2);
      $self->$attr($val);
   }

   $self;
}

sub _root { $_[0]{_root} }

=method add

  $fat->add(\%file_attrs);
  # Attributes:
  # {
  #   name               => $path_utf8_bytes,
  #   FAT_shortname      => "8CHARATR.EXT",
  #   FAT_utf16_name     => $path_utf16_bytes,
  #   FAT_utf16_basename => $basename_utf16_bytes,
  #   mode               => $unix_stat_mode,
  #   FAT_flags          => ATTR_READONLY|ATTR_HIDDEN|ATTR_SYSTEM|ATTR_ARCHIVE,
  #   atime              => $unix_epoch,
  #   mtime              => $unix_epoch,
  #   crtime             => $unix_epoch,
  #   size               => $data_size,
  #   data               => $literal_data,
  #   data_path          => $abs_path_to_data,
  #   FAT_offset         => $desired_byte_offset,
  # }

This add method takes the same file objects as used by Sys::Export, but with some optional
extras:

=over

=item FAT_shortname

Any file name not conforming to the 8.3 name limitation of FAT will get an auto-generated
"short" filename, in addition to its "long" filename.  If you want control over what short name
is generated, you can specify it with C<FAT_shortname>.

=item FAT_utf16_name

The "long" filename is stored as UTF-16.  The Perl/Unix assumption about "everyhting is just
bytes" breaks down here because now we need to know what encoding those bytes were in order
to decode them and re-encode as UTF-16.  This module assumes the bytes of C<name> are UTF-8,
but if that is not correct, you can override it with this attribute.

=item FAT_utf16_basename

Like C<FAT_utf16_name>, but just the file name and not its path.

=item FAT_flags

The Unix permission bits of the C<mode> can only be used to determine the READONLY flag of the
FAT entry.  (while Samba does more mapping of these, I don't think that's appropriate for this
use case)  If you want to directly specify the FAT flags, pass an ORed constant as C<FAT_flags>.

=item FAT_offset

For integration with ISOHybrid, you may specify C<FAT_offset> (bytes, from start of filesystem)
to request the file be placed at an exact location.  This must fall on the address of one of
the clusters of the data region, and will generate an exception if it can't be honored.
Unfortunately you won't get that exception until L</finish> is called.

=back

=cut

# The smallest conceivable address where the data region could start
my $minimum_offset_to_data= Sys::Export::VFAT::Geometry->new(
   bytes_per_sector => 512,
   sectors_per_cluster => 1,
   fat_count => 1,
   root_dirent_count => 1,
   cluster_count => 1
   )->data_offset;

sub add($self, $file) {
   # If user supplied FAT_utf16_name, use that as a more official source of Unicode
   my $uname= length $file->{FAT_utf16_name}
      ? decode('UTF-16', $file->{FAT_utf16_name}, Encode::FB_CROAK)
      : decode('UTF-8', $file->{name}, Encode::FB_CROAK);
   _validate_name($uname);
   my @path= split '/', $uname;
   my $leaf= pop @path;
   my $auto_dir= sub { +{ mode => S_IFDIR, FAT_by_fc_name => {}, FAT_is_auto_dir => 1 } };
   # Walk through the tree based on the case-folded path
   my $dir= ($self->{_root} //= $auto_dir->());
   my $fc;
   for (@path) {
      $fc= fc $_;
      my $existing= $dir->{FAT_by_fc_name}{$fc};
      if ($existing) {
         S_ISDIR($dir->{mode}) or croak "'$fc' is not a directory, while attempting to add '$uname'";
         $dir= $existing;
      } else {
         # auto-create directory
         $dir= ($dir->{FAT_by_fc_name}{$fc}= $auto_dir->());
      }
   }
   # Now add the leaf entry
   my $ent= $dir->{FAT_by_fc_name}{$fc= fc $leaf};
   if ($ent) {
      # If the user is writing a directory and the thing in the way is an auto-dir,
      # replace the entry with the new attributes.
      if (S_ISDIR($file->{mode}) && $ent->{FAT_is_auto_dir}) {
         delete $ent->{FAT_is_auto_dir};
         $ent->{$_}= $file->{$_} for keys %$file;
      }
      else {
         croak "Path '$uname' already exists";
      }
   }
   else {
      $ent= $dir->{FAT_by_fc_name}{$fc}= { %$file };
   }
   $ent->{FAT_utf16_basename} //= encode('UTF-16', $leaf, Encode::FB_CROAK);
   # Sanity check FAT_offset before we get too far along
   if (defined $ent->{FAT_offset}) {
      # must fall in the data area
      $ent->{FAT_offset} > $minimum_offset_to_data
      # must be a multiple of at least 512 (probably more)
      && !($ent->{FAT_offset} & 0x1FF)
         or croak "Invalid FAT_offset '$ent->{FAT_offset}' for file '$uname'";
   }
   $self;
}

sub _validate_name($name) {
   $name =~ /\\/ and croak "Name may not contain backslash";
   # TODO: look up all illegal characters for VFAT names
}

sub finish($self) {
   ...
}

package Sys::Export::VFAT::Geometry {
   use v5.26;
   use warnings;
   use experimental qw( signatures );
   use Sys::Export ':isa';
   use Carp;
   BEGIN {
      *FAT12= *Sys::Export::VFAT::FAT12;
      *FAT16= *Sys::Export::VFAT::FAT16;
      *FAT32= *Sys::Export::VFAT::FAT32;
      our %valid_bytes_per_sector= ( 512 => 1, 1024 => 1, 2048 => 1, 4096 => 1 );
      our %valid_sectors_per_cluster= ( map +($_ => 1), qw( 1 2 4 8 16 32 64 128 ));
   }
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
   our %valid_bytes_per_sector;
   our %valid_sectors_per_cluster;

   sub new($class, @attrs) {
      my %attrs= @attrs == 1 && isa_hash $attrs[0]? %{$attrs[0]} : @attrs;
      my ($bytes_per_sector, $sectors_per_cluster, $fat_count,     $reserved_sector_count,
          $fat_sector_count, $root_dirent_count,   $cluster_count, $total_sector_count, $bits)
         = delete @attrs{qw(
           bytes_per_sector   sectors_per_cluster   fat_count       reserved_sector_count
           fat_sector_count   root_dirent_count     cluster_count   total_sector_count   bits
         )};

      $valid_bytes_per_sector{$bytes_per_sector //= 512}
         or croak "Invalid bytes_per_sector";

      # Default sectors_per_cluster to whatever makes 4K
      $sectors_per_cluster //= ($bytes_per_sector >= 4096? 1 : 4096 / $bytes_per_sector);
      $valid_sectors_per_cluster{$sectors_per_cluster}
         or croak "Invalid sectors_per_cluster";
      $bytes_per_sector * $sectors_per_cluster <= 32*1024
         or carp "Warning: bytes_per_sector * sectors_per_cluster > 32KiB which is not valid for some drivers";

      # Default fat_count to 2 unless specified otherwise
      $fat_count //= 2;
      isa_int $fat_count && 0 < $fat_count && $fat_count <= 255
         or croak "Invalid fat_count";

      my $self= bless {
         bytes_per_sector    => $bytes_per_sector,
         sectors_per_cluster => $sectors_per_cluster,
         fat_count           => $fat_count,
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
      $self->{root_dirent_count}= $root_dirent_count;
      $self->{reserved_sector_count}= $reserved_sector_count;
      
      carp "Unused constructor parameters: ".join(' ', keys %attrs)
         if keys %attrs;
      $self;
   }

   sub bytes_per_sector    { $_[0]{bytes_per_sector} }
   sub sectors_per_cluster { $_[0]{sectors_per_cluster} }
   sub dirent_per_sector   { $_[0]{bytes_per_sector} / 32 }
   sub dirent_per_cluster  { $_[0]{bytes_per_sector} * $_[0]{sectors_per_cluster} / 32 }

   sub bits                  { $_[0]{bits} }
   sub reserved_sector_count { $_[0]{reserved_sector_count} }
   sub fat_count             { $_[0]{fat_count} }
   sub fat_sector_count      { $_[0]{fat_sector_count} }
   sub cluster_count         { $_[0]{cluster_count} }

   sub root_dirent_count     { $_[0]{root_dirent_count} }
   sub root_sector_count($self) {
      $self->{root_sector_count} //= int(($self->root_dirent_count + ($self->dirent_per_sector-1)) / $self->dirent_per_sector);
   }

   sub total_sector_count($self) {
      $self->{total_sector_count} //= $self->reserved_sector_count
         + $self->fat_count * $self->fat_sector_count
         + $self->root_sector_count
         + $self->cluster_count * $self->sectors_per_cluster;
   }

   sub data_first_sector($self) {
      $self->reserved_sector_count
      + $self->fat_count * $self->fat_sector_count
      + $self->root_sector_count
   }
   sub data_offset($self) { $self->data_first_sector * $self->bytes_per_sector }
   sub data_sector_count($self) {
      $self->total_sector_count - $self->data_first_sector;
   }
   sub min_cluster_idx { 2 }
   sub max_cluster_idx { $_[0]->cluster_count + 1 }
   sub get_cluster_first_sector($self, $cluster_idx) {
      croak "Cluster 0 and 1 are reserved" unless $cluster_idx > 1;
      croak "Cluster $cluster_idx beyond end of volume" if $cluster_idx > $self->max_cluster_idx;
      return $self->data_first_sector + ($cluster_idx-2) * $self->sectors_per_cluster;
   }
   sub get_cluster_offset($self, $cluster_idx) {
      return $self->get_cluster_first_sector($cluster_idx) * $self->bytes_per_sector;
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
}

1;
