package Sys::Export::VFAT;
# ABSTRACT: Write CPIO archives needed for Linux initrd
# VERSION

use v5.26;
use warnings;
use experimental qw( signatures );
use Fcntl qw( S_IFDIR S_ISDIR S_ISREG );
use Scalar::Util qw( blessed dualvar refaddr );
use List::Util qw( min max );
use POSIX 'ceil';
use Encode;
use Carp;
our @CARP_NOT= qw( Sys::Export Sys::Export::Unix );
use constant {
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
our @EXPORT_OK= qw( FAT12 FAT16 FAT32 ATTR_READONLY ATTR_HIDDEN ATTR_SYSTEM ATTR_ARCHIVE
  is_valid_longname is_valid_shortname is_valid_volume_label );
use Sys::Export qw( :isa expand_stat_shorthand );
use Sys::Export::VFAT::Geometry qw( FAT12 FAT16 FAT32 );
use Sys::Export::VFAT::AllocationTable;
use Log::Any '$log';

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
   my %attrs= @attrs != 1? @attrs
            : isa_hash $attrs[0]? %{$attrs[0]}
            : ( filename => $attrs[0] );
   length $attrs{filename} or croak "filename is required";
   my $self= bless {
      filename => delete $attrs{filename},
      _root => {
         mode => S_IFDIR,
         uname => '',
         FAT_shortname => '',
         FAT_longname  => '',
         FAT_is_auto_dir => 1,
      }
   }, $class;
   for (keys %attrs) {
      $self->$_($attrs{$_});
   }

   $self;
}

=attribute filename

Name of file to write.  Note that this cannot be a device file unless you zero it first, because
this module makes the assumption that it doesn't need to write the zero padding, because the
file should have been zeroed by truncate().

=cut

sub _root { $_[0]{_root} }
sub filename { $_[0]{filename} }

=attribute geometry

An instance of L<Sys::Export::VFAT::Geometry> that describes the size and location of VFAT
structures.  This is generated during L</finish>, but if you have very rigid ideas about how
the filesystem should be laid out, you can pass it to the constructor.

=attribute volume_offset

This value causes the entire volume to be written at an offset from the start of the file or
device.  This value is part of the calculation for methods like L</get_file_device_extent> and
alignment of files to device addresses.  If you are writing this filesystem within a partition
of a larger device, set this attribute to get correct device alignments.

If not set, it defaults to 0, so alignments will be performed relative to the start of the
volume, and methods like L</get_file_device_extent> will return ofsets relative to the volume.

=cut

sub geometry($self) { $self->{geometry} }

sub volume_offset($self, @val) {
   if ($self->{geometry}) {
      croak "Geometry already decided" if @val;
      return $self->{geometry}->volume_offset;
   }
   if (@val) {
      croak "volume_offset must be a multiple of 512" if $val[0] & 511;
      return $self->{volume_offset}= $val[0];
   }
   $self->{volume_offset} // 0
}

=attribute bytes_per_sector

Force a sector size other than the default 512.

=attribute sectors_per_cluster

Force a number of sectors per cluster.  The default is to try different sizes to see which
results in the smallest filesystem.

=attribute fat_count

Force a number of allocation tables.  Two is standard (for redundancy in case of disk errors)
but setting this to C<1> saves some space.

=attribute free_space

By default, the filesystem is created with zero free clusters.  Specify this (in bytes) to add
some free space to the generated filesystem.

=attribute volume_label

Volume label of the generated filesystem

=cut

sub bytes_per_sector($self, @val) {
   if ($self->{geometry}) {
      croak "Geometry already decided" if @val;
      return $self->{geometry}->bytes_per_sector;
   }
   if (@val) {
      croak "bytes_per_sector must be a power of 2" unless isa_pow2 $val[0];
      return $self->{bytes_per_sector}= $val[0];
   }
   $self->{bytes_per_sector} // 512
}

sub sectors_per_cluster($self, @val) {
   if ($self->{geometry}) {
      croak "Geometry already decided" if @val;
      return $self->{geometry}->sectors_per_cluster;
   }
   if (@val) {
      croak "sectors_per_cluster must be a power of 2, and 128 or less" unless isa_pow2 $val[0] && $val[0] <= 128;
      return $self->{sectors_per_cluster}= $val[0];
   }
   $self->{sectors_per_cluster}
}

sub fat_count($self, @val) {
   if ($self->{geometry}) {
      croak "Geometry already decided" if @val;
      return $self->{geometry}->fat_count;
   }
   if (@val) {
      croak "fat_count must be positive" unless isa_int $val[0] && $val[0] > 0;
      return $self->{fat_count}= $val[0];
   }
   $self->{fat_count}
}

sub free_space($self, @val) {
   if (@val) {
      croak "Geometry already decided" if $self->{geometry};
      return $self->{free_space}= $val[0];
   }
   $self->{free_space} // 0
}

sub volume_label($self, @val) {
   if (@val) {
      croak "Invalid volume label '$val[0]'" unless is_valid_volume_label($val[0]);
      return $self->{volume_label}= $val[0];
   }
   $self->{volume_label}
}

=method add

  $fat->add(\%file_attrs);
  # Attributes:
  # {
  #   name               => $path_utf8_bytes,
  #   uname              => $path_unicode_string,
  #   FAT_shortname      => "8CHARATR.EXT",
  #   mode               => $unix_stat_mode,
  #   FAT_attrs          => ATTR_READONLY|ATTR_HIDDEN|ATTR_SYSTEM|ATTR_ARCHIVE,
  #   atime              => $unix_epoch,
  #   mtime              => $unix_epoch,
  #   crtime             => $unix_epoch,
  #   size               => $data_size,
  #   data               => $literal_data,
  #   data_path          => $abs_path_to_data,
  #   device_offset      => $desired_byte_offset,
  #   device_align       => $desired_byte_alignment_pow2,
  # }

This add method takes the same file objects as used by Sys::Export, but with some optional
extras:

=over

=item FAT_shortname

Any file name not conforming to the 8.3 name limitation of FAT will get an auto-generated
"short" filename, in addition to its "long" filename.  If you want control over what short name
is generated, you can specify it with C<FAT_shortname>.

=item FAT_attrs

An ORed combination of L</ATTR_READONLY>, L</ATTR_HIDDEN>, L</ATTR_SYSTEM>, or L</ATTR_ARCHIVE>.
If this value is defined, this module will *not* use the C<mode> user-write bit to determine
C<ATTR_READONLY> and will not use leading "." to determine C<ATTR_HIDDEN>.

=item device_offset

For integration with ISOHybrid, you may specify C<device_offset> to request the file be placed
at an exact location, and as a single un-fragmented extent.  This accounts for the
L</device_offset> of the whole filesystem.  If you did not set that attribute, this becomes a
byte offset from the start of this filesystem.

This offset must fall on the address of one of the clusters of the data region, and will
generate an exception if it can't be honored.  It must also agree with any C<device_align> you
requested on other files.
Unfortunately you won't get that exception until L</finish> is called, as this module looks for
workable cluster layouts.

You may also set this to a scalar-ref which will I<receive> the device_offset once the file's
location is decided.

=item device_align

Like C<device_offset>, but if you just want to request the file be aligned to the device rather
than needing it to exist at a specific offset.  This is a power of 2 in bytes, such as '2048'.
This takes attribute L</volume_offset> into account, possibly providing alignment that
is a multiple of a power-of-2 from the start of the device but not from the start of the volume.

=back

=cut

sub add($self, $file) {
   $file= { expand_stat_shorthand($file) }
      if isa_array $file;
   defined $file->{uname} or defined $file->{name}
      or croak "Require 'uname' or 'name'";
   defined $file->{mode} or croak "Require 'mode'";
   # If user supplied FAT_utf16_name, use that as a more official source of Unicode
   my $uname= $file->{uname} // decode('UTF-8', $file->{name}, Encode::FB_CROAK);
   $uname =~ s,^/,,; # remove leading slash
   croak "Not a valid VFAT filename: '$uname'"
      unless is_valid_longname($uname);
   my @path= grep length, split '/', $uname;
   my $leaf= pop @path;
   # Walk through the tree based on the case-folded path
   my $dir= $self->{_root};
   for (@path) {
      my $existing= $dir->{FAT_by_fc_name}{fc $_}
         # auto-create directory
         //= { mode => S_IFDIR, FAT_longname => $_, FAT_is_auto_dir => 1 };
      S_ISDIR($dir->{mode}) or croak "'$_' is not a directory, while attempting to add '$uname'";
      $dir= $existing;
   }
   # Check collision on case-insensitive name.  '$leaf' is unicode.
   my $by_fc= $dir->{FAT_by_fc_name} //= {};
   my $ent= $by_fc->{fc $leaf};
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
      $ent= { %$file };
   }
   $ent->{uname}= $uname; # store unicode for later reporting
   delete $ent->{name};
   # Validate FAT_shortname
   if (length $ent->{FAT_shortname}) {
      is_valid_shortname($ent->{FAT_shortname})
         or croak "Invalid FAT_shortname '$ent->{FAT_shortname}' for '$uname'";
   }
   # If the normal name is also a valid shortname, assign that
   elsif (is_valid_shortname($leaf)) {
      $ent->{FAT_shortname}= $leaf;
   }
   # Check for collision on the FAT_shortname if known
   if (length $ent->{FAT_shortname}) {
      utf8::downgrade($ent->{FAT_shortname}); # verified to be just bytes
      defined $by_fc->{fc $ent->{FAT_shortname}}
         and croak "Duplicate shortname '$ent->{FAT_shortname}' at '$uname'";
      $by_fc->{fc $ent->{FAT_shortname}}= $ent;
   }
   $by_fc->{fc $leaf}= $ent;
   $ent->{FAT_longname}= $leaf;

   # Sanity check device_offset before we get too far along
   if (defined $ent->{device_offset}) {
      # The smallest conceivable address where the data region could start
      state $minimum_offset_to_data= Sys::Export::VFAT::Geometry->new(
            bytes_per_sector => 512,
            sectors_per_cluster => 1,
            fat_count => 1,
            root_dirent_count => 1,
            cluster_count => 1
         )->data_start_offset;

      # Can be a scalar ref, for output:
      if (ref $ent->{device_offset} eq 'SCALAR') {
         my $offset_ref= delete $ent->{device_offset};
         my $callback= sub($self2, $cl_id, $file) {
            $$offset_ref= $self2->geometry->get_cluster_device_offset($cl_id)
         };
         $ent->{FAT_cluster}= [ undef, $callback ];
         $ent->{device_align} //= 512; # mark file as needing to be a single extent
      }
      else {
         # must fall in the data area
         $ent->{device_offset} > $minimum_offset_to_data + $self->volume_offset
         # must be a multiple of at least 512 (probably more)
         && !($ent->{device_offset} & 511)
            or croak "Invalid device_offset '$ent->{device_offset}' for file '$uname'";
      }
   }
   if (defined $ent->{device_align}) {
      # must be a power of 2
      isa_pow2 $ent->{device_align}
         or croak "Invalid device_align $ent->{device_align} for '$uname', must be a power of 2";
   }
   $log->debugf("added %s", $ent) if $log->is_debug;
   $self;
}

=method finish

This method performs all the actual work of building the filesystem.  This module writes the
entire filesystem in one pass after deciding the best geometry and minimal number of clusters
to hold the data you've supplied.

You may get exceptions during this call if there isn't a way to write your files as requested.

=cut

sub finish($self) {
   # Build a list of all directories and files
   my $root= $self->{_root};
   $log->tracef("finalize root %s", $root);
   # sanity check, since this acts as an important flag
   $root->{FAT_longname} eq '' && $root->{FAT_shortname} eq ''
      or croak "Root dir has wrong name";
   # Find out the size of every directory
   $self->_finalize_dir($root, undef, \my @dirs, \my @files);
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates
   my ($geom, $alloc)= $self->_optimize_geometry(\@dirs, \@files)
      or croak join("\n", "No geometry options can meet your device_offset / device_align requests:",
            map "$_: $self->{_optimize_geometry_fail_reason}{$_}",
               sort { $a <=> $b } keys $self->{_optimize_geometry_fail_reason}->%*
         );
   $self->_pack_dir($_) for reverse(@dirs), $root;
   $self->{geometry}= $geom;
   $self->{allocation_table}= $alloc;

   open my $fh, '+>', $self->filename
      or croak "open: $!";
   truncate($fh, $geom->total_size)
      or croak "truncate: $!";
   $self->_write_filesystem($fh, $geom, $alloc, [ @files, @dirs ]);
   $fh->close or croak "close: $!";
   1;
}

sub _write_block_at {
   my ($fh, $addr, undef, $ofs, $size)= @_;
   sysseek($fh, $addr, 0) or croak "sysseek($addr): $!";
   $ofs //= 0;
   $size //= length($_[2]) - $ofs;
   my $wrote= syswrite($fh, $_[2], $size, $ofs);
   croak "syswrite: $!" if !defined $wrote;
   croak "Unexpected short write ($wrote != $size)" if $wrote != $size;
   return 1;
}   

# Given an inversion list describing the allocated clusters for this file,
# write the relevant chunks of the file to those cluster data areas.
sub _write_clusters {
   my ($fh, $geom, $alloc_invlist)= (shift, shift, shift);
   my $data_ofs= 0;
   for (my $i= 0; $i < @$alloc_invlist && $data_ofs < length($_[0]); $i += 2) {
      my ($cl_start, $cl_lim)= @{$alloc_invlist}[$i, $i+1];
      my $size= min(
         ($cl_lim-$cl_start) * $geom->bytes_per_cluster,
         length($_[0]) - $data_ofs
      );
      _write_block_at($fh, $geom->get_cluster_offset($cl_start), $_[0], $data_ofs, $size);
      $data_ofs += $size;
   }
}

# This function depends on the file being pre-zeroed, which happens automatially for an empty
# file that has just had its length changed by truncate().  This would write an invalid
# filesystem if the handle is a block device with random leftover data in it.
sub _write_filesystem($self, $fh, $geom, $alloc, $files) {
   ($alloc->max_cluster_id//-1) == ($geom->max_cluster_id//-1)
      or croak "Max element of 'fat_entries' should be ".$geom->max_cluster_id.", but was ".$alloc->max_cluster_id;
   # Pack the boot sector and other reserved sectors
   my $buf= $self->_pack_reserved_sectors();
   _write_block_at($fh, 0, $buf);
   # Pack the allocation tables
   $buf= $alloc->pack;
   # store a copy of this into each of the regions occupied by fats
   my $ofs= $geom->reserved_sector_count * $geom->bytes_per_sector;
   for (my $i= 0; $i < $geom->fat_count; $i++) {
      _write_block_at($fh, $ofs, $buf);
      $ofs += $geom->fat_sector_count * $geom->bytes_per_sector;
   }
   # For FAT12/FAT16, write the root directory entries
   if ($geom->bits < FAT32) {
      my $root= $self->{_root};
      die "BUG: mis-sized FAT16 root directory"
         if !$root->{size} || ($root->{size} & 31) || length $root->{data} != $root->{size}
            || $root->{size} > $geom->root_sector_count * $geom->bytes_per_sector;
      _write_block_at($fh, $geom->root_dir_offset, $root->{data});
   }
   # The files and dirs have all been assigned clusters by _optimize_geometry
   for my $f (@$files) {
      next unless $f->{size};
      my $cl= $f->{FAT_cluster}[0] or die "BUG: no cluster assigned";
      my $chain= $alloc->get_chain($cl) or die "BUG: no chain at $cl";
      $log->trace("Writing '$f->{uname}' at clusters "._render_invlist($chain->{invlist}))
         if $log->is_trace;
      _write_clusters($fh, $geom, $chain->{invlist}, $f->{data});
   }
}
sub _render_invlist {
   join ',', map $_[0][$_*2] . '-' . $_[0][$_*2+1], 0 .. int($#{$_[0]}/2)
}

# Record the file's cluster, and call any callbacks that were waiting on this
sub _resolve_cluster($self, $f, $cl_id) {
   my $cluster_ref= ref $f eq 'HASH'? $f->{FAT_cluster} : $f;
   $cluster_ref->[0]= $cl_id;
   $_->($self, $cl_id, $f) for @{$cluster_ref}[1..$#$cluster_ref];
}

=export is_valid_longname

=export is_valid_shortname

=export is_valid_volume_label

=cut

sub is_valid_longname {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   # characters permitted for LFN are all letters numbers and $%'-_@~`!(){}^#&+,;=[].
   # and space and all codepoints above 0x7F.
   # they may not begin with space, and cannot exceed 255 chars.
   $_[0] !~ /^(\.+\.?)\z/ # dot and dotdot are reserved
   && $_[0] =~ /^
      [\x21\x23-\x29\x2B-\x2E\x30-\x39\x3B\x3D\x40-\x5B\x5D-\x7B\x7D-\xE4\xE6-]
      [\x20\x21\x23-\x29\x2B-\x2E\x30-\x39\x3B\x3D\x40-\x5B\x5D-\x7B\x7D-\xE4\xE6-]{0,254}
      \z/x;
}

sub is_valid_shortname {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   $_[0] eq uc $_[0]
   && $_[0] =~ /^
      [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{1,8}
      ( \. [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,3} )
      \z/x;
}

sub is_valid_volume_label {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   # same as shortname but no '.' and space is allowed
   $_[0] =~ /^
      [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]
      [\x20\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,10}
      \z/x;
}

# This builds:
#  FAT_cluster
#  FAT_dirents (does not include LFN dirents, does include '.', '..', volume label)
#  size        (number of dirents + LFN, times 32 bytes)
#  for each entry:
#    ->{FAT_short11}
#    ->{size}
# and populates @$dirlist and @$filelist
sub _finalize_dir($self, $dir, $parent, $dirlist, $filelist) {
   # cluster refs get shared between nodes so that nodes don't need a ->{parent} weak ref
   # when the cluster finally gets set and needs encoded into the parent.
   $dir->{FAT_cluster} //= [ undef ];
   my @ents= values %{$dir->{FAT_by_fc_name} // {}};
   my @dirs;
   # Need the 8.3 name in order to know whether it matches the long name
   for (@ents) {
      unless (length $_->{FAT_short11}) {
         # Is the 8.3 name decided already?
         $_->{FAT_shortname} //= _coerce_to_83_name($_->{FAT_longname}, $dir->{FAT_by_shortname});
         $dir->{FAT_by_shortname}{$_->{FAT_shortname}}= $_;
         my ($name, $ext)= split /\./, $_->{FAT_shortname};
         $_->{FAT_short11}= pack 'A8 A3', $name, ($ext//'');
      }
      # Check file type
      if (S_ISDIR($_->{mode})) {
         push @dirs, $_;
      }
      elsif (S_ISREG($_->{mode})) {
         push @$filelist, $_;
         unless (defined $_->{size}) { # ensure size is initialized
            if (defined $_->{data}) {
               # Die on encoding mistakes
               !utf8::is_utf8($_->{data}) or utf8::downgrade($_->{data}, 1)
                  or croak "->{data} must be 8-bit, but encountered wide character at $_->{uname}";
               $_->{size}= length $_->{data};
            }
            elsif (defined $_->{data_path}) {
               $_->{size}= -s $_->{data_path} // croak "Can't read data_path=$_->{data_path} of $_->{uname}";
            }
            else {
               carp "File $_->{uname} has neither 'size' nor 'data' attributes";
               $_->{data}= '';
               $_->{size}= 0;
            }
         }
      }
      else {
         # TODO: add conditional symlink support via hardlinks
         croak "Unsupported directory entry type at $_->{uname}";
      }
   }
   # Sort into a stable order.
   @ents= sort { $a->{FAT_longname} cmp $b->{FAT_longname} } @ents;
   $dir->{FAT_attrs}= $self->_calc_FAT_attrs($dir);
   # Inject '.' and '..' entries, unless it is the root dir.
   if ($parent) {
      unshift @ents, {
         %$dir,
         uname => "$dir->{uname}/.",
         FAT_short11 => '.',
         FAT_shortname => '.',
         mode => S_IFDIR,
      }, {
         %$parent,
         uname => "$dir->{uname}/..",
         FAT_short11 => '..',
         FAT_shortname => '..',
         mode => S_IFDIR,
      };
   }
   else {
      # At the root, inject the volume label.
      my $label= $self->volume_label // 'NO NAME';
      croak "Invalid volume label '$label'"
         unless is_valid_volume_label($label);
      unshift @ents, {
         uname => "(volume label)",
         FAT_short11 => $label,
         mode => 0,
         FAT_attrs => ATTR_VOLUME_ID,
      };
   }
   $dir->{FAT_dirents}= \@ents;
   my $n= @ents;
   # Add LFN entries
   for (@ents) {
      if (defined $_->{FAT_longname} && $_->{FAT_longname} ne $_->{FAT_shortname}) {
         $n += int((length(encode('UTF-16LE', $_->{FAT_longname}, Encode::FB_CROAK)) + 25)/26);
      }
   }
   $log->debugf("%s",$dir);
   $log->debug("dir /$dir->{uname} packed with ".scalar(@ents)." real entries, $n with LFN entries")
      if $log->is_debug;
   croak "Directory /$dir->{uname} exceeds maximum entry count"
      if $n >= 65536;
   $dir->{size}= $n * 32;  # always 32 bytes per dirent
   # recursively finalize all subdirectories
   push @$dirlist, @dirs;
   $self->_finalize_dir($_, $dir, $dirlist, $filelist) for @dirs;
}

sub _optimize_geometry($self, $dirs, $files) {
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates, and also meting the needs of alignment requests
   my (@offset_files, @aligned_files, @other_files);
   push @{$_->{device_offset}? \@offset_files : $_->{device_align}? \@aligned_files : \@other_files}, $_
      for grep $_->{size}, @$files;
   $log->debugf("_optimize_geometry offset_files=%d aligned_files=%d other_files=%d",
      scalar @offset_files, scalar @aligned_files, scalar @other_files);
   my $min_ofs= min(map $_->{device_offset}, @offset_files);
   my $max_ofs= max(map $_->{device_offset} + $_->{size}, @offset_files);
   my $max_align= max(0, map $_->{device_align}, @aligned_files);
   my $root_dirent_used= $self->{_root}{size} / 32;
   isa_int $root_dirent_used && $root_dirent_used >= 1
      or die "BUG: root must always have one entry";
   my $bytes_per_sector= $self->bytes_per_sector;
   my %fail_reason;
   my $best;
   # If the user defined sectors_per_cluster, we only have one option.
   # Otherwise iterate through all of them to find the best.
   my @spc= defined $self->sectors_per_cluster? ( $self->sectors_per_cluster )
      : (1,2,4,8,16,32,64,128);
   cluster_size: for my $sectors_per_cluster (@spc) {
      my $cluster_size= $sectors_per_cluster * $bytes_per_sector;
      isa_pow2 $cluster_size or die "BUG: cluster_size not a power of 2";
      # Avoid triggering warning about incompatible cluster sizes if a good cluster size was
      # already found.
      last if $best && $cluster_size > 32*1024;
      # Count total sectors used by ->{size} of files and dirs.
      # Don't add root dir until we know it will be FAT32
      my $clusters= 0;
      for (@$dirs, @offset_files, @aligned_files, @other_files) {
         $clusters += ceil($_->{size} / $cluster_size);
      }
      $log->tracef("with sectors_per_cluster=%d, would require at least %d clusters", $sectors_per_cluster, $clusters)
         if $log->is_trace;
      $clusters ||= 1;
      my ($reserved, $root_clusters_added);
      # If file alignment is a larger power of 2 than cluster_size, then as long as data_start
      # is aligned to cluster_size there will be a cluster that can satisfy the alignment.
      # If file alignment is a smaller power of 2 than cluster_size, then as long as
      # data_start is aligned to the file alignment, every cluster can satisfy the alignment.
      my $align= min($cluster_size, $max_align);
      if ($align) {
         # But wait, does every device_offset meet this alignment?  If not, give up.
         for (@offset_files) {
            if ($_->{device_offset} & ($align-1)) {
               $fail_reason{$sectors_per_cluster}= "device_offset $_->{device_offset}"
                  ." of $_->{uname} conflicts with your alignment request of $align";
               next cluster_size;
            }
         }
      }
      elsif (@offset_files) {
         # If not aligning clusters to pow2, might need to align to device_offset.
         # First, every device_offset must have the same remainder modulo cluster_size.
         my ($remainder, $prev);
         for (@offset_files) {
            my $r= $_->{device_offset} & ($cluster_size-1);
            if (!defined $remainder) {
               $remainder= $r;
               $prev= $_;
            } elsif ($remainder != $r) {
               $fail_reason{$sectors_per_cluster}= "file $_->{uname} device_offset"
                  ." $_->{device_offset} modulo cluster_size $cluster_size conflicts with"
                  ." file $prev->{uname} device_offset $prev->{device_offset}";
               next cluster_size;
            }
         }
         if ($remainder) {
            $align= [ $cluster_size, $remainder ];
         } else {
            $align= $cluster_size;
         }
      }
      again_with_more_clusters: {
         # If this number of clusters pushes us into FAT32, also need to add the root directory
         # clusters to the count.
         if (!$root_clusters_added
            && $clusters > Sys::Export::VFAT::Geometry::FAT16_IDEAL_MAX_CLUSTERS()
         ) {
            $root_clusters_added= ceil($self->{_root}{size} / $cluster_size);
            $clusters += $root_clusters_added;
            $log->trace("reached FAT32 threshold, adding $root_clusters_added clusters for root dir")
               if $log->is_trace;
         }
         my $geom= Sys::Export::VFAT::Geometry->new(
            volume_offset          => $self->volume_offset,
            (align_clusters        => $align)x!!$align,
            bytes_per_sector       => $bytes_per_sector,
            sectors_per_cluster    => $sectors_per_cluster,
            fat_count              => $self->fat_count,
            cluster_count          => $clusters,
            used_root_dirent_count => $root_dirent_used,
         );
         $log->debugf("testing clusters=%d size=0x%X data_region=0x%X-%X min_ofs=0x%X max_ofs=0x%X",
            $clusters, $cluster_size, $geom->data_start_device_offset, $geom->data_limit_device_offset,
            $min_ofs, $max_ofs)
            if $log->is_debug;
         if (@offset_files || @aligned_files) {
            # tables are too large? Try again with larger clusters.
            if (defined $min_ofs && $min_ofs < $geom->data_start_device_offset) {
               $fail_reason{$sectors_per_cluster}= "FAT tables too large for requested device_offset $min_ofs";
               next cluster_size;
            }
            # Not enough clusters?  Try again with more.
            if (defined $max_ofs && $max_ofs > $geom->data_limit_device_offset) {
               # This might overshoot a bit since the tables also grow and push forward the
               # whole data area.
               $clusters= ceil(($max_ofs - $geom->data_start_device_offset) / $cluster_size);
               goto again_with_more_clusters;
            }
         }
         # Now verify we have enough clusters by actually alocating them
         my $alloc= Sys::Export::VFAT::AllocationTable->new;
         my %file_cluster;
         unless (eval {
            for my $f (@offset_files, @aligned_files, @other_files, @$dirs,
               ($geom->bits == FAT32? ($self->{_root}) : ())
            ) {
               my $cluster_ref= ($f->{FAT_cluster} //= [ undef ]);
               $file_cluster{refaddr $cluster_ref} //= [ $self->_alloc_file($geom, $alloc, $f), $cluster_ref ];
            }
            1
         }) {
            chomp($fail_reason{$sectors_per_cluster}= "$@");
            next cluster_size;
         }
         if ($alloc->max_used_cluster_id > $geom->max_cluster_id) {
            $clusters= $alloc->max_used_cluster_id-1;
            goto again_with_more_clusters;
         }
         # Allocation worked, so clamp the allocator to this nmber of sectors
         $alloc->max_cluster_id($geom->max_cluster_id);
         # Is this the smallest option so far?
         if (!$best || $best->{geom}->total_sector_count > $geom->total_sector_count) {
            $best= { geom => $geom, alloc => $alloc, file_cluster => \%file_cluster };
         }
      }
   } continue {
      $log->tracef("%s", $fail_reason{$sectors_per_cluster}) if $log->is_trace;
   }
   if (!$best) {
      $self->{_optimize_geometry_fail_reason}= \%fail_reason;
      return;
   }
   # Apply file cluster IDs to the file->{FAT_cluster} attributes
   $self->_resolve_cluster($_->[1], $_->[0])
      for values %{ $best->{file_cluster} };
   return @{$best}{'geom','alloc'};
}

# This returns a valid 8.3 filename which doesn't conflict with any of the keys in %$existing
sub _coerce_to_83_name($name, $fc_existing) {
   length $name or die "BUG";
   $name= uc $name;
   my ($base, $ext)= ($name =~ /^(.*?)(\.[^\.]*|)\z/) # capture final extension and everything else
      or die "BUG"; # should always match
   for ($base, $ext) {
      # Replace every run of invalid chars with a single '_'
      s/[^\x21\x23-\x29\x2D.\x30-\x39\x40-\x5A\x5E-\x7B\x7D-\xE4\xE6-\xFF]+/_/g;
      # Now that all high characters have been removed, consider these to be bytes
      utf8::downgrade($_);
   }
   $base= '~1' if !length $base; # a guess at how to handle ".foo", ->"~1.FOO" ?
   substr($base, 6)= '~1' if length $base > 8;
   substr($ext, 3)= '' if length $ext > 3;
   my $suffix= !length $ext? '' : length $ext > 3? '.'.substr($ext,0,3) : ".$ext";
   while ($fc_existing->{fc $base.$suffix}) {
      if ($base =~ /^(.*?)~([0-9]+)\z/) {
         my $next= $2 + 1;
         croak "Can't find available ~N suffix for '$name'" if length($next) > 6;
         $base= substr($1, 0, 7-length($next)) . '~' . $next;
      } else {
         $base= substr($base, 0, 6) . '~1'
      }
   }
   return $base.$suffix;
}

sub _calc_FAT_attrs($self, $ent) {
   # did user supply them?
   my $flags= $ent->{FAT_attrs} // do {
      # readonly determined by user -write bit of 'mode'
      ((defined $ent->{mode} && !($ent->{mode} & 0400))? ATTR_READONLY : 0)
      # hidden determined by leading '.' in filename
      | ($ent->{FAT_longname} =~ /^\./? ATTR_HIDDEN : 0)
   };
   # Is it a directory?
   $flags |= ATTR_DIRECTORY if S_ISDIR($ent->{mode} // 0);
   return $flags;
}

sub _epoch_to_fat_date_time($epoch) {
   my @lt = localtime($epoch);
   my $year = $lt[5] + 1900;
   my $mon  = $lt[4] + 1;
   my $mday = $lt[3];
   my $hour = $lt[2];
   my $min  = $lt[1];
   my $sec  = int($lt[0] / 2); # 2-second resolution

   $year = 1980 if $year < 1980;
   my $fat_date = (($year - 1980) << 9) | ($mon << 5) | $mday;
   my $fat_time = ($hour << 11) | ($min << 5) | $sec;
   my $fat_frac = ($epoch * 100) % 200;
   return ($fat_date, $fat_time, $fat_frac);
}

sub _append_dirent($self, $dir, $file) {
   my $short11= $file->{FAT_short11} // die "BUG: FAT_short11 not set";
   $short11 =~ s/^\xE5/\x05/; # \xE5 may occur in some charsets, and needs escaped
   my $attrs= $self->_calc_FAT_attrs($file);
   # Need Long-File-Name entries?
   if (defined $file->{FAT_longname} && $file->{FAT_shortname} ne $file->{FAT_longname}) {
      # Checksum for directory shortname, used to verify long name parts
      my $cksum= 0;
      $cksum= ((($cksum >> 1) | ($cksum << 7)) + $_) & 0xFF
         for unpack 'C*', $short11;
      # Each dirent holds up to 26 bytes (13 chars) of the long name
      my @chars= unpack 'v*', encode('UTF-16LE', $file->{FAT_longname}, Encode::FB_CROAK);
      # short final chunk is padded with \0\uFFFF*
      if (my $remainder= @chars % 13) {
         push @chars, 0;
         push @chars, (0xFFFF)x(12 - $remainder);
      }
      my $last= @chars/13 - 1;
      for my $i (reverse 0..$last) {
         my $ofs= $i*13;
         my $seq= ($i + 1) | (($i == $last) ? 0x40 : 0x00);
         $file->{data} .= pack('C v5 C C C v6 v v2',
            $seq,                      # sequence and end-flag
            @chars[$ofs .. $ofs+4],    # first 5 chars
            0x0F, 0x00, $cksum,        # attr = LFN, type = 0
            @chars[$ofs+5 .. $ofs+10], # next 6 chars
            0,                         # no cluster number
            @chars[$ofs+11 .. $ofs+12] # last 2 chars
         );
      }
   }
   # encode final short-name entry
   $file->{mtime} //= time;
   $file->{btime} //= $file->{mtime}; # creation "born" time
   $file->{atime} //= $file->{mtime};
   my ($cdate, $ctime, $ctime_frac)= _epoch_to_fat_date_time($file->{btime});
   my ($wdate, $wtime)             = _epoch_to_fat_date_time($file->{mtime});
   my ($adate)                     = _epoch_to_fat_date_time($file->{atime});
   my $cluster= 0;
   # References to the root dir are always encoded as cluster zero, even on FAT32
   # where the root dir actually lives at a nonzero cluster.
   # Volume label also doesn't need a cluster id.  Nor do empty files.
   if ($file->{size} && $file->{FAT_shortname} ne '') {
      my $cluster_ref= ($file->{FAT_cluster} ||= [ undef ]);
      if (defined $cluster_ref->[0]) {
         $cluster= $cluster_ref->[0];
      } else {
         # If we need to encode a cluster which isn't known, append a function to the
         # FAT_cluster which can later be called when the cluster is known, to repack
         # the directory entry.
         my $data_ref= \$dir->{data}; # make sure not to close over '$dir'
         my $ent_ofs= length($$data_ref);
         push @$cluster_ref, sub ($resolved_cluster) {
            substr($$data_ref, $ent_ofs+20, 2, pack 'v', ($resolved_cluster>>16));
            substr($$data_ref, $ent_ofs+26, 2, pack 'v', $resolved_cluster);
         };
      }
   }
   # Directories always have a size of 0
   my $size= S_ISDIR($file->{mode})? 0 : $file->{size} // 0;
   $dir->{data} .= pack('a11 C C C v v v v v v v V',
      $short11, $attrs, 0, #NT_reserved
      $ctime_frac, $ctime, $cdate, $adate,
      $cluster >> 16, $wtime, $wdate, $cluster, $size);
}

sub _pack_dir($self, $dir) {
   my $ents= $dir->{FAT_dirents} // croak "Dir not finalized";
   $dir->{data}= '';
   $self->_append_dirent($dir, $_) for @$ents;
   die "BUG: calculated dir size $dir->{size} != data length ".length($dir->{data})
      unless $dir->{size} == length $dir->{data};
   # Dir must be padded to length of sector/cluster with entries whose name begins with \x00
   # but that will happen automatically later as the data is appended to the file..
}

sub _alloc_file($self, $geom, $alloc, $file) {
   my $sz= $file->{size} or do { carp "Attempt to allocate zero-length file"; return };
   my $cl_count= POSIX::ceil($sz / $geom->bytes_per_cluster);
   if ($file->{device_offset}) {
      my ($cl, $n)= $geom->get_cluster_extent_of_device_extent($file->{device_offset}, $sz);
      return $alloc->alloc_range($cl, $cl_count)
         // croak "Can't allocate $cl_count clusters from offset $file->{device_offset}";
   } elsif ($file->{device_align}) {
      my ($mul, $ofs)= $geom->get_cluster_alignment_of_device_alignment($file->{device_align});
      return $alloc->alloc_contiguous($cl_count, $mul, $ofs)
         // croak "Can't allocate $cl_count clusters aligned to $file->{device_align}";
   } else {
      return $alloc->alloc($cl_count)
         // croak "Can't allocate $cl_count clusters";
   }
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
   [ BPB_Reserved   =>   52, 12, 'a12', '' ],
   [ BS_DrvNum      =>   64,  1, 'C', 0x80 ],
   [ BS_Reserved1   =>   65,  1, 'C', 0 ],
   [ BS_BootSig     =>   66,  1, 'C', 0x29 ],
   [ BS_VolID       =>   67,  4, 'V' ],
   [ BS_VolLab      =>   71, 11, 'A11', 'NO NAME' ],
   [ BS_FilSysType  =>   82,  8, 'A8' ],
   [ _signature     =>  510,  2, 'v', 0xAA55 ],
);
our @fat32_fsinfo_fields= (
   [ FSI_LeadSig    =>    0,  4, 'V', 0x41615252 ],
   [ FSI_Reserved1  =>    4,480, 'a480', '' ],
   [ FSI_StrucSig   =>  484,  4, 'V', 0x61417272 ],
   [ FSI_Free_Count =>  488,  4, 'V', 0xFFFFFFFF ],
   [ FSI_Nxt_Free   =>  492,  4, 'V', 0xFFFFFFFF ],
   [ FSI_Reserved2  =>  496, 12, 'a12', '' ],
   [ FSI_TrailSig   =>  508,  4, 'V', 0xAA550000 ],
);
sub _append_pack_args($pack, $vals, $ofs, $fields, $attrs) {
   for (@$fields) {
      push @$pack, '@'.($ofs+$_->[1]).$_->[3];
      push @$vals, $attrs->{$_->[0]} // $_->[4]
         // croak "No value supplied for $_->[0], and no default";
   }
}

sub _pack_reserved_sectors($self, %attrs) {
   my (@pack, @vals);
   my $geom= $self->geometry;
   $attrs{BPB_BytsPerSec}= $geom->bytes_per_sector;
   $attrs{BPB_SecPerClus}= $geom->sectors_per_cluster;
   $attrs{BPB_RsvdSecCnt}= $geom->reserved_sector_count;
   $attrs{BPB_NumFATs}=    $geom->fat_count;
   $attrs{BPB_RootEntCnt}= $geom->root_dirent_count;
   $attrs{BS_VolLab} //= $self->volume_label;
   $attrs{BS_VolID} //= time & 0xFFFFFFFF;
   if ($geom->bits < FAT32) {
      $attrs{BPB_FATSz16}= $geom->fat_sector_count < 0x10000? $geom->fat_sector_count : 0;
      $attrs{BPB_FATSz32}= $geom->fat_sector_count < 0x10000? 0 : $geom->fat_sector_count;
      $attrs{BPB_TotSec16}= $geom->total_sector_count < 0x10000? $geom->total_sector_count : 0;
      $attrs{BPB_TotSec32}= $geom->total_sector_count < 0x10000? 0 : $geom->total_sector_count;
      $attrs{BS_FilSysType}= $geom->bits == 12? 'FAT12' : 'FAT16';
      _append_pack_args(\@pack, \@vals, 0, \@sector0_fat16_fields, \%attrs);
   } else {
      # Did the user specify location of fsinfo?  If not, default to sector 1
      $attrs{BPB_FSInfo} //= 1;
      $attrs{BPB_FATSz16}= 0;
      $attrs{BPB_FATSz32}= $geom->fat_sector_count;
      $attrs{BPB_TotSec16}= 0;
      $attrs{BPB_TotSec32}= $geom->total_sector_count;
      $attrs{BS_FilSysType}= "FAT";
      _append_pack_args(\@pack, \@vals, 0, \@sector0_fat32_fields, \%attrs);
      
      # FSInfo struct, location is configurable
      my $fsi_ofs= $attrs{BPB_FSInfo} * $attrs{BPB_BytsPerSec};
      $attrs{FSI_Free_count} //= $self->allocation_table->free_cluster_count;
      $attrs{FSI_Nxt_Free}   //= $self->allocation_table->first_free_cluster;
      _append_pack_args(\@pack, \@vals, $fsi_ofs, \@fat32_fsinfo_fields, \%attrs);

      # Backup copy of boot sector, not required.
      if ($attrs{BPB_BkBootSec}) {
         my $bk_ofs= $attrs{BPB_BkBootSec} * $attrs{BPB_BytsPerSec};
         _append_pack_args(\@pack, \@vals, $bk_ofs, \@sector0_fat32_fields, \%attrs);
         _append_pack_args(\@pack, \@vals, $bk_ofs+$fsi_ofs, \@fat32_fsinfo_fields, \%attrs);
      }
   }
   pack join(' ', @pack), @vals;
}

1;
