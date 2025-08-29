package Sys::Export::VFAT;
# ABSTRACT: Write CPIO archives needed for Linux initrd
# VERSION

use v5.26;
use warnings;
use experimental qw( signatures );
use Fcntl qw( S_IFDIR S_ISDIR );
use Scalar::Util qw( blessed dualvar );
use POSIX 'ceil';
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
require Sys::Export::VFAT::Geometry;

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

   my $self= bless {
      _root => {
         mode => S_IFDIR,
         FAT_shortname => '',
         FAT_longname  => '',
         FAT_is_auto_dir => 1,
      }
   }, $class;
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
  #   uname              => $path_unicode_string,
  #   FAT_shortname      => "8CHARATR.EXT",
  #   FAT_longname       => $basename_characters,
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

=item FAT_flags

An ORed combination of L</ATTR_READONLY>, L</ATTR_HIDDEN>, L</ATTR_SYSTEM>, or L</ATTR_ARCHIVE>.
If this value is defined, this module will *not* use the C<mode> user-write bit to determine
C<ATTR_READONLY> and will not use leading "." to determine C<ATTR_HIDDEN>.

=item FAT_offset

For integration with ISOHybrid, you may specify C<FAT_offset> (bytes, from start of filesystem)
to request the file be placed at an exact location.  This must fall on the address of one of
the clusters of the data region, and will generate an exception if it can't be honored.
Unfortunately you won't get that exception until L</finish> is called.

=back

=cut

sub add($self, $file) {
   # If user supplied FAT_utf16_name, use that as a more official source of Unicode
   my $uname= $file->{uname} // decode('UTF-8', $file->{name}, Encode::FB_CROAK);
   $uname =~ s,^/,,; # remove leading slash
   _validate_name($uname);
   my @path= grep length, split '/', $uname;
   my $leaf= pop @path;
   my $auto_dir= sub($name) {  };
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
   # Validate FAT_shortname
   if (length $ent->{FAT_shortname}) {
      is_valid_83_name($ent->{FAT_shortname})
         or croak "Invalid FAT_shortname '$ent->{FAT_shortname}' for '$uname'";
   }
   # If the normal name is also a valid shortname, assign that
   elsif (_is_valid_83_name($leaf)) {
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
   # Sanity check FAT_offset before we get too far along
   if (defined $ent->{FAT_offset}) {
      # The smallest conceivable address where the data region could start
      state $minimum_offset_to_data= Sys::Export::VFAT::Geometry->new(
            bytes_per_sector => 512,
            sectors_per_cluster => 1,
            fat_count => 1,
            root_dirent_count => 1,
            cluster_count => 1
         )->data_start_offset;

      # must fall in the data area
      $ent->{FAT_offset} > $minimum_offset_to_data
      # must be a multiple of at least 512 (probably more)
      && !($ent->{FAT_offset} & 511)
         or croak "Invalid FAT_offset '$ent->{FAT_offset}' for file '$uname'";
   }
   if (defined $ent->{FAT_align}) {
      # must be a power of 2
      _round_up_pow2($ent->{FAT_align}-1) == $ent->{FAT_align}
         or croak "Invalid FAT_align $ent->{FAT_align} for '$uname', must be a power of 2";
   }
   $self;
}

sub _resolve_cluster($file, $cl_id) {
   my $cluster_ref= $file->{FAT_cluster};
   $cluster_ref->[0]= $cl_id;
   $_->($cl_id) for @{$cluster_ref}[1..$#$cluster_ref];
}

sub _write_clusters {
   my ($fh, $geom, $alloc_invlist)= (shift, shift, shift);
   my $data_ofs= 0;
   for (my $i= 0; $i < @$alloc_invlist; $i += 2) {
      my ($cl_start, $cl_lim)= @{$alloc_invlist}[$i, $i+1];
      sysseek($fh, $geom->get_cluster_offset($cl_start), 0)
         or croak "sysseek: $!";
      my $size= ($cl_lim-$cl_start) * $geom->bytes_per_cluster;
      $size= length($_[0]) - $data_ofs
         if length($_[0]) - $data_ofs < $size;
      my $wrote= syswrite($fh, $_[0], $size, $data_ofs);
      croak "syswrite: $!" if !defined $wrote;
      croak "Unexpected short write" if $wrote != $size;
      $data_ofs += $wrote;
   }
}

sub finish($self) {
   # Build a list of all directories and files
   my (@dirs, @files, @dir_todo);
   my $root= $self->{_root};
   # sanity check, since this acts as an important flag
   $root->{FAT_longname} eq '' && $root->{FAT_shortname} eq ''
      or croak "Root dir has wrong name";
   # Find out the size of every directory
   $self->_finalize_dir($root, undef, \@dirs, \@files);
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates
   my $geom= $self->_optimize_geometry(\@dirs, \@files)
      or croak "No geometry options can meet your FAT_offset / FAT_align requests";
   my $cluster_size= $geom->bytes_per_cluster;
   my $fh= File::Temp->new;
   truncate($fh, $geom->total_size)
      or croak "truncate: $!";
   # Begin assigning files and directories to clusters
   my $alloc= Sys::Export::VFAT::Allocator->new($geom->max_cluster_idx);
   for my $f (grep $_->{FAT_offset}, @files) {
      my $alloc_invlist= $alloc->alloc_byte_range($f->{FAT_offset}, $f->{size}, $geom)
         or croak "Failed to allocate FAT_offset = $f->{FAT_offset}";
      _resolve_cluster($f, $alloc_invlist->[0]);
      _write_clusters($fh, $geom, $alloc_invlist, $f->{data});
   }
   for my $f (grep $_->{FAT_align} && !$_->{FAT_cluster}[0], @files) {
      my $alloc_invlist= $alloc->alloc_byte_contiguous($_->{size}, $_->{FAT_align}, $geom)
         or croak "BUG: not enough contiguous clusters for aligned file $_->{uname}";
      _resolve_cluster($f, $alloc_invlist->[0]);
      _write_clusters($fh, $geom, $alloc_invlist, $f->{data});
   }
   for my $f (grep !$_->{FAT_cluster}[0], @files, @dirs) {
      my $alloc_invlist= $alloc->alloc(ceil($_->{size}/$cluster_size))
         or croak "BUG: not enough clusters for $_->{uname}";
      _resolve_cluster($f, $alloc_invlist->[0]);
      _write_clusters($fh, $geom, $alloc_invlist, $f->{data});
   }
   
   # Encode the boot sector
   ...;
   # If fat32, encode the extra structs
   ...;
   # Blank the rest of the reserved sectors, if any requested
   ...;
   # If fat12/16, encode the root dir
   ...;
   # Pack the FAT then store each copy
   my $fat;
   if ($geom->bits == FAT12) {
      ...
   } else {
      $fat= pack(($geom->bits == FAT16? 'v*':'V*'), @$fat);
   }
   for my $i (0..($geom->fat_count-1)) {
      sysseek($fh, $geom->get_fat_offset($i, 0), 0);
      syswrite($fh, $fat);
   }
   ...
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
   my @ents= $_->{FAT_by_fc_name}? values $_->{FAT_by_fc_name}->%* : ();
   my @dirs;
   # Need the 8.3 name in order to know whether it matches the long name
   for (@ents) {
      # Is the 8.3 name decided already?
      $_->{FAT_shortname} //= _coerce_to_83_name($_->{FAT_longname}, $dir->{FAT_by_shortname});
      $dir->{FAT_by_shortname}{$_->{FAT_shortname}}= $_;
      my ($name, $ext)= split /\./, $_->{FAT_shortname};
      $_->{FAT_short11}= pack 'A8 A3', $name, ($ext//'');
      $_->{FAT_short11} =~ s/^\xE5/\x05/; # \xE5 may occur in some charsets, and needs escaped
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
   $dir->{FAT_flags}= $self->_calc_FAT_flags($dir);
   # Inject '.' and '..' entries, unless it is the root dir.
   if ($parent) {
      unshift @ents, {
         %$dir,
         mode => S_IFDIR,
         FAT_short11 => '.',
      }, {
         %$parent,
         mode => S_IFDIR,
         FAT_short11 => '..',
      };
   }
   else {
      # At the root, inject the volume label.
      my $label_11= _coerce_to_83_name($self->volume_label, {});
      unshift @ents, { FAT_short11 => $label_11, FAT_flags => ATTR_VOLUME_ID };
   }
   $dir->{FAT_dirents}= \@ents;
   my $n= @ents;
   # Add LFN entries
   for (@ents) {
      if (defined $_->{FAT_longname} && $_->{FAT_longname} ne $_->{FAT_shortname}) {
         $n += int((length(encode('UTF-16LE', $_->{FAT_longname}, Encode::FB_CROAK)) + 25)/26);
      }
   }
   croak "Directory /$dir->{uname} exceeds maximum entry count"
      if $n >= 65536;
   $dir->{size}= $n * 32;  # always 32 bytes per dirent
   # recursively finalize all subdirectories
   push @$dirlist, @dirs;
   $self->_finalize_dir($_, $dir, $dirlist, $filelist) for @dirs;
}

sub _optimize_geometry($self, $files, $dirs) {
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates, and also meting the needs of $placement_requests
   my (@offset_files, @aligned_files, @other_files);
   push @{$_->{FAT_offset}? \@offset_files : $_->{FAT_align}? \@aligned_files : \@other_files}, $_
      for @$files;
   my ($min_ofs, $max_ofs);
   for (@offset_files) {
      $min_ofs= $_->{FAT_offset}
         if !defined $min_ofs || $_->{FAT_offset} < $min_ofs;
      $max_ofs= $_->{FAT_offset} + $_->{size}
         if !defined $max_ofs || $_->{FAT_offset} + $_->{size} > $max_ofs;
   }
   my $geom;
   my $bytes_per_sector= 512;
   for my $sectors_per_cluster (1,2,4,8,16,32,64,128) {
      my $cluster_size= $sectors_per_cluster * $bytes_per_sector;
      my $clusters= 0;
      for (@$dirs, @$files) {
         $clusters += int($_->{size} / $cluster_size);
         $clusters++ if $_->{size} % $cluster_size;
      }
      my $reserved;
      retry: {
         my $trial_geom= Sys::Export::VFAT::Geometry->new(
            bytes_per_sector       => $bytes_per_sector,
            sectors_per_cluster    => $sectors_per_cluster,
            fat_count              => 1,
            cluster_count          => $clusters,
            used_root_dirent_count => $self->{_root}{size} / 32,
            (defined $reserved? (reserved_sector_count => $reserved) : ()),
         );
         if (@offset_files || @aligned_files) {
            # tables are too large? Try again with larger clusters.
            next if defined $min_ofs && $min_ofs < $trial_geom->data_offset;
            # Can this meet all the alignment needs?
            # If file alignment is a larger power of 2 than cluster_size, then as long as data_start
            # is aligned to cluster_size there will be a cluster that can satisfy the alignment.
            # If file alignment is a smaller power of 2 than the cluster size, then as long as
            # data_start is aligned to the file alignment, every cluster can satisfy the alignment.
            my $align;
            for (@aligned_files) {
               my $need_align= $_->{FAT_align} > $cluster_size? $cluster_size : $_->{FAT_align};
               $align= $need_align if !defined $align || $align < $need_align;
            }
            if (defined $align) {
               # But wait, does every FAT_offset meet this alignment?  If not, give up.
               next if grep $_->{FAT_offset} & ($align-1), @offset_files;
               if (my $ofs= $trial_geom->data_offset & ($align-1)) {
                  # Need to add reserved sectors to shift data_offset to a multiple of $align
                  $reserved= $trial_geom->reserved_sector_count + $align - $ofs;
                  goto retry;
               }
            }
            # If not aligning clusters to pow2, might need to align to FAT_offset
            else {
               for (@offset_files) {
                  if (my $ofs= ($_->{FAT_offset} - $trial_geom->data_offset) & ($cluster_size-1)) {
                     croak "FAT_offset of $_->{uname} is not a multiple of $bytes_per_sector"
                        if $ofs & ($bytes_per_sector-1);
                     # Shift the start of sectors to meet this offset
                     $reserved= $trial_geom->reserved_sector_count + ($ofs / $bytes_per_sector);
                     goto retry;
                  }
               }
            }
            # Not enough clusters?  Try again with more.
            if (defined $max_ofs && $max_ofs > $trial_geom->total_size) {
               # This might overshoot a bit since the tables also grow and push forward the
               # whole data area.
               $clusters= ceil(($max_ofs - $trial_geom->data_offset) / $cluster_size);
               $reserved= undef;
               goto retry;
            }
            # Now double-check we have enough clusters by actually alocating them
            my $alloc= Sys::Export::VFAT::Allocator->new();
            $alloc->alloc_byte_range($_->{FAT_offset}, $_->{size}, $trial_geom) || next
               for @offset_files;
            $alloc->alloc_byte_contiguous($_->{size}, $_->{align}, $trial_geom) || next
               for @aligned_files;
            $alloc->alloc(ceil($_->{size} / $trial_geom->bytes_per_cluster)) || next
               for @other_files, @$dirs;
            if ($alloc->max_cluster_used > $trial_geom->max_cluster_idx) {
               $clusters= $alloc->max_cluster_used;
               goto retry;
            }
         }
         $geom= $trial_geom
            if !$geom || $geom->total_sector_count > $trial_geom->total_sector_count;
      }
   }
   return $geom;
}

sub _is_valid_name($name) {
   # characters permitted for LFN are all letters numbers and $%'-_@~`!(){}^#&+,;=[]
   # and space and all codepoints above 0x7F.
   # they may not begin with space, and cannot exceed 255 chars.
   $name !~ /^ /
   && $name =~ /^
      [\x20\x21\x23-\x29\x2B\x2C\x2D\x30-\x39\x3B\x3D\x40-\x5B\x5D-\x7B\x7D-\xE4\xE6-]{1,255}
      \z/x;
}
sub _is_valid_83_name($name) {
   $name eq uc $name
   && $name =~ /^
      [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{1,8}
      ( \. [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,3} )
      \z/x;
}

# This returns a valid 8.3 filename which doesn't conflict with any of the keys in %$existing
sub _coerce_to_83_name($name, $fc_existing) {
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

# FAT_shortname
# FAT_longname
# FAT_flags
# FAT_offset
# FAT_cluster
sub _calc_FAT_flags($self, $ent) {
   # did user supply them?
   my $flags= $ent->{FAT_flags} // do {
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
   my $attrs= $self->_calc_FAT_flags($file);
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
   $file->{mtime}  //= time;
   $file->{crtime} //= $file->{mtime}; # "creation time", not "change time" ctime of UNIX
   $file->{atime}  //= $file->{mtime};
   my ($cdate, $ctime, $ctime_frac)= _epoch_to_fat_date_time($file->{crtime});
   my ($wdate, $wtime)             = _epoch_to_fat_date_time($file->{mtime});
   my ($adate)                     = _epoch_to_fat_date_time($file->{atime});
   my $cluster_ref= ($file->{FAT_cluster} ||= [ undef ]);
   my $cluster= $cluster_ref->[0];
   # References to the root dir are always encoded as cluster zero, even on FAT32
   # where the root dir actually lives at a nonzero cluster
   if ($file->{FAT_shortname} eq '') {
      $cluster= 0;
   }
   elsif (!defined $cluster) {
      # If we need to encode a cluster which isn't known, append a function to the
      # FAT_cluster which can later be called when the cluster is known, to repack
      # the directory entry.
      my $data_ref= \$dir->{data}; # make sure not to close over '$dir'
      my $ent_ofs= length($$data_ref);
      push @$cluster_ref, sub ($resolved_cluster) {
         substr($$data_ref, $ent_ofs+20, 2, pack 'v', ($resolved_cluster>>16));
         substr($$data_ref, $ent_ofs+26, 2, pack 'v', $resolved_cluster);
      };
      $cluster= 0;
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

# Element 0 of the FAT is used for an inversion list of which sectors are allocated.
# It's not as good as a tree, but should perform well when the typical use case is
# to pack files end to end without fragmentation.
package Sys::Export::VFAT::Allocator {
   use v5.26;
   use warnings;
   use experimental qw( signatures );
   use Carp;
   sub new($class, $max_cluster=undef) {
      my (@fat, @free);
      @free= (2); # first usable cluster is always 2
      # This object can be used open-ended for sizing up the table,
      # or with an end-cluster for the packing the final copy
      if ($max_cluster) {
         push @free, $max_cluster+1;
         $#fat= $max_cluster;
      }
      bless { free => \@free, fat => \@fat }, $class;
   }
   # Allocate $count clusters from anywhere and return an inversion list describing
   # the sectors allocated.
   sub alloc($self, $count) {
      my $invlist= $self->{free};
      # If there is enough free sectors, this basically just chops some entries
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
            $self->{fat}[$prev]= 0x0FFFFFF8;
            return \@result;
         }
         $count -= $n;
      }
      return; # not enough available
   }
   # Allocate a specific range of clusters, return empty list if they aren't available
   sub alloc_range($self, $cluster_id, $count) {
      $count >= 0 or die "BUG: alloc_range count must be positive";
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
   }
   # Allocate any contiguous span of clusters, optionally restricted to
   # being aligned to some power-of-two byte offset, according to a Geometry.
   sub alloc_contiguous($self, $count, $align=1, $align_ofs=0) {
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
      $self->{fat}[$cl_lim-1]= 0x0FFFFFF8;
      # Return an allocation inversion list of one segment
      return [$cl_start, $cl_lim];
   }
}

1;
