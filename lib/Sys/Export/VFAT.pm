package Sys::Export::VFAT;
# ABSTRACT: Write minimal FAT12/16/32 filesystems with control over stored file extents
# VERSION

use v5.26;
use warnings;
use experimental qw( signatures );
use Fcntl qw( S_IFDIR S_ISDIR S_ISREG SEEK_SET SEEK_END );
use Scalar::Util qw( blessed dualvar refaddr weaken );
use List::Util qw( min max );
use POSIX 'ceil';
use Sys::Export::LogAny '$log';
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

=head1 SYNOPSIS

  my $dst= Sys::Export::VFAT->new(
    filename => $path,
    volume_label => 'ESP'
    volume_offset => 2048*512, # inform VFAT of your partition layout
  );
  # Basic files and directories
  $dst->add([ file => "README.TXT", "Hello World\r\n" ]);
  $dst->add([ file => 'EFI/BOOT/BOOTIA32.EFI', { data_path => $loader }]);
  
  # Request file 'initrd' have its bytes stored at exactly disk offset 0x110000
  $dst->add([ file => 'initrd', { data_path => $initrd, device_offset => 0x110000 }]);
  
  # Request file 'vmlinuz' have its bytes stored aligned to 2048 disk address,
  #  and capture the location that was chosen into $kernel_ofs
  $dst->add(
    name => 'vmlinuz',
    mode => S_IFREG, # stat constant
    data_path => $path_to_kernel,
    device_align => 2048,
    device_offset => \my $kernel_ofs,
  );
  
  $dst->finish;

=head1 DESCRIPTION

This module can be used as an export destination to build a FAT32/16/12 filesystem by directly
encoding your files into a very compact VFAT layout.  The generated filesystem has no
fragmentation and no free space (unless you specified device_offset/device_align in a way that
created "holes" in your cluster array).

This implementation caches all files in memory, and then chooses FAT parameters that result in
the smallest image.

This implementation also has some fun features intended to work together with the
L<ISOHybrid|Sys::Export::ISOHybrid> module, which can (on the assumption that the filesystem will
never be written) encode hardlinks, encode symlinks as hard-linked directories, and place files
at specific offsets within the generated image.

=head2 FAT Geometry

This was complicated enough it became its own module.  See L<Sys::Export::VFAT::Geometry>.

=head2 Algorithm

This module chooses the optimal cluster size and count for the files you provide.
That choice affects the starting offset of the first cluster, so this module buffers all
directories into memory until the decisions are made, and then writes the whole filesystem in
one pass during L</finish>.

The cluster size/count are chosen by scanning over all the files and directories you have added
and total up the number of clusters required, under each of the possible cluster sizes.  It also
adds in the size of the FAT, which is based on the cluster count.  It also calculates a complete
assignment of files to clusters, to verify it can meet requests for device_offset or
device_align.  It then selects whichever successful configuration had the smallest overall size.
The calculated cluster assignment is then applied to the files and directory entries, and then
the directories get encoded now that the cluster refs are resolved.  Finally, each component
of the filesystem is written to the destination filename or filehandle.

=constructor new

  $fat= Sys::Export::VFAT->new($filename_or_handle);
  $fat= Sys::Export::VFAT->new(%attrs);
  $fat= Sys::Export::VFAT->new(\%attrs);

This takes a list of attributes as a hashref or key/value list.  If there is exactly one
argument, it is treated as the filename attribute.

=cut

sub new($class, @attrs) {
   my %attrs= @attrs != 1? @attrs
            : isa_hash $attrs[0]? %{$attrs[0]}
            : isa_handle $attrs[0]? ( filehandle => $attrs[0] )
            : ( filename => $attrs[0] );
   my $self= bless {}, $class;
   $self->{_root}= $self->_new_dir('(root)', undef);
   # keep root dir separate from subdirs
   delete $self->{_subdirs}{refaddr $self->{_root}};
   # apply other attributes
   $self->$_($attrs{$_}) for keys %attrs;
   $self;
}

=attribute filename

Name of file (or device) to write.  If the file exists it will be truncated before writing.
If you want to write the filesystem amid existing data (like a partition table0, pass a file
handle as C<out_fh>.

=attribute filehandle

Output filehandle to write.  The file will be enlarged if it is not big enough.

=cut

sub _root { $_[0]{_root} }
sub filename { @_ > 1? ($_[0]{filename}= $_[1]) : $_[0]{filename} }
sub filehandle { @_ > 1? ($_[0]{filehandle}= $_[1]) : $_[0]{filehandle} }

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

# The smallest conceivable address where the data region could start
sub _minimum_offset_to_data {
   state $minimum_offset_to_data= Sys::Export::VFAT::Geometry->new(
      bytes_per_sector => 512,
      sectors_per_cluster => 1,
      fat_count => 1,
      root_dirent_count => 1,
      cluster_count => 1
   )->data_start_offset;
}

# clusterdata represents the data that needs to be written to a yet-to-be-determined
# cluster chain.  Multiple dirents may refer to the same clusterdata, such as the ".."
# and "." entries, or if hardlinks are enabled.
sub _new_clusterdata($self, $name, $size, %fields) {
   my $cdata= bless {
      name => $name,
      size => $size,
      callbacks => [],
   }, 'Sys::Export::VFAT::_ClusterData';
   for (qw( align offset cluster data_ref data_path )) {
      $cdata->{$_}= $fields{$_} if defined $fields{$_};
   }
   $cdata;
}
sub Sys::Export::VFAT::_ClusterData::name      { $_[0]{name} }
sub Sys::Export::VFAT::_ClusterData::size      { $_[0]{size} }
sub Sys::Export::VFAT::_ClusterData::data_ref  { $_[0]{data_ref} }
sub Sys::Export::VFAT::_ClusterData::data_path { $_[0]{data_path} }
sub Sys::Export::VFAT::_ClusterData::align     { $_[0]{align} }
sub Sys::Export::VFAT::_ClusterData::offset    { $_[0]{offset} }
sub Sys::Export::VFAT::_ClusterData::cluster   { $_[0]{cluster} }
# register a callback to execute when the cluster becomes known
sub Sys::Export::VFAT::_ClusterData::on_cluster_change($self, $cb) {
   push $self->{callbacks}->@*, $cb;
   $cb->($self, $self->{cluster}) if $self->{cluster};
}
# Record the file's cluster, and call any callbacks that were waiting on this
sub Sys::Export::VFAT::_ClusterData::set_cluster($self, $cl_id) {
   $self->{cluster}= $cl_id;
   $_->($self, $cl_id) for $self->{callbacks}->@*;
   $cl_id;
}

sub _new_dir($self, $name, $parent) {
   my $dir= bless {
      name => $name,
      ent_by_fc => {},
      is_root => !$parent,
      clusterdata => $self->_new_clusterdata($name, undef),
   }, 'Sys::Export::VFAT::_Directory';
   $self->{_subdirs}{refaddr $dir}= $dir;
   # VFAT holds strong reference to dirs, dir hold weak refs to other dirs
   # Root dir does not get '.' and '..' entries, even on FAT32, but it does get a volume label
   $dir->{ents}= [
      $parent? (
         $self->_new_dirent("$name/.",  0, long => undef, short11 => '.', dir => $dir),
         $self->_new_dirent("$name/..", 0, long => undef, short11 => '..', dir => $parent)
      ) : (
         # content of volume label entry gets updated during ->finish
         $self->_new_dirent("(volume label)", ATTR_VOLUME_ID, long => undef)
      )
   ];
   $dir;
}
sub Sys::Export::VFAT::_Directory::name        { $_[0]{name} }
sub Sys::Export::VFAT::_Directory::is_root     { $_[0]{is_root} }
sub Sys::Export::VFAT::_Directory::ents        { $_[0]{ents} }
sub Sys::Export::VFAT::_Directory::ent_by_fc   { $_[0]{ent_by_fc} }
sub Sys::Export::VFAT::_Directory::clusterdata { $_[0]{clusterdata} }
sub Sys::Export::VFAT::_Directory::child {
   $_[0]{ent_by_fc}{fc $_[1]}
}
sub Sys::Export::VFAT::_Directory::add_ent($dir, $dirent) {
   # Check collisions on both the long and short name
   my ($long, $short)= ($dirent->long, $dirent->short);
   my $long_conflict= defined $long && $dir->{ent_by_fc}{fc $long};
   my $short_conflict= defined $short && $dir->{ent_by_fc}{fc $short};
   if ($long_conflict || $short_conflict) {
      # If the user is writing a directory and the thing in the way is an auto-dir,
      # replace the entry with the new attributes.
      if ($long_conflict && (!$short_conflict || $short_conflict == $long_conflict)
         && $long_conflict->is_dir && $long_conflict->autovivified
         && $dirent->is_dir
      ) {
         weaken($dirent->{dir}= $long_conflict->{dir});
         $dirent->{clusterdata}= $long_conflict->{clusterdata};
      }
      else {
         croak "Path ".$dirent->name." already exists"
            if $long_conflict;
         croak "Path ".$dirent->name." short name ".$dirent->short." conflicts with "
            . $short_conflict->name;
      }
   }
   $dir->{ent_by_fc}{fc $long}= $dirent if length $long;
   $dir->{ent_by_fc}{fc $short}= $dirent if length $short;
   push $dir->{ents}->@*, $dirent;
   $dirent;
}
# This assigns a valid 8.3 filename which doesn't conflict with any of the other ents
sub Sys::Export::VFAT::_Directory::_build_shortname($dir, $ent) {
   my $name= $ent->long;
   my $ent_by_fc= $dir->ent_by_fc;
   length $name or die "BUG";
   $name= uc $name;
   my $ext_pos= rindex($name, '.');
   my $base= $ext_pos < 0? $name : substr($name, 0, $ext_pos);
   my $ext=  $ext_pos < 0? ''    : substr($name, $ext_pos+1);
   for ($base, $ext) {
      # Replace every run of invalid chars with a single '_'
      s/[^\x21\x23-\x29\x2D.\x30-\x39\x40-\x5A\x5E-\x7B\x7D-\xE4\xE6-\xFF]+/_/g;
      # Now that all high characters have been removed, consider these to be bytes
      utf8::downgrade($_);
   }
   $ext= '.'.substr($ext,0,3) if length $ext;
   my ($iter, $iter_len, $base_len)= (0,0, length $base);
   if (!$base_len || $base_len > 8) {
      substr($base, min($base_len,6), $base_len, '~1');
      ($iter, $iter_len)= (1, 2);
   }
   while ($ent_by_fc->{fc $base.$ext}) {
      my $next_iter_len= 1 + length ++$iter;
      my $iter_pos= min($base_len, 8 - $next_iter_len);
      croak "Can't find available ~N suffix for '$name'"
         if $iter_pos < 0;
      substr($base, $iter_pos, $next_iter_len, '~'.$iter);
      $iter_len= $next_iter_len;
   }
   $ent_by_fc->{fc $base.$ext}= $ent;
   $ent->{short}= $base.$ext;
}

sub Sys::Export::VFAT::_Directory::calc_size($dir) {
   # Sort the dirents but keep '.' and '..' and volume label at the start
   my $ents= $dir->{ents};
   my @head= splice(@$ents, 0, $dir->is_root? 1 : 2);
   @$ents= sort { fc $a->long cmp fc $b->long } @$ents;
   splice(@$ents, 0, 0, @head);
   # Need the 8.3 name in order to know whether it matches the long name
   my $n= @$ents;
   for (@$ents) {
      unless (length $_->short11) {
         $dir->_build_shortname($_) if !defined $_->short;
         my ($name, $ext)= split /\./, $_->short;
         $_->{short11}= pack 'A8 A3', $name, ($ext//'');
      }
      # Add LFN entries
      if (defined $_->long && $_->long ne $_->short) {
         my $utf16= encode('UTF-16LE', $_->long, Encode::FB_CROAK|Encode::LEAVE_SRC);
         $n += ceil(length($utf16) / 26);
      }
   }
   $log->debugf("dir /%s has %d real entries, %d LFN entries, size=%d ents=%s",
      $dir->name, scalar(@$ents), $n-scalar @$ents, $n*32,
      [ map [ $_->long, $_->short ], @$ents ] )
      if $log->is_debug;
   croak "Directory /".$dir->name." exceeds maximum entry count ($n >= 65536)"
      if $n >= 65536;
   $dir->clusterdata->{size}= $n * 32;  # always 32 bytes per dirent
}

# Called on every directory Allocation, this packs ->{data_dirents} into ->{data}
sub Sys::Export::VFAT::_Directory::pack($dir) {
   my $data= '';
   for my $ent ($dir->ents->@*) {
      $log->tracef("encoding dirent short=%-12s long=%s cluster=%s",
         $ent->short, $ent->long, $ent->cluster);

      my $short11= $ent->short11 // die "BUG: short11 not set";
      $short11 =~ s/^\xE5/\x05/; # \xE5 may occur in some charsets, and needs escaped

      # Need Long-File-Name entries?
      my $long= $ent->long;
      if (defined $long && $long ne $ent->short) {
         # Checksum for directory shortname, used to verify long name parts
         my $cksum= 0;
         $cksum= ((($cksum >> 1) | ($cksum << 7)) + $_) & 0xFF
            for unpack 'C*', $short11;
         # Each dirent holds up to 26 bytes (13 chars) of the long name
         my @chars= unpack 'v*', encode('UTF-16LE', $long, Encode::FB_CROAK|Encode::LEAVE_SRC);
         # short final chunk is padded with \0\uFFFF*
         if (my $remainder= @chars % 13) {
            push @chars, 0;
            push @chars, (0xFFFF)x(12 - $remainder);
         }
         my $last= ceil(@chars/13) - 1;
         for my $i (reverse 0..$last) {
            my $ofs= $i*13;
            my $seq= ($i + 1) | (($i == $last) ? 0x40 : 0x00);
            $data .= pack('C v5 C C C v6 v v2',
               $seq,                      # sequence and end-flag
               @chars[$ofs .. $ofs+4],    # first 5 chars
               0x0F, 0x00, $cksum,        # attr = LFN, type = 0
               @chars[$ofs+5 .. $ofs+10], # next 6 chars
               0,                         # no cluster number
               @chars[$ofs+11 .. $ofs+12] # last 2 chars
            );
         }
      }

      my $mtime= $ent->mtime // time;
      my ($wdate, $wtime)             = _epoch_to_fat_date_time($mtime);
      my ($cdate, $ctime, $ctime_frac)= _epoch_to_fat_date_time($ent->btime // $mtime);
      my ($adate)                     = _epoch_to_fat_date_time($ent->atime // $mtime);
      # References to the root dir are always encoded as cluster zero, even on FAT32
      # where the root dir actually lives at a nonzero cluster.
      # Volume label also doesn't need a cluster id.  Nor do empty files.
      my $cluster= 0;
      my $clusterdata= $ent->clusterdata;
      if ($clusterdata && !($ent->dir && $ent->dir->is_root)) {
         $cluster= $clusterdata->cluster;
         unless (defined $cluster) {
            $cluster= 0;
            # If we need to encode a cluster which isn't known, append a function to the
            # allocation which can later be called when the cluster is known, to repack
            # the directory entry.
            my $name= $ent->name;
            $log->tracef("temporarily encoding dirent %s cluster as 0 to be patched later", $name);
            my $ent_ofs= length($data);
            $clusterdata->on_cluster_change(sub ($cldat, $cl_id) {
               $log->tracef("patching %s with cluster id %s", $name, $cl_id);
               substr($data, $ent_ofs+20, 2, pack 'v', ($cl_id>>16));
               substr($data, $ent_ofs+26, 2, pack 'v', $cl_id);
            });
         }
      }
      # Directories always written as size = 0
      my $size= $ent->is_dir? 0 : $clusterdata? $clusterdata->size : 0;
      $log->tracef(" with encoded size=%d cluster=%d", $size, $cluster);
      $data .= pack('A11 C C C v v v v v v v V',
         $short11, $ent->attrs, 0, #NT_reserved
         $ctime_frac, $ctime, $cdate, $adate,
         $cluster >> 16, $wtime, $wdate, $cluster, $size);
   }
   die "BUG: calculated dir size ".$dir->clusterdata->size." != data length ".length($data)
      unless $dir->clusterdata->size == length $data;
   # Dir must be padded to length of sector/cluster with entries whose name begins with \x00
   # but that will happen automatically later as the data is appended to the file.
   $dir->clusterdata->{data_ref}= \$data;
}

sub _new_dirent($self, $name, $attrs, %fields) {
   my $long= exists $fields{long}? $fields{long} : ($name =~ m{([^/]+)\z})[0];
   my $short= $fields{short};
   if (defined $short) {
      croak "Invalid short name '$short'" unless is_valid_shortname($short);
   } else {
      $short //= $long if defined $long && is_valid_shortname($long);
   }
   utf8::downgrade($short) if defined $short; # verified to be just bytes
   # short11 is used internally and was already vetted
   my $short11= $fields{short11};
   $attrs |= ATTR_DIRECTORY if defined $fields{dir};
   my $ent= bless {
      name    => $name,
      long    => $long,
      short   => $short,
      short11 => $short11,
      attrs   => $attrs,
   }, 'Sys::Export::VFAT::_DirEnt';
   for (qw( atime mtime btime autovivified clusterdata )) {
      $ent->{$_}= $fields{$_} if defined $fields{$_};
   }
   $ent->dir($fields{dir}) if defined $fields{dir};
   return $ent;
}
sub Sys::Export::VFAT::_DirEnt::name        { $_[0]{name} }
sub Sys::Export::VFAT::_DirEnt::long        { $_[0]{long} }
sub Sys::Export::VFAT::_DirEnt::short       { $_[0]{short} }
sub Sys::Export::VFAT::_DirEnt::short11     { $_[0]{short11} }
sub Sys::Export::VFAT::_DirEnt::attrs       { $_[0]{attrs} }
sub Sys::Export::VFAT::_DirEnt::is_dir      { $_[0]{attrs} & ATTR_DIRECTORY }
sub Sys::Export::VFAT::_DirEnt::atime       { $_[0]{atime} }
sub Sys::Export::VFAT::_DirEnt::mtime       { $_[0]{mtime} }
sub Sys::Export::VFAT::_DirEnt::btime       { $_[0]{btime} }
sub Sys::Export::VFAT::_DirEnt::clusterdata { $_[0]{clusterdata} }
sub Sys::Export::VFAT::_DirEnt::cluster     { $_[0]{clusterdata} && $_[0]{clusterdata}->cluster }
sub Sys::Export::VFAT::_DirEnt::autovivified{ $_[0]{autovivified} }
sub Sys::Export::VFAT::_DirEnt::dir {
   if (@_ > 1) {
      # entries have weak references to directories, the VFAT holds the strong ref
      weaken($_[0]{dir}= $_[1]);
      $_[0]{clusterdata}= $_[1]->clusterdata;
   }
   $_[0]{dir}
}

=method add

  $fat->add(\%file_attrs);
  # Attributes:
  # {
  #   name               => $path_bytes,
  #   uname              => $path_unicode_string,
  #   FAT_shortname      => "8CHARATR.EXT",
  #   mode               => $unix_stat_mode,
  #   FAT_attrs          => ATTR_READONLY|ATTR_HIDDEN|ATTR_SYSTEM|ATTR_ARCHIVE,
  #   atime              => $unix_epoch,
  #   mtime              => $unix_epoch,
  #   btime              => $unix_epoch,
  #   size               => $data_size,
  #   data               => $literal_data_or_scalarref,
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
   my $path= $file->{uname} // decode('UTF-8', $file->{name}, Encode::FB_CROAK | Encode::LEAVE_SRC);
   $path =~ s,^/,,; # remove leading slash, if any

   # Validate path components
   my @path= grep length, split '/', $path;
   is_valid_longname($_) || croak "Not a valid VFAT filename: '$_'"
      for @path;
   my $leaf= pop @path;

   # did user supply FAT attribute bitmask?
   my $attrs= $file->{FAT_attrs} // do {
      # readonly determined by user -write bit of 'mode'
      (!($file->{mode} & 0400)? ATTR_READONLY : 0)
      # hidden determined by leading '.' in filename
      | (defined $leaf && $leaf =~ /^\./? ATTR_HIDDEN : 0)
   };
   my $clusterdata;
   if (S_ISREG($file->{mode})) {
      my ($size, $offset, $align, $data_path)
         = @{$file}{qw( size device_offset device_align data_path )};
      my $data_ref;
      # data can be supplied literally in ->{data} or by ->{data_path}
      if (defined $file->{data}) {
         $data_ref= ref $file->{data} eq 'SCALAR'? $file->{data} : \$file->{data};
         # Die on encoding mistakes
         if (utf8::is_utf8($$data_ref)) {
            my $x= $$data_ref;
            croak "->{data} must be 8-bit, but encountered wide character at $path"
               unless utf8::downgrade($x, 1);
            $data_ref= \$x;
         }
         $size //= length($$data_ref);
      } elsif (defined $data_path) {
         croak "Data file '$data_path' does not exist, for '$path'"
            unless -e $data_path;
         croak "Data file '$data_path' is not readable, for '$path'"
            unless -r $data_path;
         $size= -s $data_path;
      }
      else {
         croak "File $path has size=$size but lacks 'data' or 'data_path' attributes";
      }
      # must be a power of 2
      croak "Invalid device_align $align for '$path', must be a power of 2"
         if defined $align && !isa_pow2 $align;
      my $offset_out;
      # Sanity check device_offset before we get too far along
      if (defined $offset) {
         $align //= 512;
         # Could be a scalar ref, for output
         if (ref $offset eq 'SCALAR') {
            $offset_out= $offset;
            $offset= undef;
         } else {
            # must fall in the data area
            $offset > $self->volume_offset + _minimum_offset_to_data
            # must be a multiple of at least 512 (probably more)
            && !($offset & ($align-1))
               or croak "Invalid device_offset '$offset' for file '$path'";
         }
      }
      $clusterdata= $self->_new_clusterdata($path, $size,
         align => $align, offset => $offset,
         data_ref => $data_ref, data_path => $data_path,
      );
      # If user requested to find out the extent this file landed at,
      # set up a callback to record that.
      if ($offset_out) {
         weaken(my $self2= $self);
         $clusterdata->on_cluster_change(
            sub($cldat, $cl_id) {
               $$offset_out= $cl_id < 2? undef # 0 and 1 are not real clusters
                  : $self2->geometry->get_cluster_device_offset($cl_id);
            }
         )
      }
   } elsif (S_ISDIR($file->{mode})) {
      $attrs |= ATTR_DIRECTORY;
   }
   else {
      # TODO: add conditional symlink support via hardlinks
      croak "Can only export files or directories into VFAT"
   }

   # Walk through the tree based on the case-folded path
   my $dir= $self->{_root};
   for (@path) {
      my $ent= $dir->child($_);
      if ($ent) {
         croak $ent->name." is not a directory, while attempting to add '$path'"
            unless $ent->is_dir;
      } else { # auto-create directory
         $ent= $dir->add_ent($self->_new_dirent($dir->name."/$_", ATTR_DIRECTORY,
            autovivified => 1,
            dir => $self->_new_dir($ent->name, $dir)
         ));
      }
      $dir= $ent->dir;
   }
   # Add the new entry to this dir - croaks on name collision
   my $ent= $dir->add_ent($self->_new_dirent($path, $attrs, clusterdata => $clusterdata,
      $file->%{qw( atime mtime btime )},
      (length $file->{FAT_shortname}? (short => $file->{FAT_shortname}) : ())
   ));
   # initialize after add_ent in case the new dirent inherited an autovivified dir
   $ent->dir($self->_new_dir($path, $dir))
      if $ent->is_dir && !$ent->dir;

   $log->debugf("added %s longname=%s shortname=%s %s",
      $ent->name, $ent->long, $ent->short, $ent->is_dir? 'DIR'
      : $ent->clusterdata? sprintf("size=0x%X device_align=0x%X device_offset=0x%X",
         $ent->clusterdata->size, $ent->clusterdata->align, $ent->clusterdata->offset)
      : 'size=0 (empty file)' )
      if $log->is_debug;
   $self;
}

=method finish

This method performs all the actual work of building the filesystem.  This module writes the
entire filesystem in one pass after deciding the best geometry and minimal number of clusters
to hold the data you've supplied.

You may get exceptions during this call if there isn't a way to write your files as requested.

=cut

sub finish($self) {
   my $root= $self->{_root};
   # Find out the size of every directory, and build ->{_allocs}, ->{_dir_allocs} and ->{_special_allocs}
   $root->ents->[0]{short11}= $self->volume_label // 'NO NAME    ';
   $_->calc_size for $root, values $self->{_subdirs}->%*;
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates
   my ($geom, $alloc)= $self->_optimize_geometry
      or croak join("\n", "No geometry options can meet your device_offset / device_align requests:",
            map "$_: $self->{_optimize_geometry_fail_reason}{$_}",
               sort { $a <=> $b } keys $self->{_optimize_geometry_fail_reason}->%*
         );
   $self->{geometry}= $geom;
   $self->{allocation_table}= $alloc;
   # Apply file cluster IDs to the ClusterData objects
   $_->{clusterdata}->set_cluster($_->{invlist}[0])
      for values $alloc->chains->%*;
   # Pack directories now that all file cluster ids are known
   $_->pack for $root, values $self->{_subdirs}->%*;

   my $fh= $self->filehandle;
   if (!$fh) {
      defined $self->filename or croak "Must set filename or out_fh attributes";
      open $fh, '+>', $self->filename
         or croak "open: $!";
   }
   # check size by seeking to end
   my $pos= sysseek($fh, 0, SEEK_END);
   if ($pos < $geom->volume_offset + $geom->total_size) {
      truncate($fh, $geom->volume_offset + $geom->total_size)
         or croak "truncate: $!";
   }
   $self->_write_filesystem($fh, $geom, $alloc);
   unless ($self->filehandle) {
      $fh->close or croak "close: $!";
   }
   1;
}

sub _log_hexdump($buf) {
   $log->tracef('%04X'.(" %02x"x16), $_, unpack 'C*', substr($buf, $_*16, 16))
      for 0..ceil(length($buf) / 16);
}

sub _write_block_at($fh, $addr, $data_ref, $ofs, $size, $descrip=undef) {
   $log->tracef("write %s at 0x%X-0x%X from buf size 0x%X%s",
      $descrip//'blocks', $addr, $addr+$size, length($$data_ref), $ofs? sprintf(" ofs 0x%X", $ofs) : ''
      ) if $log->is_trace;
   sysseek($fh, $addr, SEEK_SET) or croak "sysseek($addr): $!";
   $ofs //= 0;
   my $avail= length($$data_ref) - $ofs;
   $size //= $avail;
   # always write full size, padding with zeroes
   if ($avail < $size) {
      my $data= $avail > 0? substr($$data_ref, $ofs) : '';
      $data .= "\0" x ($size-length($data));
      $data_ref= \$data;
   }
   my $wrote= syswrite($fh, $$data_ref, $size, $ofs);
   croak "syswrite: $!" if !defined $wrote;
   croak "Unexpected short write ($wrote != $size)" if $wrote != $size;
   return 1;
}   

# This function depends on the file being pre-zeroed, which happens automatially for an empty
# file that has just had its length changed by truncate().  This would write an invalid
# filesystem if the handle is a block device with random leftover data in it.
sub _write_filesystem($self, $fh, $geom, $alloc) {
   ($alloc->max_cluster_id//-1) == ($geom->max_cluster_id//-1)
      or croak "Max element of 'fat_entries' should be ".$geom->max_cluster_id.", but was ".$alloc->max_cluster_id;
   # Pack the boot sector and other reserved sectors
   my $buf= $self->_pack_reserved_sectors;
   my $ofs= $self->volume_offset;
   _write_block_at($fh, $ofs, \$buf, 0, $geom->reserved_size, 'reserved sectors');
   $ofs += $geom->reserved_size;
   # Pack the allocation tables
   $buf= $alloc->pack;
   # store a copy of this into each of the regions occupied by fats
   for (my $i= 0; $i < $geom->fat_count; $i++) {
      _write_block_at($fh, $ofs, \$buf, 0, $geom->fat_size, "fat table $i");
      $ofs += $geom->fat_size;
   }
   # For FAT12/FAT16, write the root directory entries
   if ($geom->bits < FAT32) {
      my $root_cldata= $self->{_root}->clusterdata;
      die "BUG: mis-sized FAT16 root directory"
         if !$root_cldata->size || ($root_cldata->size & 31)
            || length ${$root_cldata->data_ref} != $root_cldata->size
            || $root_cldata->size > $geom->root_dir_size;
      _write_block_at($fh, $ofs, $root_cldata->data_ref, 0, $geom->root_dir_size, 'root dir');
   }
   # The files and dirs have all been assigned clusters by _optimize_geometry
   for my $cl (sort { $a <=> $b } keys $alloc->chains->%*) {
      my ($invlist, $cldata)= $alloc->chains->{$cl}->@{'invlist','clusterdata'};
      my $data_ref= $cldata->data_ref;
      if (!defined $data_ref && defined $cldata->data_path) {
         slurp_file(my $data, $cldata->data_path);
         $data_ref= $data;
      }
      # Given an inversion list describing the allocated clusters for this file,
      # write the relevant chunks of the file to those cluster data areas.
      $log->debugf("writing '/%s' at cluster %s", $cldata->name, _render_invlist($invlist))
         if $log->is_debug;
      my $data_ofs= 0;
      for (my $i= 0; $i < @$invlist; $i += 2) {
         my ($cl_start, $cl_lim)= @{$invlist}[$i, $i+1];
         my $size= ($cl_lim-$cl_start) * $geom->bytes_per_cluster;
         _write_block_at($fh, $geom->get_cluster_device_offset($cl_start), $data_ref, $data_ofs, $size);
         $data_ofs += $size;
      }
   }
}
sub _render_invlist($il) {
   join ',',
      map +($il->[$_*2] == $il->[$_*2+1]-1? $il->[$_*2] : $il->[$_*2] . '-' . $il->[$_*2+1]),
      0 .. int($#$il/2)
}

=export is_valid_longname

  $bool= is_valid_longname($name)

C<$name> should be a unicode string

=export is_valid_shortname

  $bool= is_valid_shortname($name)

C<$name> should be encoded as platform-native bytes, with no codepoints above 0xFF.
Allows space characters in the name, even though most DOS tools can't handle that.

=export is_valid_volume_label

  $bool= is_valid_volume_label($name)

C<$name> should be encoded as platform-native bytes, with no codepoints above 0xFF.

=cut

sub is_valid_longname {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   # characters permitted for LFN are all letters numbers and $%'-_@~`!(){}^#&+,;=[].
   # and space and all codepoints above 0x7F.
   # they may not begin with space, and cannot exceed 255 chars.
   !!($_[0] !~ /^(\.+\.?)\z/ # dot and dotdot are reserved
   && $_[0] =~ /^
      [\x21\x23-\x29\x2B-\x2E\x30-\x39\x3B\x3D\x40-\x5B\x5D-\x7B\x7D-\xE4\xE6-]
      [\x20\x21\x23-\x29\x2B-\x2E\x30-\x39\x3B\x3D\x40-\x5B\x5D-\x7B\x7D-\xE4\xE6-]{0,254}
      \z/x);
}

sub is_valid_shortname {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   !!($_[0] eq uc $_[0]
   && $_[0] =~ /^
      [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{1,8}
      [\x20\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,7}
      ( \. [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,3} )
      \z/x);
}

sub is_valid_volume_label {
   shift if @_ > 1 && "$_[0]"->isa(__PACKAGE__);
   # same as shortname but no '.' and space is allowed
   !!($_[0] =~ /^
      [\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]
      [\x20\x21\x23-\x29\x2D\x30-\x39\x40-\x5A\x5E-\x7B\x7D\x80-\xFF]{0,10}
      \z/x);
}

sub _optimize_geometry($self) {
   # calculate what geometry gives us the best size, when rounding each file to that cluster
   # size vs. the size of the FAT it generates, and also meting the needs of alignment requests
   my $root_cldata= $self->{_root}->clusterdata;
   my (@offsets, @aligned, @others);
   my %seen= ( refaddr($root_cldata) => 1 );
   for my $dir ($self->{_root}, values $self->{_subdirs}->%*) {
      for my $ent ($dir->ents->@*) {
         my $cldat= $ent->clusterdata;
         next unless $cldat && !$seen{refaddr $cldat}++;
         push @{$cldat->offset? \@offsets : $cldat->align? \@aligned : \@others}, $cldat;
      }
   }
   $log->debugf("_optimize_geometry offsets=%d aligned=%d others=%d",
      scalar @offsets, scalar @aligned, scalar @others);
   my $min_ofs= min(map $_->offset, @offsets);
   my $max_ofs= max(map $_->offset + $_->size, @offsets);
   my $max_align= max(0, map $_->align, @aligned);
   my $root_dirent_used= $root_cldata->size / 32;
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
      for (@offsets, @aligned, @others) {
         $clusters += ceil($_->{size} / $cluster_size);
      }
      $log->tracef("with sectors_per_cluster=%d, would require at least %d clusters",
         $sectors_per_cluster, $clusters);
      $clusters ||= 1;
      my ($reserved, $root_clusters_added);
      # If file alignment is a larger power of 2 than cluster_size, then as long as data_start
      # is aligned to cluster_size there will be a cluster that can satisfy the alignment.
      # If file alignment is a smaller power of 2 than cluster_size, then as long as
      # data_start is aligned to the file alignment, every cluster can satisfy the alignment.
      my $align= min($cluster_size, $max_align);
      if ($align) {
         # But wait, does every device_offset meet this alignment?  If not, give up.
         for (@offsets) {
            if ($_->offset & ($align-1)) {
               $fail_reason{$sectors_per_cluster}= "device_offset $_->{offset}"
                  ." of $_->{name} conflicts with your alignment request of $align";
               next cluster_size;
            }
         }
      }
      elsif (@offsets) {
         # If not aligning clusters to pow2, might need to align to device_offset.
         # First, every device_offset must have the same remainder modulo cluster_size.
         my ($remainder, $prev);
         for (@offsets) {
            my $r= $_->offset & ($cluster_size-1);
            if (!defined $remainder) {
               $remainder= $r;
               $prev= $_;
            } elsif ($remainder != $r) {
               $fail_reason{$sectors_per_cluster}= "file $_->{name} device_offset"
                  ." $_->{offset} modulo cluster_size $cluster_size conflicts with"
                  ." file $prev->{name} device_offset $prev->{offset}";
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
            $root_clusters_added= ceil($root_cldata->size / $cluster_size);
            $clusters += $root_clusters_added;
            $log->tracef("reached FAT32 threshold, adding %s clusters for root dir", $root_clusters_added);
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
            $min_ofs, $max_ofs);
         if (@offsets || @aligned) {
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
         my %assignment;
         unless (eval {
            for my $cldata (@offsets, @aligned, @others,
               ($geom->bits == FAT32? ($root_cldata) : ())
            ) {
               my $chain= $self->_alloc_file($geom, $alloc, $cldata);
               $alloc->get_chain($chain)->{clusterdata}= $cldata;
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
            $best= { geom => $geom, alloc => $alloc, cluster_assignment => \%assignment };
         }
      }
   } continue {
      $log->tracef("%s", $fail_reason{$sectors_per_cluster})
         if defined $fail_reason{$sectors_per_cluster};
   }
   if (!$best) {
      $log->debug("no cluster size works");
      $self->{_optimize_geometry_fail_reason}= \%fail_reason;
      return;
   }
   $log->debugf("best cluster_size is %d", $best->{geom}->bytes_per_cluster);
   return @{$best}{'geom','alloc'};
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

sub _alloc_file($self, $geom, $alloc, $cldata) {
   my $sz= $cldata->size or do { carp "Attempt to allocate zero-length file"; return };
   my $cl_count= POSIX::ceil($sz / $geom->bytes_per_cluster);
   if ($cldata->offset) {
      my ($cl, $n)= $geom->get_cluster_extent_of_device_extent($cldata->offset, $sz);
      return $alloc->alloc_range($cl, $cl_count)
         // croak "Can't allocate $cl_count clusters from offset ".$cldata->offset;
   } elsif ($cldata->align) {
      my ($mul, $ofs)= $geom->get_cluster_alignment_of_device_alignment($cldata->align);
      return $alloc->alloc_contiguous($cl_count, $mul, $ofs)
         // croak "Can't allocate $cl_count clusters aligned to ".$cldata->align;
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
