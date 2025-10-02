package Sys::Export::ISO9660;

our $VERSION= 0; # VERSION
# ABSTRACT: Write ISO9660 filesystems with Joliet filename and El Torrito boot record support

use v5.26;
use warnings;
use experimental qw( signatures );
use Fcntl qw( S_IFDIR S_ISDIR S_ISREG SEEK_SET SEEK_END );
use Scalar::Util qw( blessed dualvar refaddr weaken );
use List::Util qw( min max );
use Time::HiRes 'time';
use POSIX 'ceil';
use Sys::Export qw( :isa write_file_extent expand_stat_shorthand );
use Sys::Export::LogAny '$log';
use Encode;
use constant {
   FLAG_HIDDEN      => dualvar(0x01, 'FLAG_HIDDEN'),      # hidden file
   FLAG_DIRECTORY   => dualvar(0x02, 'FLAG_DIRECTORY'),   # directory
   FLAG_ASSOCIATED  => dualvar(0x04, 'FLAG_ASSOCIATED'),  # associated file
   FLAG_RECORD      => dualvar(0x08, 'FLAG_RECORD'),      # record format
   FLAG_PROTECTION  => dualvar(0x10, 'FLAG_PROTECTION'),  # permissions
   FLAG_MULTIEXTENT => dualvar(0x80, 'FLAG_MULTIEXTENT'), # continued in another extent
   LBA_SECTOR_SIZE  => 2048,
   LBA_SECTOR_POW2  => 11,
};
sub _sector_of($addr) { $addr >> LBA_SECTOR_POW2 }
sub _remaining_sector_bytes($pos) { -$pos & (LBA_SECTOR_SIZE-1) }
sub _round_to_whole_sector($len) { ($len + LBA_SECTOR_SIZE - 1) & ~(LBA_SECTOR_SIZE-1) }
require Sys::Export::ISO9660::File;
require Sys::Export::ISO9660::Directory;
use Carp;
our @CARP_NOT= qw( Sys::Export Sys::Export::Unix );
use Exporter 'import';
our @EXPORT_OK= qw(
   FLAG_HIDDEN FLAG_DIRECTORY FLAG_ASSOCIATED FLAG_RECORD FLAG_PROTECTION FLAG_MULTIEXTENT
   is_valid_shortname is_valid_joliet_name remove_invalid_shortname_chars
   LBA_SECTOR_SIZE LBA_SECTOR_POW2
);

=head1 SYNOPSIS

  my $dst= Sys::Export::ISO9660->new(
    filename => $path,
  );
  # Basic files and directories
  $dst->add([ file => "README.TXT", "Hello World\r\n" ]);
  $dst->add([ file => 'EFI/BOOT/BOOTIA32.EFI', { data_path => $loader }]);
  
  # Point to a file at an absolute offset from the start of the device
  # The data is assumed to already exist in $dst->filename
  $dst->add([ file => 'initrd', { device_offset => 0x110000 }]);
  
  $dst->finish;

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
   $self->{root}= Sys::Export::ISO9660::Directory->new(name => '(root)');
   # apply other attributes
   $self->$_($attrs{$_}) for keys %attrs;
   $self;
}

# Create dir, and store a strong reference in ->{_subdirs}
sub _new_dir($self, $name, $parent, $file) {
   my $dir= Sys::Export::ISO9660::Directory->new(name => $name, parent => $parent, file => $file);
   $self->{_subdirs}{refaddr $dir}= $dir;
   $dir;
}
sub _new_file($self, $name, %attrs) {
   Sys::Export::ISO9660::File->new(name => $name, %attrs);
}

=attribute filename

Name of file (or device) to write.  If the file exists it will be truncated before writing.
If you want to write the filesystem amid existing data (like a partition table0, pass a file
handle as C<filehandle>.

=attribute filehandle

Output filehandle to write.  The file will be enlarged if it is not big enough.

=cut

sub filename { @_ > 1? ($_[0]{filename}= $_[1]) : $_[0]{filename} }
sub filehandle { @_ > 1? ($_[0]{filehandle}= $_[1]) : $_[0]{filehandle} }

=attribute root

The root L<Directory|Sys::Export::ISO9660::Directory> object.

=cut

sub root { $_[0]{root} }

=attribute volume_label

Name for the filesystem image

=attribute volume_set

Name for a multi-volume collection

=attribute publisher

Name of entity publishing this image

=attribute application

Name of application for which this image is used

=attribute system

Name of the system for which the filesystem is intended

=attribute path_to_copyright

Path within the encoded filesystem where a copyright file can be found.  Must use short names
and contain ';1' suffix on filename and be no more than 37 characters.

=attribute path_to_abstract

Path within the encoded filesystem where an abstract can be found.
Same name rules as L</path_to_copyright>.

=attribute path_to_bibliographic

Path within the encoded filesystem where bibliographic information can be found.
Same name rules as L</path_to_copyright>.

=cut

for (qw( volume_label system )) {
   eval "sub $_ { \@_ > 1? (\$_[0]{$_}= _coerce_label(\$_[1])) : \$_[0]{$_} } 1" or die "$@";
}
for (qw( volume_set publisher application )) {
   eval "sub $_ { \@_ > 1? (\$_[0]{$_}= _coerce_meta_id(\$_[1])) : \$_[0]{$_} } 1" or die "$@";
}
for (qw( copyright_file abstract_file bibliographic_file )) {
   eval "sub $_ { \@_ > 1? (\$_[0]{$_}= \$_[1]) : \$_[0]{$_} } 1" or die "$@";
}

sub _coerce_label($label) {
   $label= uc($label);
   $label =~ /^[A-Z0-9_ ]{0,32}\z/x
      or croak "Labels must be <= 32 uppercase letters, digits, underscore or space";
   $label;
}
sub _coerce_meta_id($label) {
   $label= uc($label);
   $label =~ /^[A-Z0-9_ ]{0,128}\z/x
      or croak "Metadata IDs must be <= 128 uppercase letters, digits, underscore or space";
   $label;
}

=export is_valid_shortname

Returns true if string meets the very restricted 8.3 notation of a file, with the added
exception of allowing '~' in filenames.  (Windows name mangling generates these and writes
them into iso9660 images, so it's expected to work everywhere)

=export is_valid_joliet_name

Returns true as long as string is less than 127 unicode characters and doesn't contain
forbidden characters.

=export remove_invalid_shortname_chars

  $str= remove_invalid_shortname_chars($name, '_');

Returns a string with any invalid characters replaced with the supplied replacement character.

=cut

sub is_valid_shortname($name) {
   !!($name =~ /^[A-Z0-9_~]{1,8} (\. [A-Z0-9_]{1,3} )? \z/x)
}

sub is_valid_joliet_name($name) {
   !!($name =~ m{^[^\x00-\x1F\\/;\x7F-\x9F]{1,127}\z}x)
}

sub remove_invalid_shortname_chars($name, $replacement='_') {
   $name =~ tr/a-z/A-Z/; # perform 'uc' but only for the ASCII range
   $name =~ s/[^A-Z0-9_]+/$replacement/gr;
}

=method add

  $fat->add(\%file_attrs);
  # Attributes:
  # {
  #   name               => $path_bytes,
  #   uname              => $path_unicode_string,
  #   ISO9660_shortname  => "8CHARATR.EXT",
  #   mode               => $unix_stat_mode,
  #   ISO9660_flags      => FLAG_HIDDEN,
  #   mtime              => $unix_epoch,
  #   size               => $data_size,
  #   data               => $scalar_or_scalarref_or_LazyFileData,
  #   device_offset      => $desired_byte_offset,
  # }

This add method takes the same file objects as used by Sys::Export, but with some optional
extras:

=over

=item ISO9660_shortname

Any file name not conforming to the 8.3 name limitation of FAT will get an auto-generated
"short" filename, in addition to its "long" filename.  If you want control over what short name
is generated, you can specify it with C<FAT_shortname>.

=item ISO9660_flags

An ORed combination of L<flags|/FILE FLAGS>.  The C<FLAG_DIRECTORY> is still automatically added
based on the C<mode> flags.

=item device_offset

For integration with ISOHybrid, you may specify C<device_offset> to request the file be placed
at an exact location, and as a single un-fragmented extent.  This accounts for the
L</device_offset> of the whole filesystem.  If you supply C<device_offset> and do not supply
C<data> it is assumed the data already exists at that address.

C<device_offset> must align to 2KiB boundaries of the LBA sectors.

=back

=cut

sub add($self, $spec) {
   $spec= { expand_stat_shorthand($spec) }
      if isa_array $spec;

   defined $spec->{uname} or defined $spec->{name}
      or croak "Require 'uname' or 'name'";
   defined $spec->{mode} or croak "Require 'mode'";

   # If user supplied uname, use that as a more official source of Unicode
   my $path= $spec->{uname} // decode('UTF-8', $spec->{name}, Encode::FB_CROAK | Encode::LEAVE_SRC);
   $path =~ s,^/,,; # remove leading slash, if any

   my @path= grep length, split '/', $path;
   my $leaf= pop @path;

   # Walk through the tree based on the case-folded path
   my $parent= $self->root;
   for (@path) {
      my $ent= $parent->entry($_);
      if ($ent) {
         croak $ent->name." is not a directory, while attempting to add '$path'"
            unless $ent->{dir};
      } else { # Auto-create directory. Autovivication is indicated by ->{file} = undef
         $ent= $parent->add($_, undef);
         weaken($ent->{dir}= $self->_new_dir($parent->name."/$_", $parent, undef));
      }
      $parent= $ent->{dir};
   }

   my $flags= $spec->{ISO9660_flags};
   my $file;
   if (S_ISREG($spec->{mode})) {
      my ($size, $offset, $data_ref)
         = @{$spec}{qw( size device_offset data )};
      $data_ref= do { my $x= $data_ref; \$x }
         if defined $data_ref && !ref $data_ref;
      if ($size) {
         # ensure data matches
         croak "File $path ->{data} length disagrees with ->{size}"
            if $data_ref && length($$data_ref) != $size;
      } elsif (defined $data_ref) {
         $size //= length($$data_ref);
      }
      $file= $self->_new_file($path, size => $size, flags => $flags, mtime => $spec->{mtime},
                              device_offset => $offset, data => $data_ref);
   } elsif (S_ISDIR($spec->{mode})) {
      $flags |= FLAG_DIRECTORY;
      # If adding this directory overtop a previous auto-vivified directory, the ->{file}
      # will be empty and we can just update it.
      my $cur= $parent->entry($leaf);
      croak "Attempt to add duplicate directory $leaf"
         if $cur && $cur->{file};
      $file= $self->_new_file($path, size => 0, flags => $flags, mtime => $spec->{mtime});
      if ($cur) {
         $cur->{file}= $file;
         $cur->{dir}{file}= $file;
         $log->debugf("updated attributes of %s", $path);
         return $file;
      }
      # otherwise, add this file to a directory entry
   }
   else {
      # TODO: add conditional symlink support via hardlinks
      croak "Can only export files or directories into VFAT"
   }

   # this also checks for name collisions on shortname
   my $ent= $parent->add($leaf, $file, shortname => $spec->{ISO9660_shortname});
   # If the dirent is a directory, also add a directory object to the dirent
   if ($file->is_dir) {
      # the directory object also gets a reference to its file object.
      weaken($ent->{dir}= $self->_new_dir($path, $parent, $file));
   }

   $log->debugf("added %s longname=%s shortname=%s %s",
      $path, $ent->{name}, $ent->{shortname}, (
         !$ent->{file}? 'size=0 (empty file)'
         : $ent->{file}->is_dir? 'DIR'
         : sprintf("size=0x%X device_offset=0x%X",
            $ent->{file}->size, $ent->{file}->device_offset//0)
      ))
      if $log->is_debug;

   $file;
}

=method finish

This method performs all the actual work of building the filesystem.  This module writes the
entire filesystem in one pass after calculating all the metdata that needs encoded and where to
place all the files and directories.

You may get exceptions during this call if there isn't a way to write your files as requested.

=cut

sub finish($self) {
   # Find out the size of every directory, and the path tables
   $self->_calc_dir_sizes;
   $self->_calc_path_table_size;
   # Choose LBA extents for everything
   $self->_choose_file_extents;
   # Pack directories now that all file sector ids are known
   $self->_pack_directory($_) for $self->root, values $self->{_subdirs}->%*;
   $self->_pack_path_tables;

   my $fh= $self->filehandle;
   if (!$fh) {
      defined $self->filename or croak "Must set filename or out_fh attributes";
      open $fh, '+>', $self->filename
         or croak "open: $!";
   }
   # check size / truncate larger
   my $min_size= ($self->{max_used_lba}+1) * LBA_SECTOR_SIZE;
   if (-s $fh < $min_size) {
      truncate($fh, $min_size) or croak "truncate(iso, $min_size): $!";
   }
   $self->_write_filesystem($fh);
   unless ($self->filehandle) {
      $fh->close or croak "close: $!";
   }
   1;
}

# Write a ISO9660::File object to its configured extent, and clear its ->{data}
sub _write_file($fh, $file) {
   my $size= ($file->size + LBA_SECTOR_SIZE-1) & ~(LBA_SECTOR_SIZE-1);
   write_file_extent($fh, $file->device_offset, $size, $file->data);
   $file->{data}= undef; # free up memory as we go, also deallocates mmaps
}

# Write all descriptors, path tables, directories, and any file with ->{data}.
sub _write_filesystem($self, $fh) {
   # Write Primary Volume Descriptor
   my $lba= 16;
   write_file_extent($fh, $lba++ * LBA_SECTOR_SIZE, LBA_SECTOR_SIZE,
      \$self->_pack_primary_volume_descriptor);
   # Write Secondary Volume Descriptor (Joliet)
   write_file_extent($fh, $lba++ * LBA_SECTOR_SIZE, LBA_SECTOR_SIZE,
      \$self->_pack_joliet_volume_descriptor);
   # Write Boot Record descriptors
   # ...
   # Volume Set Terminator
   write_file_extent($fh, $lba++ * LBA_SECTOR_SIZE, LBA_SECTOR_SIZE, \"\xFFCD001\x01");
   
   # Write path tables
   _write_file($fh, $_) for $self->{path_tables}->@{qw( le be jle jbe )};

   # Write directory entries
   my @dirs= sort { $a->file->extent_lba <=> $b->file->extent_lba }
      $self->root, values $self->{_subdirs}->%*;
   for (@dirs) {
      _write_file($fh, $_) for $_->file, $_->joliet_file;
   }

   # Add any non-directory files that have data defined
   for my $dir (@dirs) {
      for my $ent ($dir->entries->@*) {
         next if $ent->{dir}; # directory files already got written
         next unless $ent->{file} && $ent->{file}->size && defined $ent->{file}->data;
         _write_file($fh, $ent->{file});
      }
   }
}

# TODO
sub _boot_record_descriptor_count { 0 }

# Assign extent_lba to every file where it wasn't already declared.
# This assumes we have an open-ended number of free sectors.
# ISOHybrid calls this with all the files owned by VFAT set to extent_lba = -1 so that
# it can determine how many initial sectors are used, and know where to start the VFAT
# filesystem, after which it revises all the shared files to point to extents within
# the VFAT filesystem.
sub _choose_file_extents {
   my $self= shift;
   # Sectors 0-15 are reserved
   # one sector for Primary Volume Descriptor
   # one sector for Secondary Volume Descriptor
   # Boot Record Descriptors,
   # and a Volume Descriptor Set Terminator
   my $lba= 16 + 1 + 1 + $self->_boot_record_descriptor_count + 1;
   my $max_seen= 0;
   my $assign_lba= sub {
      if (!defined $_->extent_lba) {
         $_->{extent_lba}= $lba;
         $lba += ceil($_->size / LBA_SECTOR_SIZE);
      } else {
         $max_seen= max($max_seen, $_->extent_lba + ceil($_->size / LBA_SECTOR_SIZE) - 1);
      }
      $log->debugf("File %s at LBA %s-%s",
         $_->name, $_->extent_lba, $_->extent_lba + ceil($_->size / LBA_SECTOR_SIZE) - 1);
   };
   # Add the 4 path tables here
   &$assign_lba for $self->{path_tables}->@{qw( le be jle jbe )};
   # Add the directories here
   my @dirs= ( $self->root, values $self->{_subdirs}->%* );
   for my $dir (@dirs) {
      &$assign_lba for $dir->file, $dir->joliet_file;
   }
   # Add any files that didn't get assigned yet
   for my $dir (@dirs) {
      &$assign_lba for grep defined $_ && $_->size && defined $_->data,
         map $_->{file}, $dir->entries->@*;
   }
   $self->{max_assigned_lba}= $lba-1;
   $self->{max_used_lba}= max($max_seen, $lba-1);
}

sub _calc_dir_sizes($self) {
   # First, ensure every directory has file and joliet_file and shortname defined
   my @dirs= ( $self->root, values $self->{_subdirs}->%* );
   for (@dirs) {
      $_->{file}        //= $self->_new_file($_->name, flags => FLAG_DIRECTORY);
      $_->{joliet_file} //= $self->_new_file($_->name, flags => $_->file->flags);
      # Need the 8.3 name in order to know whether it matches the long name
      $_->build_shortnames;
   }
   for my $dir (@dirs) {
      my $size= 34; # '.' entry
      $size += 34 unless $dir->is_root; # '..' entry
      my $joliet= $size;
      for ($dir->entries->@*) {
         my $is_dir= $_->{dir} || ($_->{file} && $_->{file}->is_dir);
         my $shortname_len= length $_->{shortname};
         my $pos= $size;
         $size += 33 + (($shortname_len + ($is_dir? 0 : 2))|1);
         $size += _remaining_sector_bytes($pos) if _sector_of($pos) != _sector_of($size); # round to next sector
         $pos= $joliet;
         my $joliet_name= encode('UTF-16BE', $_->{name}, Encode::FB_CROAK | Encode::LEAVE_SRC);
         $joliet += 33 + (length($joliet_name)|1);
         $joliet += _remaining_sector_bytes($pos) if _sector_of($pos) != _sector_of($joliet); # round to next sector
         # directory entries get added to a top-level "path table".
         # while we have the name available, calculate the size of those, too.
         if ($is_dir) {
            # gets padded to even length
            $_->{path_table_size}= 8 + $shortname_len + ($shortname_len & 1);
            # utf-16 will always be even length
            $_->{path_table_jsize}= 8 + length($joliet_name);
         }
      }
      $dir->{file}{size}= _round_to_whole_sector($size);
      $dir->{joliet_file}{size}= _round_to_whole_sector($joliet);
   }
}

sub _pack_iso_datetime($epoch) {
   my @t = gmtime($epoch);
   # year since 1900
   my $tz = 0; # UTC offset in 15min intervals
   return pack('C7', $t[5], $t[4]+1, $t[3], $t[2], $t[1], $t[0], $tz);
}

our @dirent_fields = (
  [ dir_len       =>  0, 1, 'C' ],
  [ ext_attr_len  =>  1, 1, 'C', 0 ],
  [ extent_lba    =>  2, 4, 'V' ],
  [ extent_lba    =>  6, 4, 'N' ],
  [ size          => 10, 4, 'V' ],
  [ size          => 14, 4, 'N' ],
  [ packed_mtime  => 18, 7, 'a7' ],
  [ flags         => 25, 1, 'C', 0 ],
  [ unit_size     => 26, 1, 'C', 0 ],
  [ gap_size      => 27, 1, 'C', 0 ],
  [ seq           => 28, 2, 'v', 1 ], # 'seq' only used for multi-disc sets
  [ seq           => 30, 2, 'n', 1 ], # will always be '1' for a single image.
  [ name_len      => 32, 1, 'C' ],
  # name_len and name appended after 33+
);
sub _encode_dirent($self, $name, $file, $ent= {}) {
   my $name_len=     length $name;
   my $dir_len=      33 + ($name_len|1);
   my $extent_lba=   $ent->{extent_lba} // $file && $file->extent_lba // 0;
   my $size=         $ent->{size} // $file && $file->size // 0;
   my $seq=          $ent->{seq} // 1;
   pack 'C C V N V N a7 C C C v n C a'.($name_len|1),
      $dir_len, $ent->{ext_attr_len} // 0,
      $extent_lba, $extent_lba,
      $size, $size,
      _pack_iso_datetime($ent->{mtime} // $file && $file->mtime // time),
      $ent->{flags} // $file && $file->flags // 0,
      $ent->{unit_size} // 0,
      $ent->{gap_size} // 0,
      $seq, $seq,
      $name_len, $name;
}

sub _pack_directory($self, $dir) {
   # dot and dotdot
   my $data= $self->_encode_dirent("\x00", $dir->file);
   my $joliet= $self->_encode_dirent("\x00", $dir->joliet_file);
   if (my $parent= $dir->parent) {
      $data .= $self->_encode_dirent("\x01", $parent->file);
      $joliet .= $self->_encode_dirent("\x01", $parent->joliet_file);
   }
   # real entries
   for (sort { lc $a->name cmp lc $b->name } $dir->entries->@*) {
      my $is_dir= $_->{dir} || $_->{file}->is_dir;
      my $shortname= $_->{shortname} . (!$is_dir && ';1');
      my $pos= length $data;
      $data .= $self->_encode_dirent($shortname, $_->{file}, $_);
      # If it crossed a sector boundary, move it fully into the next sector
      substr($data, $pos, 0, "\0"x _remaining_sector_bytes($pos))
         if _sector_of($pos) != _sector_of(length $data);
      my $jname= encode('UTF-16BE', $_->{name}, Encode::FB_CROAK | Encode::LEAVE_SRC);
      my $jfile= $_->{dir}? $_->{dir}->joliet_file # live dir holds reference to joliet_file
         : $is_dir? $_->{joliet_file}              # dirent could also reference the joliet_file
         : $_->{file};                             # else it's a plain file with no alt encoding
      $pos= length $joliet;
      $joliet .= $self->_encode_dirent($jname, $jfile, $_);
      # If it crossed a sector boundary, move it fully into the next sector
      substr($joliet, $pos, 0, "\0"x _remaining_sector_bytes($pos))
         if _sector_of($pos) != _sector_of(length $joliet);
   }
   # verify correct size
   _round_to_whole_sector(length $data) == $dir->file->size
      or croak "BUG: encoded directory is ".length($data)." bytes but should be ".$dir->file->size;
   _round_to_whole_sector(length $joliet) == $dir->joliet_file->size
      or croak "BUG: encoded joliet directory is ".length($data)." bytes but should be ".$dir->joliet_file->size;
   $dir->file->{data}= \$data;
   $dir->joliet_file->{data}= \$joliet;
}

# The path_table encoding algorithm performs a depth-first iteration because the entries refer
# to the parent using a index into the table, so the entry needs to already exist.
# This depth-first traversal also supports the feature of allowing directories to form an
# acyclic graph, in which case one directory could get traversed multiple times; the path table
# needs to have every distinct path indexed.

sub _calc_path_table_size($self) {
   my @paths= ( [ $self->root, 0 ] );
   my $i= 0;
   my ($size, $jsize)= (10, 10); # start with root dir, always 10 bytes
   while ($i < @paths) {
      my ($dir, $parent_id)= @{ $paths[$i++] };
      for my $ent ($dir->entries->@*) {
         if ($ent->{dir}) {
            $size += $ent->{path_table_size};
            $jsize+= $ent->{path_table_jsize};
            push @paths, [ $i, $ent->{dir} ]; # 1-based index, which works because ++ above
         }
      }
   }
   $self->{path_tables}= {
      le  => $self->_new_file('(path_table LE)', size => $size),
      be  => $self->_new_file('(path_table BE)', size => $size),
      jle => $self->_new_file('(path_table Joliet,LE)', size => $jsize),
      jbe => $self->_new_file('(path_table Joliet,BE)', size => $jsize),
   };
}

sub _pack_path_tables($self) {
   # Need packed for both 8.3 filenames and Joliet filenames, and encoded both little-endian
   # and big-endian, for 4 total path tables.
   my ($le, $be, $jle, $jbe);
   $le= $jle= pack 'C C V v a2', 10, 0, $self->root->file->extent_lba, 1, '';
   $be= $jbe= pack 'C C N n a2', 10, 0, $self->root->file->extent_lba, 1, '';
   my @paths= ( [ $self->root, 1 ] );
   my $i= 0;
   my ($size, $jsize)= (0, 0);
   while ($i < @paths) {
      my ($dir, $parent_id)= @{ $paths[$i++] };
      for my $ent ($dir->entries->@*) {
         if ($ent->{dir}) {
            my @vals= ( $ent->{path_table_size}, 0, $ent->{dir}->file->extent_lba, $parent_id, $ent->{shortname} );
            $le .= pack 'C C V v a'.($ent->{path_table_size}-8), @vals;
            $be .= pack 'C C N n a'.($ent->{path_table_size}-8), @vals;
            my $jname= encode('UTF-16BE', $ent->{name}, Encode::FB_CROAK | Encode::LEAVE_SRC);
            @vals= ( $ent->{path_table_jsize}, 0, $ent->{dir}->joliet_file->extent_lba, $parent_id, $jname );
            $jle .= pack 'C C V v a*', @vals;
            $jbe .= pack 'C C N n a*', @vals;
         }
      }
   }
   my $path_tables= $self->{path_tables};
   croak "BUG: wrong encoded path_table size"
      unless length $le == $path_tables->{le}->size && length $be == $path_tables->{be}->size
      && length $jle == $path_tables->{jle}->size && length $jbe == $path_tables->{jbe}->size;
   $path_tables->{le}{data}= \$le;
   $path_tables->{be}{data}= \$be;
   $path_tables->{jle}{data}= \$jle;
   $path_tables->{jbe}{data}= \$jbe;
}

sub _pack_iso_volume_datetime($epoch) {
   my @t = gmtime($epoch); # UTC
   my $year = $t[5] + 1900;
   my $mon  = $t[4] + 1;
   my $mday = $t[3];
   my $hour = $t[2];
   my $min  = $t[1];
   my $sec  = $t[0];
   my $centi = ($epoch * 100 % 100);  # hundredths of seconds
   my $tz = 0;     # UTC → offset=0, units of 15 min
   return sprintf("%04d%02d%02d%02d%02d%02d%02d%c",
      $year, $mon, $mday, $hour, $min, $sec,
      $centi, $tz,
   );
}

our @vol_desc_fields = (
   # field_name | offset | size | pack-code | default
   [ type_code        => 0x000,   1, 'C',  1 ],
   [ std_id           => 0x001,   5, 'A', 'CD001' ],
   [ version          => 0x006,   1, 'C',  1 ],
   #[ unused1         => 0x007,   1, 'C',   0 ],
   [ system_id        => 0x008,  32, 'E', '' ], # system which uses sectors 0-15
   [ volume_id        => 0x028,  32, 'E', '' ],
   #[ unused2         => 0x048,   8, 'a8',  '' ],
   [ volume_space     => 0x050,   4, 'V' ],
   [ volume_space     => 0x054,   4, 'N' ],
   [ escape_sequences => 0x058,  32, 'a', '' ],
   [ vol_set_sz       => 0x078,   2, 'v', 1 ],
   [ vol_set_sz       => 0x07A,   2, 'n', 1 ],
   [ seq_no           => 0x07C,   2, 'v', 1 ],
   [ seq_no           => 0x07E,   2, 'n', 1 ],
   [ blk_sz           => 0x080,   2, 'v', 2048 ],
   [ blk_sz           => 0x082,   2, 'n', 2048 ],
   [ path_sz          => 0x084,   4, 'V' ],
   [ path_sz          => 0x088,   4, 'N' ],
   [ path_le          => 0x08C,   4, 'V' ],
   [ path_le_opt      => 0x090,   4, 'V', 0 ],
   [ path_be          => 0x094,   4, 'N' ],
   [ path_be_opt      => 0x098,   4, 'N', 0 ],
   [ root_dirent      => 0x09C,  34, 'a' ],
   [ vol_set_id       => 0x0BE, 128, 'E', '' ],
   [ pub_id           => 0x13E, 128, 'E', '' ],
   [ prep_id          => 0x1BE, 128, 'E', '' ],
   [ app_id           => 0x23E, 128, 'E', '' ],
   [ copy_id          => 0x2BE,  37, 'E', '' ],
   [ abs_id           => 0x2E3,  37, 'E', '' ],
   [ bib_id           => 0x308,  37, 'E', '' ],
   [ creation_ts      => 0x32D,  17, 'a', '0'x16 ],
   [ modification_ts  => 0x33E,  17, 'a', '0'x16 ],
   [ expiration_ts    => 0x34F,  17, 'a', '0'x16 ],
   [ effective_ts     => 0x360,  17, 'a', '0'x16 ],
   [ file_struct      => 0x371,   1, 'C', 1 ],
   #[ unused3         => 0x372,   1, 'C', 0 ],
);
sub _append_pack_args($pack, $vals, $ofs, $fields, $attrs, $charset=undef) {
   for (@$fields) {
      my ($name, $field_ofs, $size, $code, $default)= @$_;
      my $val= $attrs->{$name} // $default
         // croak "No value supplied for $name, and no default";
      if ($code eq 'E') { # 'E' is a virtual code I'm using to mean "Encoded space-padded string"
         if ($charset) {
            # pad with encoded spaces
            $val= substr(encode($charset, $val.(' 'x$size), Encode::FB_CROAK), 0, $size);
         }
         $code= 'A';
      }
      if (uc $code eq 'A') {
         utf8::is_utf8($val) or utf8::downgrade($val, 1)
            or croak "Wide character supplied for attribute $name";
         carp "$name will be truncated" if length($val) > $size;
         $code .= $size;
      }
      push @$pack, '@'.($ofs+$field_ofs).$code;
      push @$vals, $val;
   }
}

sub _pack_volume_descriptor($self, $attrs) {
   $attrs->{system_id}    //= $self->system // uc($^O);
   $attrs->{volume_id}    //= $self->volume_label // 'CDROM';
   $attrs->{volume_space} //= $self->{max_used_lba}+1;
   $attrs->{path_sz}      //= $attrs->{path_le_file}->size;
   $attrs->{path_le}      //= $attrs->{path_le_file}->extent_lba;
   $attrs->{path_be}      //= $attrs->{path_be_file}->extent_lba;
   $attrs->{root_dirent}  //= _encode_dirent("\0", $attrs->{root_file});
   $attrs->{creation_ts}     //= _pack_iso_volume_datetime($attrs->{btime});
   $attrs->{modification_ts} //= _pack_iso_volume_datetime($attrs->{mtime});
   $attrs->{effective_ts}    //= _pack_iso_volume_datetime(time);
   $attrs->{vol_set_id}   //= $self->volume_set // $self->volume_label;
   $attrs->{pub_id}       //= $self->publisher;
   $attrs->{prep_id}      //= __PACKAGE__.' '.$VERSION;
   $attrs->{app_id}       //= $self->application;
   # Find the filenames of the metadata files
   my $is_joliet= ($attrs->{type_code}//0) == 2;
   $attrs->{copy_id} //= _get_metadata_filename($self, $self->copyright_file, $is_joliet);
   $attrs->{abs_id}  //= _get_metadata_filename($self, $self->abstract_file, $is_joliet);
   $attrs->{bib_id}  //= _get_metadata_filename($self, $self->bibliographic_file, $is_joliet);

   my (@pack, @vals);
   _append_pack_args(\@pack, \@vals, 0, \@vol_desc_fields, $attrs, $is_joliet? 'UTF16-BE' : undef);
   pack join(' ', @pack), @vals;
}

sub _get_metadata_filename($self, $spec, $is_joliet) {
   my $ent;
   if (blessed($spec) && $spec->can('extent_lba')) { # file object
      ($ent)= grep $_->{file} == $spec, $self->root->entries->@*
         or croak "Can't find ".$spec->name." in root directory";
   } else {
      return '' unless length $spec;
      $ent= $self->root->entry($spec) // croak "Can't find $spec in root directory";
   }
   !$is_joliet? $ent->{shortname}.';1' : $ent->{name};
}

sub _pack_primary_volume_descriptor($self, $attrs={}) {
   $attrs->{root_file}= $self->root->file;
   $attrs->{path_le_file}= $self->{path_tables}{le};
   $attrs->{path_be_file}= $self->{path_tables}{be};
   $self->_pack_volume_descriptor($attrs);
}

sub _pack_joliet_volume_descriptor($self, $attrs={}) {
   $attrs->{type_code}= 2;
   $attrs->{escape_sequences} = "%/E"; # Level 3 of Joliet, UCS-2BE
   $attrs->{root_file}= $self->root->joliet_file;
   $attrs->{path_le_file}= $self->{path_tables}{jle};
   $attrs->{path_be_file}= $self->{path_tables}{jbe};
   $self->_pack_volume_descriptor($attrs);
}

1;
