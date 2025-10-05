package Sys::Export::ISO9660Hybrid;

# VERSION
# ABSTRACT: Write ISO9660 filesystem overlaid on MBR+GPT partition EFI filesystem

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export qw( :isa write_file_extent expand_stat_shorthand round_up_to_multiple );
use Sys::Export::LogAny '$log';
use Sys::Export::ISO9660 qw( BOOT_EFI );
use Sys::Export::VFAT;
use constant {
   ISO_SECTOR_SIZE => Sys::Export::ISO9660::LBA_SECTOR_SIZE,
   GPT_TYPE_ESP    => 'C12A7328-F81F-11D2-BA4B-00A0C93EC93B',
   GPT_TYPE_GRUB   => '21686148-6449-6E6F-744E-656564454649',
};

=head1 SYNOPSIS

  my $dst= Sys::Export::ISO9660Hybrid->new(
    output => $filename_or_handle,
  );   
  $dst->add(...); # add directory entries, cached in memory
  $dst->finish;   # builds both a FAT32 and ISO9660 filesystem

=head1 DESCRIPTION

This module helps you generate a "isohybrid" image which is both an iso9660 filesystem and a
MBR and GPT-labeled disk with one VFAT EFI partition.  Unlike with the C<xorriso> tool, your
files are visible in both filesystems simultaneously.

=head2 Legacy BIOS, Disk

Legacy i386 BIOS executes the first 446 bytes of the first sector as i386 instructions.  Those
instructions are free to do whatever they want.  GRUB 2 uses them to read a GPT partition label
and locate a partition where the rest of the boot loader is found.  The rest of the grub boot
loader then locates the EFI partition, and finds the main boot loader files in C<< boot/grub >>
of that volume.

=head2 Legacy BIOS, CDROM

Legacy i386 BIOS looks through the CDROM Volume Descriptor entries looking for an entry that
describes an extent which is a virtual floppy disk image.  It then loads that extent as if it
were a floppy disk, which means it essentially just starts executing it.
This image is large enough that it doesn't require Grub to be split into two parts.
Grub then locates the EFI partition, and finds the main boot loader files in C<< boot/grub >>
of that volume.

=head2 UEFI, Disk

UEFI expects a GPT-labeled disk with a special EFI System Partition which is formatted as VFAT
and which contains a file C<< \EFI\BOOT\BOOTX64.EFI >>.  It executes this file as an EFI
application.

=head2 UEFI, CDROM

UEFI looks through the CDROM Volume Descriptors for an entry that lists an extent of sectors
containing a EFI VFAT filesystem.  This extent is essentially the same as a partition.
It then loads C<< \EFI\BOOT\BOOTX64.EFI >> the same as if it were a disk.

=head2 Hybridization

All the specifications above can co-exist in the same disk image.  CDROM images leave the first
16 sectors empty, which is enough for a GPT partition data structure, and GPT leaves the first
sector empty which is enough for the 446 bytes of legacy boot code.  The GPT label can list
partitions that exist anywhere in the image, and the CDROM Volume Descriptors can refer to
extents anywhere in the image, so the EFI partition can be referenced by each of them.
Additionally, the CDROM's filesystem can refer to extents anywhere on the image, so it can
refer to the bodies of VFAT files as long as they are aligned to 2KiB boundaries.

=constructor new

  $fat= Sys::Export::ISO9660Hybrid->new($filename_or_handle);
  $fat= Sys::Export::ISO9660Hybrid->new(%attrs);
  $fat= Sys::Export::ISO9660Hybrid->new(\%attrs);

This takes a list of attributes as a hashref or key/value list.  If there is exactly one
argument, it is treated as the filename attribute.

=cut

sub new($class, @attrs) {
   my %attrs= @attrs != 1? @attrs
            : isa_hash $attrs[0]? %{$attrs[0]}
            : isa_handle $attrs[0]? ( filehandle => $attrs[0] )
            : ( filename => $attrs[0] );
   my $self= bless {
         iso => Sys::Export::ISO9660->new,
         vfat => Sys::Export::VFAT->new,
         files => [],
      }, $class;
   # apply other attributes
   $self->$_($attrs{$_}) for keys %attrs;
   $self;
}

=attribute filename

Name of file (or device) to write.  If the file exists it will be truncated before writing.
If you want to write the filesystem amid existing data (like a partition table), pass a file
handle as C<filehandle>.

=attribute filehandle

Output filehandle to write.  The file will be enlarged if it is not big enough.

=cut

sub filename { @_ > 1? ($_[0]{filename}= $_[1]) : $_[0]{filename} }
sub filehandle { @_ > 1? ($_[0]{filehandle}= $_[1]) : $_[0]{filehandle} }

=attribute iso

An instance of L<Sys::Export::ISO9660>.

=attribute esp

An instance of L<Sys::Export::VFAT> for the EFI System Partition.

=cut

sub iso($self) { $self->{iso} }
sub esp($self) { $self->{esp} }

=attribute volume_label

Returns ISO volume_label.  If written, sets both C<iso> and C<esp> volume label attributes to
the new value.  Note that while ESP is usually given a volume label like "ESP", for a removable
read-only image you probably want to give it a distinct name to avoid being confused with the
ESP of your harddrive.

=cut

sub volume_label {
   if (@_ > 1) {
      $_[0]->iso->volume_label($_[1]);
      $_[0]->esp->volume_label($_[1]);
   }
   $_[0]->iso->volume_label;
}

=attribute boot_code

A string of bytes (scalar, scalar-ref, or C<FileData|Sys::Export::FileData>) which will be
written to sector 0.

=attribute partition_align

Power of 2 for aligning partitions.  Must be at least 512.  Default is 4096.

=attribute partitions

A list of partition specifications:

  [
    {
      type   => $guid,
      data   => $scalar_or_scalarref_or_FileData,
      guid   => $part_guid,             # random default
      offset => $starting_byte_offset,  # defaults to next available
      size   => $byte_length,           # defaults to data length
    },
    ...
  ]

These are written as GPT partitions.  This will always include one partition of type ESP_GUID.
You can use this to add the partition for GRUB stage 1.5, or any other partiton you want, or to
force the offset or size of the ESP.  If 'data' is undefined, L</finish> will calculate the
partition location but not write anything to it, allowing you to handle that yourself.

=method add

  $dst->add(\%fileinfo);

Adds a file or directory to both VFAT and ISO filesystems.  Returns the ISO file/directroy
object.

=cut

sub add($self, $fileinfo) {
   my $is_dir= S_ISDIR($fileinfo->{mode}||0);
   if ($is_dir) {
      $self->esp->add($fileinfo);
      return $self->iso->add($fileinfo);
   } else {
      my $vfile= $self->esp->add({ %$fileinfo, align => ISO_SECTOR_SIZE );
      # prevent ISO9660 from assigning a LBA to this file
      my $ifile= $self->iso->add({ %$fileinfo, device_offset => -1 });
      push @{$self->{files}}, [ $vfile, $ifile ];
      return $ifile;
   }
}

=method finish

  $dst->finish;

Calculates the size of the ISO filesystem directories, then calculates the size of the VFAT
filesystem, then updates the ISO filesystem to refer to extents within VFAT, then writes GPT
and MBR partition tables.

=cut

sub finish($self) {
   my $fh= $self->filehandle;
   if (!$fh) {
      defined $self->filename or croak "Must set filename or filehandle attributes";
      open $fh, '+>', $self->filename
         or croak "open: $!";
   }
   my ($iso, $esp)= ($self->iso, $self->esp);
   $iso->filehandle($fh);
   $esp->filehandle($fh);
   # Add the El Torrito ESP entry, but we don't know the offset yet so leave that as -1
   my $esp_catalog_entry= $iso->add_boot_catalog_entry(
      platform_id => BOOT_EFI,
      device_offset => -1,
      size => 0,
   );
   # Find out the size of every ISO directory, and the path tables
   $iso->_calc_dir_sizes;
   $iso->_calc_path_table_size;
   # Choose LBA extents for all the directory structures and files other than the shared ones
   # we marked as LBA -1
   $iso->_choose_file_extents;
   # Now we know how much space the ISO structures and filesystem occupy, and can place all
   # partitions after that.
   my $ofs= ($iso->{max_used_lba}+1) * ISO_SECTOR_SIZE;
   for my $p ($self->partitons->@*) {
      if (!defined $p->{offset}) {
         $ofs= round_up_to_multiple($ofs, $self->partition_align);
         $p->{offset}= $ofs;
         if ($p->{type} eq GPT_TYPE_ESP) {
            # Now we know the device offset for the partition containing VFAT
            $esp->device_offset($p->{offset});
            # Now we can finalize the VFAT and get device addresses for all its files
            $esp->finalize;
            $p->{size}= $esp->size;
            $esp_catalog_entry->{file}->extent_lba($ofs / ISO_SECTOR_SIZE);
            $esp_catalog_entry->{file}->size($p->{size});
         }
         $ofs += $p->{size};
      }
   }
   for ($self->{files}->@*) {
      my ($vfile, $ifile)= @$_;
      croak "BUG: unaligned file" if $vfile->device_offset % ISO_SECTOR_SIZE;
      $ifile->extent_lba($vfile->device_offset / ISO_SECTOR_SIZE);
   }
   # Now we can finalize the ISO9660
   $iso->finalize;
   ...;
   # Now write the partition table
   # Make sure the boot_code didn't have data in the partition tables
   my $sector0= pack('a512', ${$self->boot_code});
   carp "boot_code contains nonzero bytes in partition table area"
      unless length($sector0) <= 446 || substr($sector0, 446, 64) eq "\0"x64;
   # "Protective MBR" for GPT
   $sector0= pack 'a446 NNVV @510 v',
      $sector0, # keep first 446 bytes
      0x00000200, 0xEEFFFFFF, 1, min(0xFFFFFFFF, $img_size),
      0xAA55; # signature at end of MBR
   write_file_extent($fh, 0, $sector0);
   ...; #encode partition table header
   # Each partition for GPT
   for my $p ($self->partitons->@*) {
      ...
   }
   # ensure it ends before offset 2048*16
   write_file_extent($fh, 512, $gpt);
}

our @gpt_header_fields= (
   [ GPT_signature       =>    0,  8, 'a8', 'EFI PART' ],
   [ GPT_revision        =>  0x8,  4, 'V' ],
   [ GPT_header_size     =>  0xC,  4, 'V', 512 ],
   [ GPT_header_crc32    => 0x10,  4, 'V' ],
   [ GPT_header_lba      => 0x18,  8, 'Q<', 2 ],
   [ GPT_header2_lba     => 0x20,  8, 'Q<' ],
   [ GPT_first_block     => 0x28,  8, 'Q<' ],
   ] GPT_last_block      => 0x30,  8, 'Q<' ],
   [ GPT_disk_guid       => 0x38, 16, 'a16' ],
   [ GPT_entry_lba       => 0x48,  8, 'a8' ],
   [ GPT_entry_count     => 0x50,  4, 'V' ],
   [ GPT_entry_size      => 0x54,  4, 'V', 128 ],
   [ GPT_entry_crc32     => 0x58,  4, 'V' ],
);
our @got_entry_fields= (
   [ type_guid           =>    0, 16, 'a16' ],
   [ partition_guid      => 0x10, 16, 'a16' ],
   [ extent_lba          => 0x20,  8, 'Q<' ],
   [ end_lba
);
sub _append_pack_args($pack, $vals, $ofs, $fields, $attrs) {
   for (@$fields) {
      push @$pack, '@'.($ofs+$_->[1]).$_->[3];
      push @$vals, $attrs->{$_->[0]} // $_->[4]
         // croak "No value supplied for $_->[0], and no default";
   }
}

sub _pack_gpt_header($self, %attrs) {
   ...
}

sub _pack_gpt_entry($self, %attrs) {
   # GPT header
   pack 
}

1;
