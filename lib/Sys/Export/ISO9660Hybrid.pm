package Sys::Export::ISO9660Hybrid;

# VERSION
# ABSTRACT: Write ISO9660 filesystem overlaid on MBR+GPT partition VFAT filesystem

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export::ISO9660;
use Sys::Export::VFAT;
use constant { ISO_SECTOR_SIZE => Sys::Export::ISO9660::LBA_SECTOR_SIZE };

=head1 SYNOPSIS

  my $dst= Sys::Export::ISO9660Hybrid->new(
    output => $filename_or_handle,
  );   
  $dst->add(...); # add directory entries, cached in memory
  $dst->finish;   # builds both a FAT32 and ISO9660 filesystem

=head1 DESCRIPTION

This module helps you generate a "isohybrid" image which is both an iso9660 filesystem and a
MBR and GPT-labeled disk with one VFAT EFI partition.  Unlike with the C<xorriso> tool, both
filesystems' files B<refer to the same data blocks>, making the image smaller.

=cut

sub add($self, $fileinfo) {
   ...
   if ($is_dir) {
      $vfat->add($fileinfo);
      $iso->add($fileinfo);
   } else {
      my $vfile= $vfat->add({ %$fileinfo, align => ISO_SECTOR_SIZE );
      # prevent ISO9660 from assigning a LBA to this file
      my $ifile= $iso->add({ %$fileinfo, device_offset => -1 });
      push @{$self->{files}}, [ $vfile, $ifile ];
   }
   ...
}

sub finish($self) {
   ...
   # Find out the size of every directory, and the path tables
   $iso->_calc_dir_sizes;
   $iso->_calc_path_table_size;
   # Choose LBA extents for all the directory structures and files other than the shared ones
   # we marked as LBA -1
   $iso->_choose_file_extents;
   # Now we know the device offset for the partition containing VFAT
   $vfat->device_offset(($iso->{max_used_lba}+1) * ISO_SECTOR_SIZE);
   # Now we can finalize the VFAT and get device addresses for all its files
   $vfat->finalize;
   for ($self->{files}->@*) {
      my ($vfile, $ifile)= @$_;
      croak "BUG: unaligned file" if $vfile->device_offset % ISO_SECTOR_SIZE;
      $ifile->{extent_lba}= $vfile->device_offset / ISO_SECTOR_SIZE;
   }
   # Now we can finalize the ISO9660
   $iso->finalize;
   # Now write the partition table
   ...
}

1;
