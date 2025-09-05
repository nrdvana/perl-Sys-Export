=head1 SYNOPSIS

  my $dst= Sys::Export::ISO9660Hybrid->new(
    output => $filename_or_handle,
    partitions => [
      { type => 'EFI', build => 'FAT32' },
      # the 9660 filesystem doesn't need to be assigned a partition, and can
      # just live in unallocated space, but you can give it one if you want.
      { type => 'CD', build => 'ISO9660' },
      ... # any other partitions you wanted
    ]
  );   
  $dst->add(...); # add directory entries, cached in memory
  $dst->finish;   # builds both a Fat32 and ISO9660 filesystem

=head1 DESCRIPTION

This module helps you generate a "isohybrid" image which is both an iso9660 filesystem and a
GPT-labeled disk with one Fat32 EFI partition.  Unlike with the C<xorriso> tool, both
filesystems' files B<refer to the same data blocks>, making the image smaller.

=cut
