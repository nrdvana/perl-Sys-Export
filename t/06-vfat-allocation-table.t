use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::VFAT::AllocationTable;

my $alloc= Sys::Export::VFAT::AllocationTable->new;
is( $alloc->alloc_range(10, 20), 10, 'alloc_range' );
is( $alloc->get_chain(10)->{invlist}, [ 10, 30 ], 'invlist' );

is( $alloc->alloc_contiguous(10), 30, 'alloc_contiguous' );
is( $alloc->get_chain(30)->{invlist}, [ 30, 40 ], 'invlist' );

is( $alloc->alloc(9), 2, 'alloc(9)' );
is( $alloc->get_chain(2)->{invlist}, [ 2, 10, 40, 41 ], 'invlist' );

is( $alloc->max_cluster_id, undef, 'max_cluster_id' );
is( $alloc->max_used_cluster_id, 40, 'max_used_cluster_id' );

is( $alloc->fat, [
   undef, undef, 3, 4, 5, 6, 7, 8, 9, 40,
   11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
   21, 22, 23, 24, 25, 26, 27, 28, 29, 0xFFFFFFF,
   31, 32, 33, 34, 35, 36, 37, 38, 39, 0xFFFFFFF,
   0xFFFFFFF
], 'fat array' );
   

is( [uc(unpack('H*', $alloc->pack)) =~ /(..)/g], [qw(
   FF FF FF 03 40 00 05 60 00 07 80 00 09 80 02
   0B C0 00 0D E0 00 0F 00 01 11 20 01 13 40 01
   15 60 01 17 80 01 19 A0 01 1B C0 01 1D F0 FF
   1F 00 02 21 20 02 23 40 02 25 60 02 27 F0 FF
   FF 0F
)], 'packed fat16 table' );

done_testing;

   