use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::VFAT qw( FAT12 FAT16 FAT32 );

subtest geometry => sub {
   subtest fat12_math => sub {
      my $g= Sys::Export::VFAT::Geometry->new(cluster_count => 4000);
      is( $g, object {
         call bytes_per_sector      => 512;
         call sectors_per_cluster   => 8;
         call dirent_per_sector     => 512/32;
         call dirent_per_cluster    => 512*8/32;
         call bits                  => FAT12;
         call reserved_sector_count => 1;
         call fat_count             => 2;
         call fat_sector_count      => 12;
         call root_dirent_count     => 512;
         call root_sector_count     => 32;
         call data_first_sector     => 1 + 12 + 12 + 32;
         call total_sector_count    => 1 + 12 + 12 + 32 + (4000 * 8);
      }, 'default geometry for 5000 clusters' );
   };

   my @compare_object_fields= qw( bytes_per_sector sectors_per_cluster dirent_per_sector
      dirent_per_cluster bits reserved_sector_count fat_count fat_sector_count
      root_dirent_count root_sector_count data_first_sector total_sector_count );
   subtest fat12_pack_unpack => sub {
      for ($ENV{TEST_ALL_PERMUTATIONS}? (1..4084) : (1..500, 3500..4084)) {
         my $g= Sys::Export::VFAT::Geometry->new(cluster_count => $_, exact_cluster_count => 1);
         my $buf_ref= $g->pack;
         is( Sys::Export::VFAT::Geometry->unpack($buf_ref), object {
            call $_ => $g->$_
               for @compare_object_fields;
         }, "cluster_count => $_" )
            or last; # prevent massive test failure spam
      }
   };

   subtest fat16_pack_unpack => sub {
      for ($ENV{TEST_ALL_PERMUTATIONS}? (4085..65524) : (4085..6000, 64000..65524)) {
         my $g= Sys::Export::VFAT::Geometry->new(cluster_count => $_, exact_cluster_count => 1);
         my $buf_ref= $g->pack;
         is( Sys::Export::VFAT::Geometry->unpack($buf_ref), object {
            call $_ => $g->$_
               for @compare_object_fields;
         }, "cluster_count => $_" )
            or last; # prevent massive test failure spam
      }
   };
};

done_testing;

   