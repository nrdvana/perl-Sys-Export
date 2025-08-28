use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::VFAT qw( FAT12 FAT16 FAT32 );

subtest geometry => sub {
   my sub new_geom { Sys::Export::VFAT::Geometry->new(@_) }
   0 && subtest fat12_math => sub {
      my $g= new_geom(cluster_count => 4000);
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
   0 && subtest fat12_pack_unpack => sub {
      for ($ENV{TEST_ALL_PERMUTATIONS}? (1..4084) : (1..500, 3500..4084)) {
         my $g= new_geom(cluster_count => $_, exact_cluster_count => 1);
         my $buf_ref= $g->pack;
         is( Sys::Export::VFAT::Geometry->unpack($buf_ref), object {
            call $_ => $g->$_
               for @compare_object_fields;
         }, "cluster_count => $_" )
            or last; # prevent massive test failure spam
      }
   };

   0 && subtest fat16_pack_unpack => sub {
      for ($ENV{TEST_ALL_PERMUTATIONS}? (4085..65524) : (4085..4500, 65000..65524)) {
         my $g= new_geom(cluster_count => $_, exact_cluster_count => 1);
         my $buf_ref= $g->pack;
         is( Sys::Export::VFAT::Geometry->unpack($buf_ref), object {
            call $_ => $g->$_
               for @compare_object_fields;
         }, "cluster_count => $_" )
            or last; # prevent massive test failure spam
      }
   };

   # Test requesting that clusters start aligned to the volume
   subtest align_clusters => sub {
      for my $volume_offset (0, 512, 1024, 1536) {
         for my $sec_per_cl (1, 2, 4, 8, 16) {
            for my $cluster_count (1111, 5001, 70001) { # FAT12, FAT16, FAT32
               subtest "vol_ofs=$volume_offset,sec_per_cl=$sec_per_cl,cl=$cluster_count" => sub {
                  my %attr= (
                     volume_offset => $volume_offset,
                     bytes_per_sector => 512,
                     sectors_per_cluster => $sec_per_cl,
                     cluster_count => $cluster_count,
                     fat_count => 1,
                  );

                  # First test without requesting alignment
                  my $geom= new_geom(%attr);
                  note sprintf "data offset on media = 0x%X (sector 0x%X)",
                     $geom->data_media_offset,
                     $geom->data_first_sector;
                  note sprintf " reserved=0x%Xsec fat=0x%Xsec root=0x%Xsec",
                     $geom->reserved_sector_count,
                     $geom->fat_sector_count,
                     $geom->root_sector_count;
                  ok( ($geom->data_media_offset & 4095) != 0, 'cluster not aligned by default' );

                  # Now test with requesting alignment to 4K
                  $geom= new_geom(%attr, align_clusters => 4096);
                  note sprintf "data offset on media = 0x%X (sector 0x%X)",
                     $geom->data_media_offset,
                     $geom->data_first_sector;
                  note sprintf " reserved=0x%Xsec fat=0x%Xsec root=0x%Xsec",
                     $geom->reserved_sector_count,
                     $geom->fat_sector_count,
                     $geom->root_sector_count;
                  # Request media alignment numbers for 1K, 2K, 4K, 8K
                  for my $align (1<<10, 1<<11, 1<<12, 1<<13) {
                     my ($cl_align, $cl_ofs)= $geom->get_cluster_alignment_of_media_alignment($align);
                     note "align=$align, cl_align=$cl_align, cl_ofs=$cl_ofs"; 
                     is( $geom->get_cluster_media_offset($cl_align*2+$cl_ofs) & ($align-1), 0,
                        'Cluster '.($cl_align*2+$cl_ofs).' aligned to '.$align );
                     is( $geom->get_cluster_media_offset($cl_align*3+$cl_ofs) & ($align-1), 0,
                        'Cluster '.($cl_align*3+$cl_ofs).' aligned to '.$align );
                  }
               };
            }
         }
      }
   };
};

subtest test_mounts => sub {
   skip_all 'Set TEST_MOUNTS=1 to enable tests that call "mount"'
      unless $ENV{TEST_MOUNTS};
   
};

done_testing;

   