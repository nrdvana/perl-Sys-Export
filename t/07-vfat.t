use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::VFAT qw( FAT12 FAT16 FAT32 );

subtest device_addr_placement => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::VFAT->new(filename => "$tmp");
   my $token= "UniqueString".("0123456789"x50);
   my $addr= 4096*256;
   $dst->add([ file => "/TEST.DAT", $token, { device_offset => $addr }]);
   $dst->finish;

   # TODO: aim for exact minimal size
   #is( -s $tmp, $addr + $dst->geometry->bytes_per_cluster, 'filesystem sized to exactly hold file' );
   ok( -s $tmp > $addr + $dst->geometry->bytes_per_cluster, 'filesystem large enough' );

   sysseek $tmp, $addr, 0 or die "seek: $!";
   sysread $tmp, my $buf, length $token or die "read: $!";
   is( $buf, $token, 'Found token at addr' );
};

subtest device_align_placement => sub {
   for my $align (1<<9, 1<<10, 1<<11, 1<<12, 1<<13, 1<<14) {
      my $tmp= File::Temp->new;
      my $dst= Sys::Export::VFAT->new($tmp);
      my $token= "UniqueString".("0123456789"x50);

      $dst->add([ file => "/TEST.DAT", $token, { device_align => $align }]);
      $dst->finish;

      my $img= do { $tmp->seek(0,0); local $/; <$tmp> };
      my $offset= index($img, $token);
      note sprintf "image is 0x%X bytes, found token at 0x%X, align 0x%X", length($img), $offset, $align;

      ok( $offset > 0, 'offset > 0' );
      ok( ($offset & ($align-1)) == 0, "aligned to $align" );
   }
};

subtest test_mounts => sub {
   skip_all 'Set TEST_MOUNTS=1 to enable tests that call "mount"'
      unless $ENV{TEST_MOUNTS};

   my $tmp= File::Temp->newdir;
   my $dst= Sys::Export::VFAT->new(filename => "fs");
   $dst->add([ dir => "a" ]);
   $dst->add([ dir => "a/b" ]);
   $dst->add([ dir => "a/b/c" ]);
   my $data= "Example Data";
   $dst->add([ file => "a/b/c/.d/config", $data ]);
   $dst->finish;
   
   mkdir "$tmp/mnt" or die "mkdir: $!";
   if (is( system('mount', "$tmp/fs", "$tmp/mnt"), 0, "mount $tmp/mnt" )) {
      ok( -d "$tmp/mnt/a/b/c/.d", 'a/b/c/d exist' );
      ok( -f "$tmp/mnt/a/b/c/.d/config", "a/b/c/.d/config exists" );
      is( slurp("$tmp/mnt/a/b/c/.d/config"), $data, 'a/b/c/.d/config content' );
      is( system('umount', "$tmp/mnt"), 0, "umount $tmp/mnt" );
   }
};

done_testing;

   