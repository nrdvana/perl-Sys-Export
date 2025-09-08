use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use POSIX 'ceil';
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::VFAT qw( FAT12 FAT16 FAT32 );

# Additional tests if host contains fsck.vfat
chomp(my $fsck= `which fsck.vfat`);
my $have_fsck= length $fsck && -x $fsck;
sub fsck($fname) {
   SKIP: {
      skip "no fsck.vfat available" unless $have_fsck;
      my $cmd= "$fsck -v -n '$fname'";
      note `$cmd`;
      is( $?, 0, $cmd );
   }
}

subtest empty_fs => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::VFAT->new($tmp);
   $dst->finish;
   # one boot sector, two FATs, one root dir, one empty cluster
   is( -s $tmp, 512*5, 'minimal fs size' );
   fsck($tmp);
};

subtest one_file => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::VFAT->new($tmp);
   $dst->add([ file => "README.TXT", "Hello World!\r\n" ]);
   $dst->finish;
   # one boot sector, two FATs, one root dir, one used cluster
   is( -s $tmp, 512*5, 'fs size' );
   fsck($tmp);
};

subtest one_dir => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::VFAT->new($tmp);
   $dst->add([ dir => "a" ]);
   $dst->finish;
   is( -s $tmp, 512*5, 'fs size' ); # one boot sector, two FATs, one root dir, one used cluster
   fsck($tmp);
};

subtest root_dir_math => sub {
   # Root directory has one volume label dirent, no '.' or '..' entries, and 16 dirents per sector
   # Test crossing of threshold to more sectors.
   for (510, 511, 512) {
      my $tmp= File::Temp->new;
      my $dst= Sys::Export::VFAT->new($tmp);
      $dst->add([ file => "$_.TXT", "Some Data" ]) for 1..$_;
      $dst->finish;
      # one boot sector, two FATs 2 sec each, root dir in 32 sec, 511 used clusters
      is( -s $tmp, 512*(1 + 2*2 + ceil((1+$_)/16) + $_), "fs size $_ root entries" );
      fsck($tmp);
   }
};

subtest device_addr_placement => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::VFAT->new($tmp);
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
      my $token2= "UniqueString9876543210";

      $dst->add([ file => "TEST.DAT", $token, { device_align => $align }]);
      $dst->add([ file => "TEST2.DAT", $token2, { device_align => $align*2, device_offset => \my $addr }]);
      $dst->finish;

      my $img= do { $tmp->seek(0,0); local $/; <$tmp> };
      my $offset= index($img, $token);
      my $offset2= index($img, $token2);
      note sprintf "image is 0x%X bytes, found token at 0x%X, token2 at 0x%X, align 0x%X",
         length($img), $offset, $offset2, $align;

      ok( $offset > 0, 'offset > 0' );
      is( $offset & ($align-1), 0, "TEST.DAT aligned to $align" );
      is( $offset2 & (($align*2)-1), 0, "TEST2.DAT aligned to ".($align*2) );
      is( $addr, $offset2, 'reported location of TEST2.DAT' );
   }
};

subtest test_mounts => sub {
   skip_all 'Set TEST_MOUNTS=1 to enable tests that call "mount"'
      unless $ENV{TEST_MOUNTS};

   my $tmp= File::Temp->newdir;
   my $dst= Sys::Export::VFAT->new(filename => "$tmp/fs");
   $dst->add([ dir => "a" ]);
   $dst->add([ dir => "a/b" ]);
   $dst->add([ dir => "a/b/c" ]);
   my $data= "Example Data";
   $dst->add([ file => "a/b/c/.d/config", $data ]);
   $dst->finish;
   
   mkdir "$tmp/mnt" or die "mkdir: $!";
   if (is( system('mount', -t => 'vfat', -o => 'loop', "$tmp/fs", "$tmp/mnt"), 0, "mount $tmp/mnt" )) {
      ok( -d "$tmp/mnt/a/b/c/.d", 'a/b/c/d exist' );
      ok( -f "$tmp/mnt/a/b/c/.d/config", "a/b/c/.d/config exists" );
      is( slurp("$tmp/mnt/a/b/c/.d/config"), $data, 'a/b/c/.d/config content' );
      is( system('umount', "$tmp/mnt"), 0, "umount $tmp/mnt" );
   }
};

done_testing;

   