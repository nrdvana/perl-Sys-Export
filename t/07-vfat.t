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

subtest test_mounts => sub {
   skip_all 'Set TEST_MOUNTS=1 to enable tests that call "mount"'
      unless $ENV{TEST_MOUNTS};
   
};

done_testing;

   