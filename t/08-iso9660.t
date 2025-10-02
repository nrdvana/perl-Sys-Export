use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use POSIX 'ceil';
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export::ISO9660 qw( is_valid_shortname is_valid_joliet_name );

# The isoinfo utility can dump details of an ISO filesystem.
my $isoinfo= do { chomp(my $x= `which isoinfo`); $? == 0? $x : undef };

subtest is_valid_shortname => sub {
   ok( is_valid_shortname($_), "valid short '$_'" )
      for '12345678',
          '12345678.9AB',
          '123456~1.TXT';
   ok( !is_valid_shortname($_), "invalid short '$_'" )
      for '12345678.', '123456789.', '12345678.1234';
};

subtest is_valid_joliet_name => sub {
   ok( is_valid_joliet_name($_), "valid joliet '$_'" )
      for '12345678',
          '12345678.',
          '12345678.9AB',
          "&%'-_@~`.!()",
          '${}^#',
          '+,=[]';
   ok( !is_valid_joliet_name($_), "invalid joliet '$_'" )
      for '/', '\\', ';';
};

subtest empty_fs => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::ISO9660->new($tmp);
   $dst->finish;
   my $sectors= 16 # system
      + 3 # Volume Descriptors (primary, secondary, terminator)
      + 4 # 4x path table (LE, BE, Joliet LE, Joliet BE)
      + 2;# root dir, Joliet root dir
   is( -s $tmp, $sectors * 2048, 'minimal fs size' );
   note `$isoinfo -dJRf -i $tmp` if $isoinfo;
};

subtest readme_fs => sub {
   my $tmp= File::Temp->new;
   my $dst= Sys::Export::ISO9660->new(filename => $tmp, volume_label => 'TESTVOL');
   my $readme= <<END;
Hello World
-----------

Stuff and things.
END
   my $readme_file= $dst->add([ file => 'README.TXT', \$readme ]);
   $dst->abstract_file($readme_file);
   $dst->{default_time}= 946684800;
   $dst->finish;
   my $sectors= 16 # system
      + 3 # Volume Descriptors (primary, secondary, terminator)
      + 4 # 4x path table (LE, BE, Joliet LE, Joliet BE)
      + 2 # root dir, Joliet root dir
      + 1;# README.txt content
   is( -s $tmp, $sectors*2048, 'fs size with only README.TXT' );
   note `$isoinfo -d -i $tmp` if $isoinfo;
   `cp $tmp /tmp/README.iso && chmod go+r /tmp/README.iso`;
   subtest mount_fs => sub {
      skip_all 'Set TEST_MOUNT=1 to enable tests that mount the generated filesystem'
         unless $ENV{TEST_MOUNT};
      my $mnt= File::Temp->newdir;
      if (is( system('mount', '-r', -t => 'iso9660', -o => 'loop', "$tmp", "$mnt"), 0, "mount $tmp on $mnt" )) {
         ok( -f "$mnt/README.TXT", 'README.TXT exists' );
         is( slurp("$mnt/README.TXT"), $readme, 'README.TXT content' );
         is( system('umount', "$mnt"), 0, "umount $mnt" );
      }
   };
};

done_testing;

   