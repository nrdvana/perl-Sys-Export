use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export qw( :isa :stat_modes :stat_tests expand_stat_shorthand );

umask 022;
my @tests= (
   [ [ file => "test", "DATA" ], { mode => S_IFREG|0644, name => "test", nlink => 1, data => "DATA" } ],
   [ [ sym  => "foo",  "bar"  ], { mode => S_IFLNK|0777, name => "foo",  nlink => 1, data => "bar"  } ],
   [ [ dir  => "bin"          ], { mode => S_IFDIR|0755, name => "bin",  nlink => 1 } ],
   [ [ blk  => "sda",  [8,0]  ], { mode => S_IFBLK|0644, name => "sda",  nlink => 1, major => 8, minor => 0 } ],
   [ [ chr  => "null", "1,3"  ], { mode => S_IFCHR|0644, name => "null", nlink => 1, major => 1, minor => 3 } ],
   [ [ fifo => "queue"        ], { mode => S_IFIFO|0644, name => "queue",nlink => 1 } ],
   [ [ sock => "service.sock" ], { mode => S_IFSOCK|0644,name => "service.sock",nlink => 1 } ],
);

for (@tests) {
   is( { expand_stat_shorthand($_->[0]) }, $_->[1], join ' ', "shorthand @{$_->[0]}" );
}

done_testing;
