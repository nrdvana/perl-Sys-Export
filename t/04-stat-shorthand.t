use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Sys::Export qw( :isa :stat_modes :stat_tests expand_stat_shorthand );

umask 022;
my @tests= (
   [ [ file => "test", "DATA" ], { mode => S_IFREG|0644, name => "test", data => "DATA" } ],
   [ [ file => "test", { data_path => "test2" } ],
                                 { mode => S_IFREG|0644, name => "test", data_path => "test2" } ],
   [ [ sym  => "foo",  "bar"  ], { mode => S_IFLNK|0777, name => "foo",  data => "bar"  } ],
   [ [ dir  => "bin"          ], { mode => S_IFDIR|0755, name => "bin",  } ],
   [ [ blk  => "sda",  [8,0]  ], { mode => S_IFBLK|0644, name => "sda",  major => 8, minor => 0 } ],
   [ [ chr  => "null", "1,3"  ], { mode => S_IFCHR|0644, name => "null", major => 1, minor => 3 } ],
   [ [ fifo => "queue"        ], { mode => S_IFIFO|0644, name => "queue" } ],
   [ [ sock => "service.sock" ], { mode => S_IFSOCK|0644,name => "service.sock" } ],
);

for (@tests) {
   is( { expand_stat_shorthand($_->[0]) }, $_->[1], join ' ', "shorthand @{$_->[0]}" );
}

done_testing;
