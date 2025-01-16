use Test2::V0;
use v5.36;
use Sys::Export::CPIO;

my $buffer;
my $cpio= Sys::Export::CPIO->new(\$buffer);
$cpio->append({ name => '.', mode => 0x41ED, nlink => 2 });
is( $buffer, "07070100000001000041ED0000000000000000000000020000000000000000000000000000000000000000000000000000000200000000.\0",
   'encoding of directory "."'
);
my $ofs= length $buffer;

$cpio->append({ name => 'dev', mode => 0x41ED, nlink => 4 });
is( substr($buffer, $ofs),
   "07070100000002000041ED0000000000000000000000040000000000000000000000000000000000000000000000000000000400000000dev\0\0\0",
   'encoding of directory "dev"'
);
$ofs= length $buffer;

$cpio->append({ name => 'dev/null', mode => 0x21B6, rdev => 0x103, mtime => 0x45CCDDE3 });
is( substr($buffer, $ofs),
   "07070100000003000021B600000000000000000000000145CCDDE300000000000000000000000000000001000000030000000900000000dev/null\0\0",
   'encoding of device "dev/null"'
);
$ofs= length $buffer;

done_testing;

   