package Test2AndUtils;
use v5.26;
use warnings;
use Test2::V0 '!subtest';
use Test2::Tools::Subtest 'subtest_streamed';
use experimental qw( signatures );
use parent 'Test2::V0';
use File::Temp;
use IO::Handle;
use Log::Any::Adapter 'TAP';

our @EXPORT= (
   @Test2::V0::EXPORT,
   qw( explain unindent mkfile slurp escape_nonprintable hexdump )
);

# Test2 runs async by default, which messes up the relation between warnings and the test
# that generated them.  Streamed generates output sequentially.
*subtest= \&subtest_streamed;

# Use Data::Printer if available, but fall back to Data::Dumper
eval q{
   use Data::Printer;
   sub explain { Data::Printer::np(@_) }
   1
} or eval q{
   use Data::Dumper;
   sub explain { Data::Dumper->new(\@_)->Terse(1)->Indent(1)->Sortkeys(1)->Dump }
   1
} or die $@;

# Perl didn't get <<~'x' until 5.28, so this lets you write an indented here-block and
# then remove the common indent from all lines.
sub unindent {
   my ($indent)= ($_[0] =~ /^(\s+)/);
   (my $x= $_[0]) =~ s/^$indent//mg;
   $x;
}

# Convert data strings to and from C / Perl backslash notation.
# Not exhaustive, just hit the most common cases and hex-escape the rest.

my %escape_to_char = ( "\\" => "\\", r => "\r", n => "\n", t => "\t" );
my %char_to_escape = reverse %escape_to_char;

sub escape_nonprintable($str) {
   $str =~ s/([^\x21-\x7E])/ defined $char_to_escape{$1}? "\\".$char_to_escape{$1} : sprintf("\\x%02X", ord $1) /ge;
   return $str;
}

sub unescape_nonprintable($str) {
   $str =~ s/\\(x([0-9A-F]{2})|.)/ defined $2? chr hex $2 : $escape_to_char{$1} /ge;
   return $str;
}

sub mkfile($name, $data, $mode=undef) {
   open my $fh, '>:raw', $name or die "open(>$name): $!";
   $fh->print($data) or die "write($name): $!";
   $fh->close or die "close($name): $!";
   chmod $mode, $name or die "chmod($name, $mode): $!"
      if defined $mode;
   1;
}

sub slurp($name) {
   open my $fh, '<:raw', $name or die "open(<$name): $!";
   local $/;
   my $ret= scalar <$fh>;
   close $fh or die "close($name): $!";
   $ret;
}

sub hexdump($data) {
   my @lines= unpack '(a16)*', $data;
   my $skipping= 0;
   for my $i (0..$#lines) {
      if ($lines[$i] eq "\0"x16) {
         $lines[$i]= $skipping? undef : '*';
         $skipping= 1;
      } else {
         $skipping= 0;
         my $ascii= $lines[$i] =~ s/[\0-\x1F\x7F-\xFF]/./gr;
         my $hex= join ' ', unpack '(H2)*', $lines[$i];
         $lines[$i]= sprintf "%8X  %s %s |%s|",
            $i*16, substr($hex, 0, 23), substr($hex, 24), $ascii;
      }
   }
   return join "\n", grep defined, @lines;
}

1;

