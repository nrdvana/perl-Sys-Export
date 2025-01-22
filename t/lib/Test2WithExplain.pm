package Test2WithExplain;
use v5.36;
use Test2::V0 '!subtest';
use Test2::Tools::Subtest 'subtest_streamed';
use parent 'Test2::V0';
our @EXPORT= (@Test2::V0::EXPORT, 'explain', 'mkfile');
*subtest= \&subtest_streamed;
eval q{
   use Data::Printer;
   sub explain { Data::Printer::np(@_) }
   1
} or eval q{
   use Data::Dumper;
   sub explain { Data::Dumper->new(\@_)->Terse(1)->Indent(1)->Sortkeys(1)->Dump }
   1
} or die $@;

sub mkfile($name, $content, $mode=undef) {
   open my $fh, '>', $name or die "open($name): $!";
   $fh->print($content) or die "write(<$name>, ...): $!";
   $fh->close or die "close(<$name>): $!";
   chmod $mode, $name or die "chmod($name, $mode): $!"
      if defined $mode;
   1;
}

1;
