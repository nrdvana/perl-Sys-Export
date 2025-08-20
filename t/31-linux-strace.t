#! /usr/bin/env perl
use v5.26;
use warnings;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
use experimental qw( signatures );
use Cwd 'abs_path';
use Fcntl 'S_IFREG';
use Sys::Export::Linux;

package Sys::Export::MockDst {
   sub new($class)        { bless { files => {} }, $class }
   sub files($self)       { $self->{files} }
   sub add($self, $attrs) { $self->{files}{$attrs->{name}}= $attrs; }
   sub finish($self)      {}
}

my $tmp= File::Temp->newdir;
my $dst= Sys::Export::MockDst->new();
my $exporter= Sys::Export::Linux->new(src => "/", dst => $dst, log => 'info');

skip_all "strace not supported in this environment"
   unless $exporter->_can_trace_deps;

$exporter->add(abs_path(__FILE__));

note "dep: $_" for keys $dst->files->%*;

ok( scalar(grep m{Sys/Export/Linux.pm\z}, keys $dst->files->%*), 'Saw dep Sys/Export/Linux.pm' );
ok( scalar(grep m{t/lib/Test2AndUtils.pm\z}, keys $dst->files->%*), 'Saw dep Test2AndUtils.pm' );

done_testing;
