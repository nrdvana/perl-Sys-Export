use Test2::V0;
use v5.36;
use File::Temp;
use Sys::Export::Unix;
use File::stat;
use Fcntl qw( S_IFDIR );
use autodie;

# Set up some symlinks
my $tmp= File::Temp->newdir;
mkdir "$tmp/usr";
mkdir "$tmp/usr/local";
chmod 0700, "$tmp/usr";
open my $out, '>', "$tmp/usr/local/datafile";
$out->print("Just some data");
$out->close;
symlink "./datafile", "$tmp/usr/local/datafile2";

my $exporter= Sys::Export::Unix->new(src => $tmp, dst => File::Temp->newdir);
note "exporter src: '".$exporter->src."' dst: '".$exporter->dst."'";

$exporter->add('usr/local/datafile');
$exporter->add('usr/local/datafile2');

is( stat($exporter->{dst_abs} . 'usr'),
   object {
      call mode => (S_IFDIR | 0700);
   },
   'usr'
);

done_testing;
