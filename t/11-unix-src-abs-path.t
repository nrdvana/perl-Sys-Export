use Test2::V0;
use v5.36;
use File::Temp;
use Sys::Export::Unix;
use autodie;

# Set up some symlinks
my $tmp= File::Temp->newdir;
mkdir "$tmp/usr";
mkdir "$tmp/usr/bin";
mkdir "$tmp/usr/local";
symlink "/usr/bin", "$tmp/bin";
symlink "/bin", "$tmp/usr/local/bin";
symlink "../bin", "$tmp/usr/local/sbin";

my $exporter= Sys::Export::Unix->new(src => $tmp, dst => File::Temp->newdir);
note "exporter src: '".$exporter->src."'";

for (
   [ 'usr/local/bin'  => 'usr/bin' ],
   [ 'usr/local/sbin' => 'usr/bin' ],
) {
   is( $exporter->_src_abs_path( $_->[0] ), $_->[1], $_->[0] );
}

done_testing;
