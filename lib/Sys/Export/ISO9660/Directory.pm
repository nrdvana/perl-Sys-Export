package Sys::Export::ISO9660::Directory;

# VERSION
# ABSTRACT: Represents a case-folded directory in ISO9660

use v5.26;
use warnings;
use experimental qw( signatures );
use parent 'Sys::Export::VFAT::Directory';
use Scalar::Util qw( dualvar weaken );

=constructor new

  $file= Sys::Export::ISO9660::Directory->new(%attributes);

Represents file (or directory) data to be encoded into the ISO image.

=cut

sub new($class, %attrs) {
   my $joliet_file= delete $attrs{joliet_file};
   my $self= $class->next::method(%attrs);
   $self->{joliet_file}= $joliet_file if defined $joliet_file;
   $self;
}

sub is_valid_name($self, $name) {
   Sys::Export::ISO9660::is_valid_joliet_name($name)
}
sub is_valid_shortname($self, $name) {
   Sys::Export::ISO9660::is_valid_shortname($name)
}
sub remove_invalid_shortname_chars($self, $name, $repl) {
   Sys::Export::ISO9660::remove_invalid_shortname_chars($name, $repl)
}

=attribute joliet_file

Unlike Files, Directories get encoded twice, once with a short filename and again with a long
unicode filename.  The directory short-name encoding is stored in C<file> and the joliet
encoding is stored in C<joliet_file>.

=cut

sub joliet_file { $_[0]{joliet_file} }

require Sys::Export::ISO9660;
1;
