package Sys::Export::Exporter;

# VERSION
# ABSTRACT: base class for exporters, only used for 'isa' checks

use v5.26;
use warnings;

1;
__END__

=head1 INTERFACE

Every exporter should support the following attributes:

=over

=item src

A source filesystem path

=item dst

A destination path, or object having methods 'add' and 'finish' (like Sys::Export::CPIO)

=back

and the following methods:

=over

=item add

=item finish

=back
