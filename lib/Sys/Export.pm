package Sys::Export;

# VERSION
# ABSTRACT: Export a subset of an OS file tree, for chroot/initrd

use v5.36;
use Scalar::Util qw( blessed looks_like_number );
use Exporter 'import';
our @EXPORT_OK= qw( isa_exporter isa_export_dst isa_userdb isa_user isa_group );
our %EXPORT_TAGS= (
   isa => [qw( isa_exporter isa_export_dst isa_userdb isa_user isa_group )],
);

=head1 SYNOPSIS

=head1 DESCRIPTION

This module is designed to export a subset of a Linux filesystem to a new directory,
automatically detecting and including any libraries or interpreters required by the requested
subset, and optionally restructuring the directories and updating the copied files to refer
to the updated paths, when possible.

Currently, only Linux is fully supported.

=head1 EXPORTS

=head2 C<:isa> bundle

  use Sys::Export ":isa";

These boolean functions are useful for type inspection.

=over

=item isa_exporter

Is it an object and an instance of C<Sys::Export::Exporter>?

=item isa_export_dst

Is it an object which can receive exported files? (C<add> and C<finish> methods)

=item isa_userdb

Is it an instance of C<Sys::Export::Unix::UserDB>?

=item isa_user

Is it an instance of C<Sys::Export::Unix::UserDB::User>?

=item isa_group

Is it an instance of C<Sys::Export::Unix::UserDB::Group>?

=back

=cut

sub isa_exporter :prototype($) { blessed $_[0] && $_[0]->isa('Sys::Export::Unix') }
sub isa_export_dst :prototype($) { blessed $_[0] && $_[0]->can('add') && $_[0]->can('finish') }
sub isa_userdb :prototype($) { blessed($_[0]) && $_[0]->can('user') && $_[0]->can('group') }
sub isa_user  :prototype($) { blessed($_[0]) && $_[0]->isa('Sys::Export::Unix::UserDB::User') }
sub isa_group :prototype($) { blessed($_[0]) && $_[0]->isa('Sys::Export::Unix::UserDB::Group') }

1;
