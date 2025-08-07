package Sys::Export::PathMatch;

# ABSTRACT: Iterate matching paths in a tree
# VERSION

=head1 SYNOPSIS

  use Sys::Export::PathMatch 'paths';
  # Find all files and directories in a tree
  say for paths('usr/share/zoneinfo/**')->at('/')->all;
  
  # Find all directories in /my/chroot/usr
  say for paths('usr/**', sub { -d })->at('/my/chroot/')->all;
  
  # All large files in /var or /srv
  say for paths(['var/**', 'srv/**'], sub { -s > 1024**2 })->at('/')->all;

=head1 DESCRIPTION

This module is like a cross between L<File::Find> and C<DBIx::Class::ResultSet>.  The object
represents a set of patterns or coderef filters to apply to any paths relative to an
un-specified root directory.  You can perform unions and intersections on the sets, and
then root them at a real filesystem path to iterate the matches.

=head1 MATCH SPECIFICATIONS

=over

=item Strings

Simple strings are interpreted as shell-globs.  C<*> is a wildcard for/within one file or
directory component.  C<**> is a wildcard across multiple path components.  Square brackets
designate a character set similar to regular expresisons e.g. C<< [0-9] >>, and curly braces
specify a list of strings to expand into that portion of the pattern e.g. C<< {a,b,c}* >>
representing essentially three different patterns of C<<a*>> C<<b*>> C<<c*>> in which any is
allowed to match.

=item CODErefs

Coderefs are evaluated in a context where:

=over

=item C<$_>

the full logical path

=item C<@_>

the ocmponents of the logical path, where an empty list means the root directory of the logical
filesystem being referenced.  This means $_[-1] is the name of the leaf file or directory being
inspected.

=item C<_>

the result of an C<lstat> on the path

=item C<$Sys::Export::PathMatch::abspath>

localized to the absolute path name, which was passed to C<lstat>.

=back

If the coderef returns a true value, the path is considered to match.

=item Regexp-refs

Regular expressions are applied to the whole path.  They currently aren't very optimized and
are just essentially equivalent to a coderef of C<< sub { /regexp/ } >>.

=item PathMatch Objects

PathMatch specifications may include other PathMatch objects.  This allows games with overloaded
operators, such as:

  paths 'usr/share/**/*.png', !paths 'usr/share/{application,doc}'

=back

=cut

use v5.26;
use warnings;
use experimental qw( signatures );
use Carp;

=constructor new

  Sys::Export::PathMatch->new(%attributes);

Return a new PathMatch object constructed from named attributes.

=constructor paths

  match($include_spec)
  match($include_spec, $exclude_spec)

This is an exported function, not available as a method:

  use Sys::Export::PathMatch qw( match );
  # or, just Sys::Export qw( match );

=cut

1;
