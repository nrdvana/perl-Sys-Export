package Sys::Export::LazyFileData;

# VERSION
# ABSTRACT: Reference a path and optional range of bytes and load it on demand

use v5.26;
use warnings;
use experimental qw( signatures );
use Carp;
use overload q{""} => \&as_string, q{${}} => \&as_scalarref;
use Cwd ();
use Sys::Export ();

=head1 SYNOPSIS

  my $m= Sys::Export::LazyFileData->new($filename);
  say $m;  # prints contents of file
  say $$m; # virtual scalar-ref, also prints contents of file

=head1 DESCRIPTION

This allows you to pass around a filename in place of file data, and automatically load the data
in any context that wants it.  Aside from being lazy, this object helps avoid making copies of
large memory-maps by letting you pass around a reference.

=constructor new

  $m= Sys::Export::LazyFileData->new($filename, $offset= 0, $size= undef);
  $m= Sys::Export::LazyFileData->new(\$scalar, $offset= 0, $size= undef);

Return a new object that either lazily maps/loads a range of a file, or lazily reads a range of
bytes from a file, or lazily performs a 'substr' on a scalar-ref.

=cut

sub new($class, $src, $offset=0, $size=undef) {
   my ($abs, $actual_size);
   if (ref $src eq 'SCALAR') {
      # Ensure is only bytes
      croak "Wide character in file data scalar ref"
         if utf8::is_utf8($src) && !utf8::downgrade($src, 1);
      # If given a scalar-ref, and size and offset are the defaults, then we can just cache that directly.
      return bless [ $src, $src, 0, length $$src ], $class
         if !$offset && (!$size || $size == length $$src);
      $actual_size= length $$src;
   } else {
      $abs= Cwd::abs_path($src) // croak "Can't resolve '$src' to a real file";
      $actual_size= -s $abs;
   }
   $size //= $actual_size - $offset;
   croak "Requested size ($src, $offset+$size) exceeds actual size ($actual_size)"
      if $offset+$size > $actual_size;
   return bless [ undef, $src, $offset, $size, $abs ], $class;
}

=attribute abs_path

The absolute pathname from which the file data is being read.  This can be C<undef> if the file
name wasn't specified in the constructor.

=attribute offset

The byte offset from which it will read

=attribute size

The number of bytes of data that will be returned (clamped to file size)

=method as_string

Returns the content of the range of the file

=method as_ref

Returns the content of the range of the file as a scalar ref

=cut

sub source   { $_[0][1] }
sub offset   { $_[0][2] }
sub size     { $_[0][3] }
sub abs_path { $_[0][4] }

sub as_scalarref {
   $_[0][0] //= ref $_[0][1] eq 'SCALAR'? \substr(${$_[0][1]}, $_[0][2], $_[0][3])
      : Sys::Export::map_or_load_file($_[0][1], $_[0][2], $_[0][3]);
}

sub as_string {
   ${ $_[0][0] // $_[0]->as_scalarref }
}

1;
