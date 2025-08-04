package Sys::Export::Linux;

# ABSTRACT: Export subsets of a Linux system
# VERSION

=head1 SYNOPSIS

  use Sys::Export::Linux;
  my $exporter= Sys::Export::Linux->new(
    src => '/', dst => '/initrd'
  );
  $exporter->add('bin/busybox');
  $exporter->add_passwd;
  $exporter->add_localtime("UTC");

=head1 DESCRIPTION

This object extends L<Sys::Export::Unix> with Linux-specific helpers and special cases.

See C<Sys::Export::Unix> for the list of core attributes and methods.

=cut

use v5.26;
use warnings;
use experimental qw( signatures );
use parent 'Sys::Export::Unix';

=method add_passwd

  $exporter->add_passwd; # no options yet

This method writes the Linux password files ( C<< /etc/passwd >>, C<< /etc/group >>,
C<< /etc/shadow >> ) either according to the contents of L<Sys::Export::Unix/dst_userdb>
(if you used name-based exports) or according to L<Sys::Export::Unix/src_userdb> filtered by
L<Sys::Export::Unix/dst_uid_used> if C<dst_userdb> is not defined.

In the first pattern, you've either pre-specified the C<dst_userdb> users, or built it lazily
as you exported files.  The C<dst_userdb> contains the complete contents of C<passwd>, C<group>,
and C<shadow> and this method simply generates and adds those files.

In the second pattern, you want the destination userdb to be a subset of the source userdb
according to which UIDs and GIDs were actually used.

If you actually just want to copy the entire source user database files, you could just call

  $exporter->add(qw( /etc/passwd /etc/group /etc/shadow ));

so that pattern doesn't need a special helper method.

=cut

sub add_passwd($self, %options) {
   # If the dst_userdb hasn't been created, create it by filtering the src_userdb by which
   # group and user ids have been seen during the export.
   my $db= $self->dst_userdb;
   unless ($db) {
      $db= Sys::Export::Unix::UserDB->new(
         auto_import => ($self->{src_userdb} //= $self->_build_src_userdb),
      );
      $db->group($_) for keys $self->dst_gid_used->%*;
      $db->user($_) for keys $self->dst_uid_used->%*;
   };
   $db->save($self);
   $self;
}

=method add_localtime

  $exporter->add_localtime($tz_name);

Generate the symlink in C</etc/localtime>, *or* export the timezone file directly to that path
if the system image lacks C</usr/share/zoneinfo>.

Linux uses a symlink at C</etc/localtime> to point to a compiled zoneinfo file describing the
current time zone.  You can simply export this symlink, but if you are building a minimal
system image (like initrd) you might not be exporting the timezone database at
C</usr/share/zoneinfo>.  In that case, you want the single timezone file copied to
C</etc/localtime>.

This method looks for the zone file in your destination filesystem, and if not found there, it
looks in the source filesystem, and if not there either, it checks the host filesystem.  If it
can't find this timezone in any of those locations, it dies.

=cut

sub add_localtime($self, $tz_name) {
   if (exists $self->{dst_path_set}{"usr/share/zoneinfo/$tz_name"}
      || ($self->_dst->can('dst_abs') && -f $self->_dst->dst_abs . $tz_name)
   ) {
      # zoneinfo is exported, and includes this timezone, so symlink to it
      $self->add([ sym => "etc/localtime" => "../usr/share/zoneinfo/$tz_name" ]);
   }
   elsif (my ($path)= grep -e $_,
      $self->src_abs . "usr/share/zoneinfo/$tz_name",
      "/usr/share/zoneinfo/$tz_name"
   ) {
      # resolve symliks down to actual file
      $path= abs_path($path) || croak "Broken symlink at $path";
      $self->add([ file644 => 'etc/localtime', { data_path => $path } ]);
   }
   else {
      croak "Can't find 'usr/share/zoneinfo/$tz_name' in destination, source, or host filesystem";
   }
}

1;
