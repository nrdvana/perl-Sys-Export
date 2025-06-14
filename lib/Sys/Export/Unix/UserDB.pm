package Sys::Export::Unix::UserDB;
# ABSTRACT: Abstractions for Unix passwd/group/shadow files
# VERSION

use v5.36;
use warnings;
use Carp qw(croak carp);
use File::Spec::Functions qw(catfile);
use Storable qw(dclone);
use Scalar::Util ();

=head1 SYNOPSIS

  use Sys::Export::Unix::UserDB;
  
  # Load from filesystem
  my $source_db = Sys::Export::Unix::UserDB->new;
  $source_db->load('/path/to/source/etc');
  
  # Create destination database
  my $dest_db = Sys::Export::Unix::UserDB->new;
  
  # Add users and groups
  $dest_db->add_user($source_db->user('root'));
  $dest_db->add_user('newuser', uid => 1001, gid => 1001);
  $dest_db->add_group('newgroup', gid => 1001);
  
  # Save to files
  $dest_db->save('/path/to/dest/etc');

=head1 DESCRIPTION

This module provides abstractions for working with Unix passwd, group, and shadow files.
It's designed for operations that extract user/group information from one system image
and merge it into another, with proper conflict detection and UID/GID management.

=cut

sub new($class, %args) {
   my $self = {
      users  => {},
      uids   => {},
      groups => {},
      gids   => {},
   };
   
   # Validate and set provided attributes
   for my $key (keys %args) {
      croak "Unknown attribute: $key" unless exists $self->{$key};
      $self->{$key} = $args{$key};
   }
   
   return bless $self, $class;
}

=attribute users

A hashref of C<< username => $user_obj >>.

=attribute uids

A convenience hashref C<< uid => $first_user_obj_having_uid >>.

=attribute groups

A hashref of C<< groupname => $group_obj >>.

=attribute gids

A convenience hashref C<< gid => $first_group_obj_having_gid >>.

=cut

sub users($self) { $self->{users} }

sub uids($self) { $self->{uids} }

sub groups($self) { $self->{groups} }

sub gids($self) { $self->{gids} }

=method clone

   my $cloned_db = $userdb->clone;

Creates a deep clone of the entire UserDB object.

=cut

sub clone($self) {
   return dclone($self);
}

=method load

   $userdb->load($path);

Given a path like C</example/etc>, reads passwd, group, and if readable, shadow files
from that directory.

=cut

sub load($self, $path) {
   croak "Path is required" unless defined $path;
   
   my $passwd_file = catfile($path, 'passwd');
   my $group_file = catfile($path, 'group');
   my $shadow_file = catfile($path, 'shadow');
   
   # Load passwd file
   open my $passwd_fh, '<', $passwd_file 
      or croak "Cannot open $passwd_file: $!";
   
   while (my $line = <$passwd_fh>) {
      chomp $line;
      next if $line =~ /^\s*#/ || $line =~ /^\s*$/;
      
      my ($name, $passwd, $uid, $gid, $gecos, $home, $shell) = split /:/, $line, 7;
      
      my $user = Sys::Export::Unix::UserDB::User->new(
         name   => $name,
         passwd => $passwd,
         uid    => $uid + 0,
         gid    => $gid + 0,
         gecos  => $gecos // '',
         home   => $home // '',
         shell  => $shell // '',
         groups => {},
      );
      
      $self->_add_user_object($user);
   }
   close $passwd_fh;
   
   # Load group file
   open my $group_fh, '<', $group_file 
      or croak "Cannot open $group_file: $!";
   
   while (my $line = <$group_fh>) {
      chomp $line;
      next if $line =~ /^\s*#/ || $line =~ /^\s*$/;
      
      my ($name, $passwd, $gid, $members) = split /:/, $line, 4;
      
      my $group = Sys::Export::Unix::UserDB::Group->new(
         name   => $name,
         passwd  => $passwd // '',
         gid    => $gid + 0,
      );
      
      $self->_add_group_object($group);
      
      # Add group membership to users
      my @member_names = split /,/, ($members // '');
      for my $member_name (@member_names) {
         next unless $member_name;
         if (exists $self->{users}{$member_name}) {
            $self->{users}{$member_name}->add_group($name);
         }
      }
   }
   close $group_fh;
   
   # Load shadow file if readable
   if (-r $shadow_file) {
      open my $shadow_fh, '<', $shadow_file 
         or carp "Cannot open $shadow_file: $!";
      
      if ($shadow_fh) {
         while (my $line = <$shadow_fh>) {
            chomp $line;
            next if $line =~ /^\s*#/ || $line =~ /^\s*$/;
            
            my ($name, $passwd, $lastchg, $min, $max, $warn, $inactive, $expire, $flag) 
               = split /:/, $line, 9;
            
            if (exists $self->{users}{$name}) {
               my $user = $self->{users}{$name};
               $user->passwd($passwd) if defined $passwd;
               $user->lastchg($lastchg) if defined $lastchg;
               $user->min($min) if defined $min;
               $user->max($max) if defined $max;
               $user->warn($warn) if defined $warn;
               $user->inactive($inactive) if defined $inactive;
               $user->expire($expire) if defined $expire;
               $user->flag($flag) if defined $flag;
            }
         }
         close $shadow_fh;
      }
   }
   
   return $self;
}

=method save

   $userdb->save($path_or_hashref);

If given a path, saves passwd, group, and shadow files to that directory.
If given a hashref, saves the file contents into scalars named 'passwd', 'group', 'shadow'.

=cut

sub _mkfile($name, $data, $mode=undef) {
   open my $fh, '>:raw', $name or croak "open(>$name): $!";
   $fh->print($data) or croak "write($name): $!";
   $fh->close or croak "close($name): $!";
   chmod $mode, $name or croak "chmod($name, $mode): $!"
      if defined $mode;
}

sub save($self, $target) {
   croak "Target is required" unless defined $target;
   
   my ($passwd_content, $group_content, $shadow_content) = $self->_generate_file_contents;
   
   if (ref $target eq 'HASH') {
      $target->{passwd} = $passwd_content;
      $target->{group} = $group_content;
      $target->{shadow} = $shadow_content;
   } else {
      _mkfile(catfile($target, 'passwd'), $passwd_content, 0755);
      _mkfile(catfile($target, 'group'),  $group_content,  0755);
      _mkfile(catfile($target, 'shadow'), $shadow_content, 0700);
   }
   
   return $self;
}

=method import_user

   $userdb->import_user($name);

Imports a user from the system via getpwnam.

=cut

sub import_user($self, $name) {
   croak "Username is required" unless defined $name;
   
   my ($login, $passwd, $uid, $gid, $quota, $comment, $gcos, $home, $shell, $expire) = getpwnam($name);
   croak "User '$name' not found in system" unless defined $login;
   
   my $user = Sys::Export::Unix::UserDB::User->new(
      name   => $login,
      passwd => $passwd // 'x',
      uid    => $uid,
      gid    => $gid,
      gecos  => $gcos // '',
      home   => $home // '',
      shell  => $shell // '',
   );
   
   $self->_add_user_object($user);
   return $self;
}

=method import_group

   $userdb->import_group($name);

Imports a group from the system via getgrnam.

=cut

sub import_group($self, $name) {
   croak "Group name is required" unless defined $name;
   
   my ($name_ret, $passwd, $gid, $members) = getgrnam($name);
   croak "Group '$name' not found in system" unless defined $name_ret;
   
   my $group = Sys::Export::Unix::UserDB::Group->new(
      name   => $name_ret,
      passwd  => $passwd // '',
      gid    => $gid,
   );
   
   $self->_add_group_object($group);
   
   # Add group membership to users
   my @member_names = split /\s+/, ($members // '');
   for my $member_name (@member_names) {
      next unless $member_name;
      if (exists $self->{users}{$member_name}) {
         $self->{users}{$member_name}->add_group($name_ret);
      }
   }
   return $self;
}

=method add_user

   $userdb->add_user($name_or_user_obj, %attrs);

Creates a new user. If the first parameter is a User object, clones it and filters
group membership to only include groups that exist in this UserDB. Otherwise creates 
a new user with the given name. Defaults C<uid> to the same as getpwnam if not provided.

=cut

sub add_user($self, $name_or_obj, %attrs) {
   my ($user, $name);
   
   if (ref $name_or_obj && $name_or_obj->isa('Sys::Export::Unix::UserDB::User')) {
      # Clone the user object
      $user = $name_or_obj->clone;
      $name = $user->name;
      
      # Filter group membership to only include groups that exist in this UserDB
      my %groups= $user->groups->%*;
      exists $self->{groups}{$_} or delete $groups{$_}
         for keys %groups;
      $user->groups(\%groups);
      
      # Override any provided attributes
      for my $key (keys %attrs) {
         $user->$key($attrs{$key}) if $user->can($key);
      }
   } else {
      # Create new user
      $name = $name_or_obj;
      croak "Username is required" unless defined $name;
      
      # Set defaults
      $attrs{uid} //= (getpwnam($name))[2];
      $attrs{name} = $name;
      $attrs{passwd} //= 'x';
      $attrs{gid} //= $attrs{uid} // 100;
      $attrs{gecos} //= '';
      $attrs{home} //= "/home/$name";
      $attrs{shell} //= '/bin/bash';
      $attrs{groups} //= {};
      
      $user = Sys::Export::Unix::UserDB::User->new(%attrs);
   }
   
   $self->_add_user_object($user);
   return $self;
}

=method add_group

   $userdb->add_group($name_or_group_obj, %attrs);

Creates a new group. If the first parameter is a Group object, clones it.
Otherwise creates a new group with the given name.

=cut

sub add_group($self, $name_or_obj, %attrs) {
   my ($group, $name);
   
   if (ref $name_or_obj && $name_or_obj->isa('Sys::Export::Unix::UserDB::Group')) {
      # Clone the group object
      $group = $name_or_obj->clone;
      $name = $group->name;
      
      # Override any provided attributes
      for my $key (keys %attrs) {
         $group->$key($attrs{$key}) if $group->can($key);
      }
   } else {
      # Create new group
      $name = $name_or_obj;
      croak "Group name is required" unless defined $name;

      $attrs{gid} //= (getgrnam($name))[2];
      $attrs{name} = $name;
      $attrs{passwd} //= '';
      
      $group = Sys::Export::Unix::UserDB::Group->new(%attrs);
   }
   
   $self->_add_group_object($group);
   return $self;
}

=method user

   my $user = $userdb->user($name_or_uid);

Returns a user by name or UID if it exists, C<undef> otherwise.

=method group

   my $group = $userdb->group($name_or_uid);

Returns a group by name or GID if it exists, C<undef> otherwise.

=cut

sub _looks_like_integer($x) { Scalar::Util::looks_like_number($x) && int($x) == $x }

sub user($self, $spec) {
   _looks_like_integer($spec)? $self->{uids}{$spec} : $self->{users}{$spec};
}

sub group($self, $spec) {
   _looks_like_integer($spec)? $self->{gids}{$spec} : $self->{groups}{$spec};
}

# Private methods

sub _add_user_object($self, $user) {
   my $name = $user->name;
   my $uid = $user->uid;
   
   # Check for name conflicts
   croak "Username '$name' already exists"
      if defined $self->{users}{$name};
   
   # Warn about UID conflicts
   carp "UID $uid already exists for user '".$self->{uids}{$uid}->name."', now also used by '$name'"
      if defined $self->{uids}{$uid};
   
   $self->{users}{$name} = $user;
   $self->{uids}{$uid} //= $user;
}

sub _add_group_object($self, $group) {
   my $name = $group->name;
   my $gid = $group->gid;
   
   # Check for name conflicts
   croak "Group name '$name' already exists"
      if defined $self->{groups}{$name};
   
   # Warn about GID conflicts
   carp "GID $gid already exists for group '".$self->{gids}{$gid}->name."', now also used by '$name'"
      if defined $self->{gids}{$gid};
   
   $self->{groups}{$name} = $group;
   $self->{gids}{$gid} //= $group;
}

sub _generate_file_contents($self) {
   my $passwd_content = '';
   my $group_content = '';
   my $shadow_content = '';
   
   # Generate passwd content
   for my $user (values %{$self->{users}}) {
      $passwd_content .= sprintf "%s:x:%d:%d:%s:%s:%s\n",
         $user->name, $user->uid, $user->gid,
         $user->gecos, $user->home, $user->shell;
   }
   
   # Generate group content
   for my $group (values %{$self->{groups}}) {
      # Collect members from users who have this group
      my @members= map $_->name, grep $_->groups->{$group->name},
         values $self->{users}->%*;
      $group_content .= sprintf "%s:%s:%d:%s\n",
         $group->name, $group->passwd, $group->gid, join ',', sort @members;
   }
   
   # Generate shadow content
   for my $user (values %{$self->{users}}) {
      next unless defined $user->lastchg;  # Only include if shadow data exists
      $shadow_content .= sprintf "%s:%s:%s:%s:%s:%s:%s:%s:%s\n",
         $user->name,
         $user->passwd // '',
         $user->lastchg // '',
         $user->min // '',
         $user->max // '',
         $user->warn // '',
         $user->inactive // '',
         $user->expire // '',
         $user->flag // '';
   }
   
   return ($passwd_content, $group_content, $shadow_content);
}

package Sys::Export::Unix::UserDB::User {
   use v5.36;
   use Carp qw(croak);
   
   sub new($class, %attrs) {
      my $self = {
         name     => undef,
         passwd   => 'x',
         uid      => undef,
         gid      => undef,
         gecos    => '',
         home     => '',
         shell    => '',
         groups   => {},
         # Shadow fields
         lastchg  => undef,
         min      => undef,
         max      => undef,
         warn     => undef,
         inactive => undef,
         expire   => undef,
         flag     => undef,
      };
      
      for my $key (keys %attrs) {
         croak "Unknown user attribute: $key" unless exists $self->{$key};
         $self->{$key} = $attrs{$key};
      }
      # Coerce array of groups to hashref
      if (ref $self->{groups} ne 'HASH') {
         $self->{groups}= { map +($_ => 1), $self->{groups}->@* };
      }
      
      croak "User name is required" unless defined $self->{name};
      croak "User UID is required" unless defined $self->{uid};
      croak "User GID is required" unless defined $self->{gid};
      
      return bless $self, $class;
   }
   
   # Read-only attributes
   sub name($self) { $self->{name} }
   sub uid($self) { $self->{uid} }
   sub gid($self) { $self->{gid} }
   
   # Writable attributes
   sub passwd($self, $val = undef) {
      $self->{passwd} = $val if defined $val;
      return $self->{passwd};
   }
   
   sub gecos($self, $val = undef) {
      $self->{gecos} = $val if defined $val;
      return $self->{gecos};
   }
   
   sub home($self, $val = undef) {
      $self->{home} = $val if defined $val;
      return $self->{home};
   }
   
   sub shell($self, $val = undef) {
      $self->{shell} = $val if defined $val;
      return $self->{shell};
   }
   
   sub lastchg($self, $val = undef) {
      $self->{lastchg} = $val if defined $val;
      return $self->{lastchg};
   }
   
   sub min($self, $val = undef) {
      $self->{min} = $val if defined $val;
      return $self->{min};
   }
   
   sub max($self, $val = undef) {
      $self->{max} = $val if defined $val;
      return $self->{max};
   }
   
   sub warn($self, $val = undef) {
      $self->{warn} = $val if defined $val;
      return $self->{warn};
   }
   
   sub inactive($self, $val = undef) {
      $self->{inactive} = $val if defined $val;
      return $self->{inactive};
   }
   
   sub expire($self, $val = undef) {
      $self->{expire} = $val if defined $val;
      return $self->{expire};
   }

   sub flag($self, $val = undef) {
      $self->{flag} = $val if defined $val;
      return $self->{flag};
   }

   sub groups($self, $val = undef) {
      $self->{groups} = $val if defined $val;
      return $self->{groups};
   }
   
   sub add_group($self, $group_name) {
      $self->{groups}{$group_name}= 1;
      return $self;
   }
   
   sub remove_group($self, $group_name) {
      delete $self->{groups}{$group_name};
      return $self;
   }
   
   sub clone($self) {
      return Sys::Export::Unix::UserDB::User->new(
         name     => $self->{name},
         passwd   => $self->{passwd},
         uid      => $self->{uid},
         gid      => $self->{gid},
         gecos    => $self->{gecos},
         home     => $self->{home},
         shell    => $self->{shell},
         groups   => { $self->{groups}->%* },  # Clone the set
         lastchg  => $self->{lastchg},
         min      => $self->{min},
         max      => $self->{max},
         warn     => $self->{warn},
         inactive => $self->{inactive},
         expire   => $self->{expire},
         flag     => $self->{flag},
      );
   }
}

package Sys::Export::Unix::UserDB::Group {
   use v5.36;
   use Carp qw(croak);
   
   sub new($class, %attrs) {
      my $self = {
         name   => undef,
         passwd => '',
         gid    => undef,
      };
      
      for my $key (keys %attrs) {
         croak "Unknown group attribute: $key" unless exists $self->{$key};
         $self->{$key} = $attrs{$key};
      }
      
      croak "Group name is required" unless defined $self->{name};
      croak "Group GID is required" unless defined $self->{gid};
      
      return bless $self, $class;
   }
   
   # Read-only attributes
   sub name($self) { $self->{name} }
   sub gid($self) { $self->{gid} }
   
   # Writable attributes
   sub passwd($self, $val = undef) {
      $self->{passwd} = $val if defined $val;
      return $self->{passwd};
   }
   
   sub clone($self) {
      return Sys::Export::Unix::UserDB::Group->new(
         name   => $self->{name},
         passwd => $self->{passwd},
         gid    => $self->{gid},
      );
   }
}

1;
