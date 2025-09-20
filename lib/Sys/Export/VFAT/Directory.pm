package Sys::Export::VFAT::Directory;

# VERSION
# ABSTRACT: Represents a directory in VFAT

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export::VFAT qw( ATTR_VOLUME_ID ATTR_DIRECTORY is_valid_longname is_valid_shortname build_shortname );
use parent 'Sys::Export::VFAT::File';
use Sys::Export::LogAny '$log';
use Encode;
use Scalar::Util qw( weaken );
use List::Util qw( min max );
use POSIX qw( ceil );
use Carp;
our @CARP_NOT= qw( Sys::Export::VFAT );

=constructor new

  my $dir= Sys::Export::VFAT::Directory->new(%attrs);

Accepts all attributes except is_dir, is_root, ents, and ent_by_fc

=cut

sub new($class, %attrs) {
   my $self= bless { ent_by_fc => {}, ents => [] }, $class;
   for (qw( name atime btime mtime flags cluster size data_ref data_path parent )) {
      if (defined (my $v= delete $attrs{$_})) {
         $self->{$_}= $v
      }
   }
   croak "Unknown constructor option ".join(', ', keys %attrs) if keys %attrs;
   weaken($self->{parent}) if $self->{parent};
   $self;
}

=attribute is_dir

True

=attribute parent

A weak reference to the parent directory, or C<undef> at the root.

=attribute is_root

True if C<parent> is C<undef>.

=attribute ents

An array of ::Directory::Entry

=attribute ent_by_fc

A hashref of case-folded unicode long filename to ::Entry object.  This also includes short
names, to ensure that long and short do not conflict, but short names may not be resolved until
L</pack>.

=cut

sub parent    { $_[0]{parent} }
sub is_dir    { 1 }
sub is_root   { !defined $_[0]{parent} }
sub flags     { ($_[0]{flags} // 0) | ATTR_DIRECTORY }
sub ents      { $_[0]{ents} }
sub ent_by_fc { $_[0]{ent_by_fc} }

=method child

  my $ent= $dir->child($name);

Return a directory entry by case-folded name.

=cut

sub child {
   $_[0]{ent_by_fc}{fc $_[1]}
}

=method add_ent

  $ent= $dir->add($name, $file_or_dir, %other_attrs);

Returns the hashref storing the directory entry.

=cut

sub add($self, $name, $file, %attrs) {
   croak "Invalid long name" unless is_valid_longname($name);
   $attrs{name}= $name;
   $attrs{file}= $file;
   weaken($attrs{file}) if $file && $file->is_dir;
   $attrs{shortname} //= $name if is_valid_shortname($name);
   # any conflict?
   my $by_fc= $self->ent_by_fc;
   croak "Path ".$self->name."/$name already exists"
      if defined $by_fc->{fc $name};
   if (defined $attrs{shortname}) {
      utf8::downgrade($attrs{shortname}); # must be bytes
      my $conflict= $by_fc->{fc $attrs{shortname}};
      croak "Path ".$self->name."/$name short name '$attrs{shortname}' conflicts with "
         . $self->name."/".$conflict->{name}
         if $conflict;
      $by_fc->{fc $attrs{shortname}}= \%attrs;
   }
   $by_fc->{fc $name}= \%attrs;
   push $self->ents->@*, \%attrs;
   \%attrs;
}

=method calc_size

Computes the L</size> attribute from the L</ents> added so far.

=cut

sub calc_size($self) {
   my $ents= $self->{ents};
   # root dir has a volume label ent, and all other dirs have '.' and '..'
   my $n= @$ents + ($self->is_root? 1 : 2);
   for (@$ents) {
      # Need the 8.3 name in order to know whether it matches the long name
      unless (defined $_->{shortname}) {
         $_->{shortname}= build_shortname($_->{name}, $self->ent_by_fc);
         $self->ent_by_fc->{fc $_->{shortname}}= $_;
      }
      unless (length $_->{short11}) {
         my ($base, $ext)= split /\./, $_->{shortname};
         $_->{short11}= pack 'A8 A3', $base, ($ext//'');
      }
      # Add LFN entries
      if ($_->{name} ne $_->{shortname}) {
         my $utf16= encode('UTF-16LE', $_->{name}, Encode::FB_CROAK|Encode::LEAVE_SRC);
         $n += ceil(length($utf16) / 26);
      }
   }
   $log->debugf("dir /%s has %d real entries, %d LFN entries, size=%d ents=%s",
      $self->name, scalar(@$ents), $n-scalar @$ents, $n*32,
      [ map [ @{$_}{'name','shortname'} ], @$ents ] )
      if $log->is_debug;
   croak "Directory /".$self->name." exceeds maximum entry count ($n >= 65536)"
      if $n >= 65536;
   $self->{size}= $n * 32;  # always 32 bytes per dirent
}

=method pack

  $dir->pack($vfat);

Computes the L</data_ref> attribute from the L</ents>.  For the root entry, it also needs
access to C<$vfat> for the volume label.

=cut

sub pack($self, $vfat) {
   my $data= '';
   my @special= $self->is_root? (
      { short11 => $vfat->volume_label // 'NO NAME    ', flags => ATTR_VOLUME_ID },
   ) : (
      { short11 => ".",  flags => ATTR_DIRECTORY, file => $self },
      { short11 => '..', flags => ATTR_DIRECTORY, file => $self->parent },
   );
   for my $ent (@special, sort { fc $a->{name} cmp fc $b->{name} } $self->ents->@*) {
      my ($name, $file, $shortname, $short11)= @{$ent}{qw( name file shortname short11 )};
      $log->tracef("encoding dirent short=%-12s long=%s cluster=%s",
         $shortname//$short11, $name, $file && $file->cluster);

      $short11 =~ s/^\xE5/\x05/; # \xE5 may occur in some charsets, and needs escaped

      # Need Long-File-Name entries?
      if (defined $name && $name ne $shortname) {
         # Checksum for directory shortname, used to verify long name parts
         my $cksum= 0;
         $cksum= ((($cksum >> 1) | ($cksum << 7)) + $_) & 0xFF
            for unpack 'C*', $short11;
         # Each dirent holds up to 26 bytes (13 chars) of the long name
         my @chars= unpack 'v*', encode('UTF-16LE', $name, Encode::FB_CROAK|Encode::LEAVE_SRC);
         # short final chunk is padded with \0\uFFFF*
         if (my $remainder= @chars % 13) {
            push @chars, 0;
            push @chars, (0xFFFF)x(12 - $remainder);
         }
         my $last= ceil(@chars/13) - 1;
         for my $i (reverse 0..$last) {
            my $ofs= $i*13;
            my $seq= ($i + 1) | (($i == $last) ? 0x40 : 0x00);
            $data .= pack('C v5 C C C v6 v v2',
               $seq,                      # sequence and end-flag
               @chars[$ofs .. $ofs+4],    # first 5 chars
               0x0F, 0x00, $cksum,        # attr = LFN, type = 0
               @chars[$ofs+5 .. $ofs+10], # next 6 chars
               0,                         # no cluster number
               @chars[$ofs+11 .. $ofs+12] # last 2 chars
            );
         }
      }

      my $mtime= $ent->{mtime} // ($file && $file->mtime) // time;
      my $atime= $ent->{atime} // ($file && $file->atime) // $mtime;
      my $btime= $ent->{btime} // ($file && $file->btime) // $mtime;
      my ($wdate, $wtime)             = Sys::Export::VFAT::_epoch_to_fat_date_time($mtime);
      my ($cdate, $ctime, $ctime_frac)= Sys::Export::VFAT::_epoch_to_fat_date_time($btime);
      my ($adate)                     = Sys::Export::VFAT::_epoch_to_fat_date_time($atime);
      my $flags= $ent->{flags} // ($file && $file->flags) // 0;
      # References to the root dir are always encoded as cluster zero, even on FAT32
      # where the root dir actually lives at a nonzero cluster.
      # Volume label also doesn't need a cluster id.  Nor do empty files.
      my $cluster= 0;
      if ($file && !($file->is_dir && $file->is_root)) {
         $cluster= $file->cluster // croak "File ".$file->name." lacks a defined cluster id";
      }
      # Directories always written as size = 0
      my $size= !$file? 0 : $file->is_dir? 0 : $file->size;
      $log->tracef(" with encoded size=%d cluster=%d", $size, $cluster);
      $data .= pack('A11 C C C v v v v v v v V',
         $short11, $flags, 0, #NT_reserved
         $ctime_frac, $ctime, $cdate, $adate,
         $cluster >> 16, $wtime, $wdate, $cluster, $size);
   }
   die "BUG: calculated dir size ".$self->size." != data length ".length($data)
      unless $self->size == length $data;
   # Dir must be padded to length of sector/cluster with entries whose name begins with \x00
   # but that will happen automatically later as the data is appended to the file.
   $self->{data_ref}= \$data;
}

=head1 Directory Entries

Directory entries can have the following hash keys:

=over

=item name

A unicode "long" name

=item short

A FAT "short" name in 8.3 notation.  This must be bytes, and is fairly restricted in the ASCII
range, but may contain high-bit bytes for an unspecified BIOS character encoding.

=item short11

The 11 bytes of a short name (space-padded and with the extension '.' removed) to be encoded
into a directory entry.  This is specified for volume labels and '.' and '..' entries instead
of using the C<name> and C<shortname> attributes.

=item flags

A bitwise 'or' of FAT attribute flags.

=item atime

Unix epoch time of last access

=item mtime

Unix epoch time of last modification

=item btime

Unix epoch time of file creation ('born')

=item file

A reference to a File or Directory object.  If C<undef>, this entry represents an empty file.

=item autovivified

Flag to indicate that the VFAT module created the directory automatically.

=back

=cut

1;
