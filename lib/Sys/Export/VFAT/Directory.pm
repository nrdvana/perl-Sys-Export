package Sys::Export::VFAT::Directory;

# VERSION
# ABSTRACT: Represents a directory in VFAT

use v5.26;
use warnings;
use experimental qw( signatures );
use Sys::Export::VFAT qw( ATTR_VOLUME_ID ATTR_DIRECTORY is_valid_shortname );
use parent 'Sys::Export::VFAT::File';
use Sys::Export::LogAny '$log';
use Encode;
use Scalar::Util qw( weaken );
use List::Util qw( min max );
use POSIX qw( ceil );
use Carp;
our @CARP_NOT= qw( Sys::Export::VFAT );
*_epoch_to_fat_date_time= *Sys::Export::VFAT::_epoch_to_fat_date_time;

=constructor new

  my $dir= Sys::Export::VFAT::Directory->new(%attrs);

Accepts all attributes except is_dir, is_root, ents, and ent_by_fc

=cut

sub new($class, %attrs) {
   my $self= bless { ent_by_fc => {}, ents => [] }, $class;
   exists $attrs{$_} && ($self->{$_}= delete $attrs{$_})
      for qw( name size cluster align offset data_ref data_path parent );
   croak "Unknown attribute ".join(', ', keys %attrs) if keys %attrs;
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
sub ents      { $_[0]{ents} }
sub ent_by_fc { $_[0]{ent_by_fc} }

sub _new_dirent { Sys::Export::VFAT::Directory::Entry->new(@_) }

=method child

  my $ent= $dir->child($name);

Return a directory entry by case-folded name.

=cut

sub child {
   $_[0]{ent_by_fc}{fc $_[1]}
}

=method add_ent

  $ent= $dir->add_ent($ent);
  $ent= $dir->add_ent(\%entry_attributes);

=cut

sub add_ent($self, $dirent) {
   $dirent= _new_dirent(%$dirent) if ref $dirent eq 'HASH';
   # Check collisions on both the long and short name
   my ($long, $short)= ($dirent->long, $dirent->short);
   my $long_conflict= defined $long && $self->{ent_by_fc}{fc $long};
   my $short_conflict= defined $short && $self->{ent_by_fc}{fc $short};
   if ($long_conflict || $short_conflict) {
      # If the user is writing a directory and the thing in the way is an auto-dir,
      # replace the entry with the new attributes.
      if ($long_conflict && (!$short_conflict || $short_conflict == $long_conflict)
         && $long_conflict->is_dir && $long_conflict->autovivified
         && $dirent->is_dir
      ) {
         weaken($dirent->{file}= $long_conflict->{file});
      }
      else {
         croak "Path ".$dirent->name." already exists"
            if $long_conflict;
         croak "Path ".$dirent->name." short name ".$dirent->short." conflicts with "
            . $short_conflict->name;
      }
   }
   $self->{ent_by_fc}{fc $long}= $dirent if length $long;
   $self->{ent_by_fc}{fc $short}= $dirent if length $short;
   push $self->{ents}->@*, $dirent;
   $dirent;
}

# This assigns a valid 8.3 filename which doesn't conflict with any of the other ents
sub _build_shortname($self, $ent) {
   my $name= $ent->long;
   my $ent_by_fc= $self->ent_by_fc;
   length $name or die "BUG";
   $name= uc $name;
   my $ext_pos= rindex($name, '.');
   my $base= $ext_pos < 0? $name : substr($name, 0, $ext_pos);
   my $ext=  $ext_pos < 0? ''    : substr($name, $ext_pos+1);
   for ($base, $ext) {
      # Replace every run of invalid chars with a single '_'
      s/[^\x21\x23-\x29\x2D.\x30-\x39\x40-\x5A\x5E-\x7B\x7D-\xE4\xE6-\xFF]+/_/g;
      # Now that all high characters have been removed, consider these to be bytes
      utf8::downgrade($_);
   }
   $ext= '.'.substr($ext,0,3) if length $ext;
   my ($iter, $iter_len, $base_len)= (0,0, length $base);
   if (!$base_len || $base_len > 8) {
      substr($base, min($base_len,6), $base_len, '~1');
      ($iter, $iter_len)= (1, 2);
   }
   while ($ent_by_fc->{fc $base.$ext}) {
      my $next_iter_len= 1 + length ++$iter;
      my $iter_pos= min($base_len, 8 - $next_iter_len);
      croak "Can't find available ~N suffix for '$name'"
         if $iter_pos < 0;
      substr($base, $iter_pos, $next_iter_len, '~'.$iter);
      $iter_len= $next_iter_len;
   }
   $ent_by_fc->{fc $base.$ext}= $ent;
   $ent->{short}= $base.$ext;
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
      unless (length $_->short11) {
         $self->_build_shortname($_) if !defined $_->short;
         my ($name, $ext)= split /\./, $_->short;
         $_->{short11}= pack 'A8 A3', $name, ($ext//'');
      }
      # Add LFN entries
      if (defined $_->long && $_->long ne $_->short) {
         my $utf16= encode('UTF-16LE', $_->long, Encode::FB_CROAK|Encode::LEAVE_SRC);
         $n += ceil(length($utf16) / 26);
      }
   }
   $log->debugf("dir /%s has %d real entries, %d LFN entries, size=%d ents=%s",
      $self->name, scalar(@$ents), $n-scalar @$ents, $n*32,
      [ map [ $_->long, $_->short ], @$ents ] )
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
      _new_dirent(short11 => $vfat->volume_label // 'NO NAME    ', attrs => ATTR_VOLUME_ID),
   ) : (
      _new_dirent(short11 => ".", attrs => ATTR_DIRECTORY, file => $self),
      _new_dirent(short11 => '..', attrs => ATTR_DIRECTORY, file => $self->parent),
   );
   for my $ent (@special, sort { fc $a->long cmp fc $b->long } $self->ents->@*) {
      $log->tracef("encoding dirent short=%-12s long=%s cluster=%s",
         $ent->short//$ent->short11, $ent->long, $ent->file && $ent->file->cluster);

      my $short11= $ent->short11 // die "BUG: short11 not set";
      $short11 =~ s/^\xE5/\x05/; # \xE5 may occur in some charsets, and needs escaped

      # Need Long-File-Name entries?
      my $long= $ent->long;
      if (defined $long && $long ne $ent->short) {
         # Checksum for directory shortname, used to verify long name parts
         my $cksum= 0;
         $cksum= ((($cksum >> 1) | ($cksum << 7)) + $_) & 0xFF
            for unpack 'C*', $short11;
         # Each dirent holds up to 26 bytes (13 chars) of the long name
         my @chars= unpack 'v*', encode('UTF-16LE', $long, Encode::FB_CROAK|Encode::LEAVE_SRC);
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

      my $mtime= $ent->mtime // time;
      my ($wdate, $wtime)             = _epoch_to_fat_date_time($mtime);
      my ($cdate, $ctime, $ctime_frac)= _epoch_to_fat_date_time($ent->btime // $mtime);
      my ($adate)                     = _epoch_to_fat_date_time($ent->atime // $mtime);
      # References to the root dir are always encoded as cluster zero, even on FAT32
      # where the root dir actually lives at a nonzero cluster.
      # Volume label also doesn't need a cluster id.  Nor do empty files.
      my $cluster= 0;
      my $file= $ent->file;
      if ($file && !($file->is_dir && $file->is_root)) {
         $cluster= $file->cluster // croak "File ".$file->name." lacks a defined cluster id";
      }
      # Directories always written as size = 0
      my $size= $ent->is_dir? 0 : $file? $file->size : 0;
      $log->tracef(" with encoded size=%d cluster=%d", $size, $cluster);
      $data .= pack('A11 C C C v v v v v v v V',
         $short11, $ent->attrs, 0, #NT_reserved
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

Directory entries have the following attributes:

=over

=item name

A unicode full pathname for debugging

=item long

A unicode name not including path.  May be undef if file uses only a short name.

=item short

A FAT "short" name in 8.3 notation.  This must be bytes, and is fairly restricted in the ASCII
range, but may contain high bytes for an unspecified character encoding.

=item short11

The 11 bytes of the short name to be encoded into a directory entry, used for volume labels and
'.' and '..' entries.  L</long> and L</short> may be undefined if this is set.

=item attrs

A bitwise 'or' of FAT attribute flags.

=item atime

Unix epoch time of last access

=item mtime

Unix epoch time of last modification

=item btime

Unix epoch time of file creation ('born')

=item file

A reference to a File or Directory object.  If C<undef>, this entry represents an empty file.

=item is_dir

whether L</attrs> includes C<ATTR_DIRECTORY>.

=item autovivified

Flag to indicate that the VFAT module created the directory automatically.

=back

=cut

no warnings 'once';
@Sys::Export::VFAT::Directory::Entry::CARP_NOT= qw( Sys::Export::VFAT::Directory );

sub Sys::Export::VFAT::Directory::Entry::new($class, %attrs) {
   my $self= bless {}, $class;
   for (qw( name long short short11 attrs atime mtime btime file autovivified )) {
      if (defined (my $v= delete $attrs{$_})) {
         $self->{$_}= $v;
      }
   }
   croak "Unknown attribute ".join(', ', keys %attrs) if keys %attrs;
   unless (defined $self->{short11}) {
      # 'name' is for debugging.  But also use it as the default for the long name
      my $long= ($self->{long} //= ($self->{name} =~ m{([^/]+)\z})[0]);
      if (defined $self->{short}) {
         croak "Invalid short name '$self->{short}'" unless is_valid_shortname($self->{short});
      } elsif (defined $long) {
         $self->{short} //= $long if is_valid_shortname($long);
      } else {
         croak "Must define 'name', 'long', 'short', or 'short11'";
      }
      utf8::downgrade($self->{short}) if defined $self->{short}; # verified to be just bytes
   }
   if ($self->{file} && $self->{file}->is_dir) {
      $self->{attrs}= ($self->{attrs} // 0) | ATTR_DIRECTORY;
      # References to other directories must always be weak refs
      weaken($self->{file});
   }
   $self;
}

sub Sys::Export::VFAT::Directory::Entry::name        { $_[0]{name} }
sub Sys::Export::VFAT::Directory::Entry::long        { $_[0]{long} }
sub Sys::Export::VFAT::Directory::Entry::short       { $_[0]{short} }
sub Sys::Export::VFAT::Directory::Entry::short11     { $_[0]{short11} }
sub Sys::Export::VFAT::Directory::Entry::attrs       { $_[0]{attrs} }
sub Sys::Export::VFAT::Directory::Entry::is_dir      { $_[0]{attrs} & ATTR_DIRECTORY }
sub Sys::Export::VFAT::Directory::Entry::atime       { $_[0]{atime} }
sub Sys::Export::VFAT::Directory::Entry::mtime       { $_[0]{mtime} }
sub Sys::Export::VFAT::Directory::Entry::btime       { $_[0]{btime} }
sub Sys::Export::VFAT::Directory::Entry::autovivified{ $_[0]{autovivified} }
sub Sys::Export::VFAT::Directory::Entry::file($self, @val) {
   if (@val) {
      $self->{file}= $val[0];
      weaken($self->{file}) if $self->{file} && $self->{file}->is_dir;
   }
   $self->{file}
}

1;
