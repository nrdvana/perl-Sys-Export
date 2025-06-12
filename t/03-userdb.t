use v5.36;
use lib (__FILE__ =~ s,[^\\/]+$,lib,r);
use Test2AndUtils;
my $tmp = File::Temp->newdir;

use Sys::Export::Unix::UserDB;
use File::Spec::Functions qw(catfile catdir);

subtest 'constructor and attributes' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   isa_ok($db, 'Sys::Export::Unix::UserDB');
   
   is(ref $db->users, 'HASH', 'users is a hashref');
   is(ref $db->uids, 'HASH', 'uids is a hashref');
   is(ref $db->groups, 'HASH', 'groups is a hashref');
   is(ref $db->gids, 'HASH', 'gids is a hashref');
   
   # Test constructor with initial values
   my $db2 = Sys::Export::Unix::UserDB->new(
      users => { test => 'value' },
      groups => { test => 'value' }
   );
   is($db2->users->{test}, 'value', 'constructor accepts initial users');
   is($db2->groups->{test}, 'value', 'constructor accepts initial groups');
   
   # Test invalid attribute rejection
   like(dies { Sys::Export::Unix::UserDB->new(invalid => 'value') }, 
       qr/Unknown attribute/, 'rejects unknown attributes');
};

subtest 'user object creation and methods' => sub {
   my $user = Sys::Export::Unix::UserDB::User->new(
      name => 'testuser',
      uid => 1001,
      gid => 1001,
      passwd => 'x',
      gecos => 'Test User',
      home => '/home/testuser',
      shell => '/bin/bash',
      groups => ['wheel', 'users']
   );
   
   isa_ok($user, 'Sys::Export::Unix::UserDB::User');
   is($user->name, 'testuser', 'name accessor works');
   is($user->uid, 1001, 'uid accessor works');
   is($user->gid, 1001, 'gid accessor works');
   is($user->passwd, 'x', 'passwd accessor works');
   is($user->gecos, 'Test User', 'gecos accessor works');
   is($user->home, '/home/testuser', 'home accessor works');
   is($user->shell, '/bin/bash', 'shell accessor works');
   is($user->groups, { wheel => 1, users => 1 }, 'groups accessor works');
   
   # Test writable attributes
   $user->passwd('newpass');
   is($user->passwd, 'newpass', 'passwd is writable');
   
   $user->gecos('New GECOS');
   is($user->gecos, 'New GECOS', 'gecos is writable');
   
   # Test group management
   $user->add_group('admin');
   is($user->groups, { wheel => 1, users => 1, admin => 1 }, 'add_group works');
   
   $user->add_group('wheel');  # duplicate
   is($user->groups, { wheel => 1, users => 1, admin => 1 }, 'add_group ignores duplicates');
   
   $user->remove_group('users');
   is($user->groups, { wheel => 1, admin => 1 }, 'remove_group works');
   
   # Test clone
   my $cloned = $user->clone;
   isa_ok($cloned, 'Sys::Export::Unix::UserDB::User');
   is($cloned->name, 'testuser', 'clone preserves name');
   is($cloned->groups, { wheel => 1, admin => 1 }, 'clone preserves groups');
   
   # Verify it's a deep clone
   $cloned->add_group('test');
   isnt($user->groups, $cloned->groups, 'clone is deep copy');
   
   # Test required fields
   like(dies { Sys::Export::Unix::UserDB::User->new(uid => 1001, gid => 1001) },
       qr/User name is required/, 'name is required');
   like(dies { Sys::Export::Unix::UserDB::User->new(name => 'test', gid => 1001) },
       qr/User UID is required/, 'uid is required');
   like(dies { Sys::Export::Unix::UserDB::User->new(name => 'test', uid => 1001) },
       qr/User GID is required/, 'gid is required');
};

subtest 'group object creation and methods' => sub {
   my $group = Sys::Export::Unix::UserDB::Group->new(
      name => 'testgroup',
      gid => 1001,
      passwd => 'x'
   );
   
   isa_ok($group, 'Sys::Export::Unix::UserDB::Group');
   is($group->name, 'testgroup', 'name accessor works');
   is($group->gid, 1001, 'gid accessor works');
   is($group->passwd, 'x', 'passwd accessor works');
   
   # Test writable attributes
   $group->passwd('newpass');
   is($group->passwd, 'newpass', 'passwd is writable');
   
   # Test clone
   my $cloned = $group->clone;
   isa_ok($cloned, 'Sys::Export::Unix::UserDB::Group');
   is($cloned->name, 'testgroup', 'clone preserves name');
   is($cloned->gid, 1001, 'clone preserves gid');
   
   # Test required fields
   like(dies { Sys::Export::Unix::UserDB::Group->new(gid => 1001) },
       qr/Group name is required/, 'name is required');
   like(dies { Sys::Export::Unix::UserDB::Group->new(name => 'test') },
       qr/Group GID is required/, 'gid is required');
};

subtest 'add_user and add_group methods' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   
   # Test add_user with name
   $db->add_user('testuser', uid => 1001, gid => 1001);
   ok($db->user('testuser'), 'user was added');
   is($db->users->{testuser}->uid, 1001, 'user has correct uid');
   is($db->users->{testuser}->groups, {}, 'user starts with empty groups');
   
   # Test add_group with name
   $db->add_group('testgroup', gid => 1001);
   ok($db->group('testgroup'), 'group was added');
   is($db->groups->{testgroup}->gid, 1001, 'group has correct gid');
   
   # Test add_user with user object
   my $source_user = Sys::Export::Unix::UserDB::User->new(
      name => 'cloneuser',
      uid => 1002,
      gid => 1002,
      groups => ['testgroup', 'nonexistent']
   );
   
   $db->add_user($source_user);
   ok($db->user('cloneuser'), 'user object was cloned and added');
   is($db->users->{cloneuser}->uid, 1002, 'cloned user has correct uid');
   is($db->users->{cloneuser}->groups, { testgroup => 1 }, 'groups filtered to existing only');
   
   # Test add_group with group object
   my $source_group = Sys::Export::Unix::UserDB::Group->new(
      name => 'clonegroup',
      gid => 1003,
      passwd => 'x'
   );
   
   $db->add_group($source_group);
   ok($db->group('clonegroup'), 'group object was cloned and added');
   is($db->groups->{clonegroup}->gid, 1003, 'cloned group has correct gid');
};

subtest 'conflict detection' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   
   # Add initial user and group
   $db->add_user('user1', uid => 1001, gid => 1001);
   $db->add_group('group1', gid => 1001);
   
   # Test name conflict error
   like(dies { $db->add_user('user1', uid => 1002, gid => 1002) },
       qr/Username 'user1' already exists/, 'name conflict detected');
   
   like(dies { $db->add_group('group1', gid => 1002) },
       qr/Group name 'group1' already exists/, 'group name conflict detected');
   
   # Test UID/GID conflict warning
   like(warning { $db->add_user('user2', uid => 1001, gid => 1002) },
       qr/UID 1001 already exists/, 'UID conflict warning');
   
   like(warning { $db->add_group('group2', gid => 1001) },
       qr/GID 1001 already exists/, 'GID conflict warning');
};

subtest 'existence checking methods' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   
   $db->add_user('testuser', uid => 1001, gid => 1001);
   $db->add_group('testgroup', gid => 1002);
   
   ok($db->user('testuser'), 'user returns true for existing user');
   ok(!$db->user('nonexistent'), 'user returns false for non-existing user');
   
   ok($db->group('testgroup'), 'group returns true for existing group');
   ok(!$db->group('nonexistent'), 'group returns false for non-existing group');
   
   ok($db->user(1001), 'user returns true for existing uid');
   ok(!$db->user(9999), 'user returns false for non-existing uid');
   
   ok($db->group(1002), 'group returns true for existing gid');
   ok(!$db->group(9999), 'group returns false for non-existing gid');
};

subtest 'clone method' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   
   $db->add_user('testuser', uid => 1001, gid => 1001);
   $db->add_group('testgroup', gid => 1001);
   $db->users->{testuser}->add_group('testgroup');
   
   my $cloned = $db->clone;
   isa_ok($cloned, 'Sys::Export::Unix::UserDB');
   
   # Verify it's a deep clone
   ok($cloned->user('testuser'), 'cloned db has user');
   ok($cloned->group('testgroup'), 'cloned db has group');
   is($cloned->users->{testuser}->groups, { testgroup => 1 }, 'cloned user has groups');
   
   # Modify original and verify clone is unchanged
   $db->add_user('newuser', uid => 1002, gid => 1002);
   ok($db->user('newuser'), 'original has new user');
   ok(!$cloned->user('newuser'), 'clone does not have new user');
};

subtest 'load from files' => sub {
   my $test_dir = catdir($tmp, 'test_etc');
   mkdir $test_dir;
   
   # Create test passwd file
   my $passwd_data = join("\n",
      'root:x:0:0:root:/root:/bin/bash',
      'daemon:x:1:1:daemon:/usr/sbin:/usr/sbin/nologin',
      'testuser:x:1001:1001:Test User:/home/testuser:/bin/bash',
      'alice:x:1002:1002:Alice:/home/alice:/bin/bash',
      ''
   );
   mkfile(catfile($test_dir, 'passwd'), $passwd_data);
   
   # Create test group file
   my $group_data = join("\n",
      'root:x:0:',
      'daemon:x:1:',
      'testgroup:x:1001:testuser,alice',
      'wheel:x:1002:alice',
      ''
   );
   mkfile(catfile($test_dir, 'group'), $group_data);
   
   # Create test shadow file
   my $shadow_data = join("\n",
      'root:!:19000:0:99999:7:::',
      'testuser:$6$salt$hash:19000:0:99999:7:::',
      ''
   );
   mkfile(catfile($test_dir, 'shadow'), $shadow_data);
   
   my $db = Sys::Export::Unix::UserDB->new;
   $db->load($test_dir);
   
   # Verify users loaded
   ok($db->user('root'), 'root user loaded');
   ok($db->user('testuser'), 'testuser loaded');
   ok($db->user('alice'), 'alice loaded');
   
   is($db->users->{testuser}->uid, 1001, 'testuser has correct uid');
   is($db->users->{testuser}->home, '/home/testuser', 'testuser has correct home');
   
   # Verify groups loaded
   ok($db->group('root'), 'root group loaded');
   ok($db->group('testgroup'), 'testgroup loaded');
   
   # Verify group membership
   is($db->users->{testuser}->groups, { testgroup => 1 }, 'testuser in testgroup');
   is($db->users->{alice}->groups, { testgroup => 1, wheel => 1 }, 'alice in multiple groups');
   
   # Verify shadow data
   is($db->users->{testuser}->passwd, '$6$salt$hash', 'shadow password loaded');
   is($db->users->{testuser}->lastchg, '19000', 'shadow lastchg loaded');
   
   # Test loading non-existent directory
   like(dies { $db->load('/nonexistent') }, qr/Cannot open/, 'dies on missing files');
};

subtest 'save to files' => sub {
   my $db = Sys::Export::Unix::UserDB->new;
   
   # Add test data
   $db->add_user('testuser', uid => 1001, gid => 1001, gecos => 'Test User', 
              home => '/home/testuser', shell => '/bin/bash');
   $db->add_user('alice', uid => 1002, gid => 1002, gecos => 'Alice', 
              home => '/home/alice', shell => '/bin/bash');
   $db->add_group('testgroup', gid => 1001);
   $db->add_group('wheel', gid => 1002);
   
   # Add group memberships
   $db->users->{testuser}->add_group('testgroup');
   $db->users->{alice}->add_group('testgroup');
   $db->users->{alice}->add_group('wheel');
   
   # Add shadow data
   $db->users->{testuser}->lastchg('19000');
   $db->users->{testuser}->passwd('$6$salt$hash');
   
   # Test saving to hashref
   my %files;
   $db->save(\%files);
   
   like($files{passwd}, qr/testuser:x:1001:1001:Test User:\/home\/testuser:\/bin\/bash/, 
       'passwd file contains testuser');
   like($files{group}, qr/testgroup:.*:1001:alice,testuser/,
       'group file contains membership');
   like($files{shadow}, qr/testuser:\$6\$salt\$hash:19000/, 
       'shadow file contains testuser');
   
   # Test saving to directory
   my $save_dir = catdir($tmp, 'save_test');
   mkdir $save_dir;
   $db->save($save_dir);
   
   ok(-f catfile($save_dir, 'passwd'), 'passwd file created');
   ok(-f catfile($save_dir, 'group'), 'group file created');
   ok(-f catfile($save_dir, 'shadow'), 'shadow file created');
   
   my $saved_passwd = slurp(catfile($save_dir, 'passwd'));
   like($saved_passwd, qr/testuser:x:1001:1001/, 'saved passwd contains testuser');
   
   my $saved_group = slurp(catfile($save_dir, 'group'));
   like($saved_group, qr/testgroup:.*:1001:alice,testuser/, 'saved group contains membership');
};

subtest 'roundtrip load and save' => sub {
   # Create original test data
   my $orig_dir = catdir($tmp, 'orig');
   mkdir $orig_dir;
   
   my $passwd_data = join("\n",
      'root:x:0:0:root:/root:/bin/bash',
      'testuser:x:1001:1001:Test User:/home/testuser:/bin/bash',
      'alice:x:1002:1002:Alice:/home/alice:/bin/bash',
      ''
   );
   mkfile(catfile($orig_dir, 'passwd'), $passwd_data);
   
   my $group_data = join("\n",
      'root:x:0:',
      'testgroup:x:1001:testuser,alice',
      'wheel:x:1002:alice',
      ''
   );
   mkfile(catfile($orig_dir, 'group'), $group_data);
   
   my $shadow_data = join("\n",
      'testuser:$6$salt$hash:19000:0:99999:7:::',
      'alice:!:19001:::::',
      ''
   );
   mkfile(catfile($orig_dir, 'shadow'), $shadow_data);
   
   # Load, then save
   my $db = Sys::Export::Unix::UserDB->new;
   $db->load($orig_dir);
   
   my $save_dir = catdir($tmp, 'roundtrip');
   mkdir $save_dir;
   $db->save($save_dir);
   
   # Load the saved data and verify
   my $db2 = Sys::Export::Unix::UserDB->new;
   $db2->load($save_dir);
   
   ok($db2->user('testuser'), 'roundtrip preserved testuser');
   ok($db2->user('alice'), 'roundtrip preserved alice');
   ok($db2->group('testgroup'), 'roundtrip preserved testgroup');
   
   is($db2->users->{testuser}->groups, { testgroup => 1 }, 'roundtrip preserved testuser groups');
   is($db2->users->{alice}->groups, { testgroup => 1, wheel => 1 }, 'roundtrip preserved alice groups');
   
   is($db2->users->{testuser}->passwd, '$6$salt$hash', 'roundtrip preserved shadow password');
   is($db2->users->{testuser}->lastchg, '19000', 'roundtrip preserved shadow data');
};

done_testing;
