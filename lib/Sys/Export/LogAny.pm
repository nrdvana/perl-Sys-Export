package Sys::Export::LogAny;

# VERSION
# ABSTRACT: Use Log::Any without depending on it

=head1 DESCRIPTION

Sys::Export aims to be dependency-free at runtime, so that it can be mounted into arbitrary
environments without needing to install any other modules.  But Log::Any sure is useful...
This shim module loads the real Log::Any if it is installed, else it falls back to a bare
minimum log object that logs to STDERR.

=cut

use v5.26;
use warnings;

if (eval 'use Log::Any 1.051; 1') {
   our @ISA= ( 'Log::Any' );
   Log::Any->import(default_adapter => [ 'Stderr', log_level => 'info' ]);
} else {
   *get_logger= sub { bless {}, 'Sys::Export::LogAny::_Logger'; };
   *import= sub {
      for (@_[1..$#_]) {
         if ($_ eq '$log') {
            no strict 'refs';
            my $caller= caller;
            ${$caller . '::log'}= bless {}, 'Sys::Export::LogAny::_Logger';
         }
         else { die "Can't export '$_'"; }
      }
   };
}

package Sys::Export::LogAny::_Logger {
   use v5.26;
   use warnings;
   use experimental qw( signatures );
   sub _dump {
      state $dumper_loaded= require Data::Dumper;
      chomp(my $s= Data::Dumper->new([$_[0]])->Terse(1)->Sortkeys(1)->Dump);
      $s;
   }
   sub is_info { 1 }
   sub info($self, @msg) {
      print STDERR join(' ', @msg)."\n"
   }
   sub infof($self, $fmt, @args) {
      printf STDERR $fmt."\n", map +(ref? _dump($_) : $_), @args;
   }
   *error = *warn  = *notice = *info;
   *errorf= *warnf = *noticef= *infof;
   *is_error= *is_warn= *is_notice= *is_info;
   sub is_debug { ($ENV{DEBUG} // 0) >= 1 }
   sub is_trace { ($ENV{DEBUG} // 0) >= 2 }
   sub debug  { is_debug? info (@_) : () }
   sub debugf { is_debug? infof(@_) : () }
   sub trace  { is_trace? info (@_) : () }
   sub tracef { is_trace? infof(@_) : () }
}

1;
