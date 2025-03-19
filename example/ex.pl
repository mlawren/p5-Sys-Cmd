#!/usr/bin/env perl
use strict;
use warnings;
use lib 'lib';

#use OptArgs2::Pager 'start_pager';
#use Log::Any::Adapter 'Stderr';
use Sys::Cmd 'runsub';

#start_pager()

my $git = runsub('git');
print "lib/\n";
my @list = $git->( 'ls-files', { dir => 'lib' } );
print @list;
print "t/\n";
my $commit = $git->( 'ls-files', { dir => 't' } );
print $commit;

__END__
