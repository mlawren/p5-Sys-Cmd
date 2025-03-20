#!/usr/bin/env perl
use strict;
use warnings;
use lib 'lib';
use Log::Any::Adapter 'Stdout', log_level => 'info';
use Sys::Cmd 'runsub';

my $git = runsub('git');

print "lib/\n";
my @list = $git->( 'ls-files', { dir => 'lib' } );
print @list, "\n";

print "t/\n";
my $commit = $git->( 'ls-files', { dir => 't' } );
print $commit, "\n";

my $dummy = runsub(
    'junk-cmd',
    {
        input => "Gotcha!\n",
        mock  => sub {
            print 'mocked: ' . $_[0]->cmdline . ': ' . $_[0]->input . "\n";
            [ "out\n", "err", 0, 6553, 9876 ];
        },
    }
);

print $dummy->( 'some', 'args' ), "\n";

$git->('bad-cmd');
__END__
