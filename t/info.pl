#!perl
use strict;
use warnings;
use Cwd qw( cwd );
use Data::Dumper;

my $input = $ENV{SYS_CMD_INPUT} ? join( '', <> ) : '';

print Data::Dumper->Dump(
    [
        {
            argv  => \@ARGV,
            env   => \%ENV,
            dir   => cwd(),
            input => $input,
            pid   => $$,
        }
    ],
    ['info']
);
