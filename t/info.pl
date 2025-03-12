#!/usr/bin/env -S perl -CSDA
use strict;
use warnings;
use Cwd qw( cwd );
use Data::Dumper;

$Data::Dumper::Sortkeys++;

my $input = $ENV{SYS_CMD_INPUT} ? join( '', <> ) : '';
my $err   = $ENV{SYS_CMD_ERR} // undef;

binmode STDOUT, ':encoding(utf8)';
binmode STDERR, ':encoding(utf8)';

if ( length $err ) {
    delete $ENV{SYS_CMD_ERR};
    print STDERR $err, "\n";
}

print Data::Dumper->Dump(
    [
        {
            argv  => \@ARGV,
            env   => \%ENV,
            cwd   => lc( cwd() ),
            input => $input,
            pid   => $$,
        }
    ],
    ['info']
);
