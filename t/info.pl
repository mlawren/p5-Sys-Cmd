#!/usr/bin/env perl
use strict;
use warnings;
use Cwd qw( cwd );
use Data::Dumper;
use Encode 'decode';

$Data::Dumper::Sortkeys++;

binmode STDIN,  ':encoding(utf8)';
binmode STDOUT, ':encoding(utf8)';
binmode STDERR, ':encoding(utf8)';

my $CODESET = eval { require I18N::Langinfo; I18N::Langinfo::CODESET() };

if ($CODESET) {
    my $codeset = I18N::Langinfo::langinfo($CODESET);
    $_ = decode( $codeset, $_ ) for @ARGV;
}

my $input = $ENV{SYS_CMD_INPUT} ? join( '', <> ) : '';
my $err   = $ENV{SYS_CMD_ERR} // undef;

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
