use strict;
use warnings;
use Test::More;
use Sys::Command qw/run spawn/;

ok run( 'perl', '-v' ) =~ m/perl -V/, 'run scalar context';

my $perl = Sys::Command->new( cmd => ['perl'], );

my $txt = $perl->run('-v');
ok $txt =~ m/perl -V/, 'obj run scalar context';
ok scalar( grep { /perl -V/ } $perl->run('-v') ), 'obj run list context';

eval { $perl->run( '-e', 'die "with message".(1+2)' ) };
like $@, qr/with message3/, 'Caught STDERR on error';

done_testing();
