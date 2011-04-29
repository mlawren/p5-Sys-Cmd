use strict;
use warnings;
use Test::More;
use Sys::Command::Process;

my $perl = Sys::Command::Process->new( cmd => [ 'perl', '-v' ] );
my $out = $perl->stdout;
ok scalar( grep { /perl -V/ } <$out> ), 'found perl -V';

#$git->run('branch');

done_testing();
