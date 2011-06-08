use strict;
use warnings;
use Test::More;
use Sys::Command::Process;

my $perl = Sys::Command::Process->new( cmd => [ $^X, '-v' ] );
ok scalar( grep { /perl -V/ } $perl->stdout->getlines ), 'found perl -V';

done_testing();
