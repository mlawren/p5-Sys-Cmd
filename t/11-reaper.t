use strict;
use warnings;
use Test::More;
use Sys::Cmd qw/spawn run/;

my $proc  = spawn($^X);
my $proc2 = spawn(
    $^X,
    {
        on_exit => sub { kill 9, $proc->pid }
    }
);

kill 9, $proc2->pid;
$proc2->wait_child;    # still need to wait for the signal to happen
ok $proc2->have_exit, 'reaper found 2';
is $proc2->signal, 9, 'matching signal';

$proc->wait_child;
ok $proc->have_exit, 'reaper worked';
is $proc->signal, 9, 'matching signal - on_exit worked';

done_testing();
