# Derived from System-Command/t/10-command.t which is
# copyright Phillipe Bruhat (BooK).
use strict;
use warnings;
use utf8;
use Cwd qw/cwd abs_path/;
use File::Spec;
use File::Temp qw/tempdir/;
use Sys::Cmd qw/spawn run/;
use Test::More;

use constant MSWin32 => $^O eq 'MSWin32';

$ENV{TO_BE_DELETED} = 'LATER';
{
    # Environment variables are not passed in and out of Perl like
    # string scalars so make sure they stay as bytes.
    no utf8;
    $ENV{UTF8_CHECK} = 'Défaut';
}

my $dir   = abs_path( tempdir( CLEANUP => 1 ) );
my $cwd   = cwd;
my $name  = File::Spec->catfile( t => 'info.pl' );
my @tests = (
    {
        test    => 'standard',
        cmdline => [ $^X, $name ],
        options => {},
    },
    {
        test    => 'env',
        cmdline => [ $^X, $name, { env => { SYS_CMD => 'Sys::Cmd' } } ],
        options => { env => { SYS_CMD => 'Sys::Cmd' } },
    },
    {
        test    => 'dir',
        cmdline => [
            $^X,
            File::Spec->catfile( $cwd => $name ),
            { dir => $dir, env => { SYS_CMD => 'Sys::Cmd' } },
        ],
        name    => File::Spec->catfile( $cwd => $name ),
        options => {
            env => { SYS_CMD => 'Sys::Cmd' },
            dir => $dir,
        },
    },
    {
        test    => 'delete env',
        cmdline => [
            $^X, $name,
            {
                env => {
                    SYS_CMD       => 'Sys::Cmd',
                    TO_BE_DELETED => undef,
                    OTHER_ENV     => 'something else',
                }
            },
        ],
        options => {
            env => {
                OTHER_ENV     => 'something else',
                SYS_CMD       => 'Sys::Cmd',
                TO_BE_DELETED => undef,
            }
        },
    },
    {
        test    => 'input',
        cmdline => [
            $^X, $name,
            { env => { 'SYS_CMD_INPUT' => 1 }, input => 'test input' }
        ],
        options => { env => { 'SYS_CMD_INPUT' => 1 }, input => 'test input' }
    },
    {
        test    => 'empty input',
        cmdline => [
            $^X, $name,
            {
                env   => { 'SYS_CMD_INPUT' => 1, 'TO_BE_DELETED' => undef },
                input => ''
            }
        ],
        options => {
            env   => { 'SYS_CMD_INPUT' => 1, 'TO_BE_DELETED' => undef },
            input => ''
        }
    },
);
my @fail = (
    {
        test => 'chdir fail',
        cmdline =>
          [ $^X, $name, { dir => File::Spec->catdir( $dir, 'nothere' ) } ],
        fail    => qr/^Failed to change directory/,
        options => {},
    },
    {
        test    => 'command not found',
        cmdline => ['no_command_x77328efe'],
        fail    => qr/^command not found/,
        options => {},
    },
    {
        test    => 'not executable',
        cmdline => [__FILE__],
        fail    => qr/^command not executable/,
        options => {},
    },
    {
        test    => 'execute a directory',
        cmdline => ['t'],
        fail    => qr/^command not a file/,
        options => {},
    },
);

for my $t ( @tests, @fail ) {

    subtest $t->{test}, sub {

        # run the command
        my $cmd = eval { spawn( @{ $t->{cmdline} } ) };
        if ( $t->{fail} ) {
            ok( !$cmd, 'command failed: ' . ( defined $cmd ? $cmd : '' ) );
            like( $@, $t->{fail}, '... expected error message' );
            return;
        }

        isa_ok( $cmd, 'Sys::Cmd' );

        # test the handles
        for my $handle (qw( stdin stdout stderr )) {
            if (MSWin32) {
                isa_ok( $cmd->$handle, 'IO::Handle' );
            }
            else {
                isa_ok( $cmd->$handle, 'IO::Handle' );
            }
            if ( $handle eq 'stdin' ) {
                my $opened = !exists $t->{options}{input};
                is( $cmd->$handle->opened, $opened,
                    "$handle @{[ !$opened && 'not ']}opened" );
            }
            else {
                ok( $cmd->$handle->opened, "$handle opened" );
            }
        }

        is_deeply( [ $cmd->cmdline ],
            [ grep { !ref } @{ $t->{cmdline} } ], 'cmdline' );

        # get the output
        my $output = join '', $cmd->stdout->getlines();
        my $errput = join '', $cmd->stderr->getlines();
        is( $errput, '', 'no errput' );

        my $env = { %ENV, %{ $t->{options}{env} || {} } };
        delete $env->{$_}
          for grep { !defined $t->{options}{env}{$_} }
          keys %{ $t->{options}{env} || {} };
        my $info;
        eval $output;
        is_deeply(
            $info,
            {
                argv  => [],
                dir   => $t->{options}{dir} || $cwd,
                env   => $env,
                input => $t->{options}{input} || '',
                pid   => $cmd->pid,
            },
            "perl $name"
        );

        # close and check
        $cmd->close();
        $cmd->wait_child();
        is( $cmd->exit,   0, 'exit 0' );
        is( $cmd->signal, 0, 'no signal received' );
        is( $cmd->core, $t->{core} || 0, 'no core dumped' );
    };
}

subtest 'reaper', sub {
    my $proc  = spawn($^X);
    my $proc2 = spawn(
        $^X,
        {
            on_exit => sub { kill 9, $proc->pid }
        }
    );

    kill 9, $proc2->pid;
    $proc2->wait_child;    # still need to wait for the signal to happen
    ok( ( defined $proc2->exit ), 'reaper found 2' );
    is $proc2->signal, 9, 'matching signal';

    $proc->wait_child;
    ok( ( defined $proc->exit ), 'reaper worked' );
    is $proc->signal, 9, 'matching signal - on_exit worked';

};

subtest 'coderef', sub {

    my $proc = spawn(
        sub {
            while ( my $line = <STDIN> ) {
                print STDOUT $line;
            }
            exit 3;
        }
    );

    foreach my $i ( 1 .. 10, 'Zürich' ) {
        $proc->stdin->print( $i . "\n" );
        my $res = $proc->stdout->getline;
        chomp $res;
        is $res, $i, "echo $i";
    }

    $proc->close;
    $proc->wait_child;
    is $proc->exit, 3, 'exit 3';

    $proc = spawn(
        sub {
            while ( my $line = <STDIN> ) {
                print STDOUT $line;
            }
            return 3;    # return value should be independpent of exit
        }
    );

    foreach my $i ( 1 .. 2, 'Zürich' ) {
        $proc->stdin->print( $i . "\n" );
        my $res = $proc->stdout->getline;
        chomp $res;
        is $res, $i, "echo $i";
    }

    $proc->close;
    $proc->wait_child;
    is $proc->exit, 0, 'exit 0';
};

done_testing();
