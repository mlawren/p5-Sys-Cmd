# Derived from System-Command/t/10-command.t which is
# copyright Phillipe Bruhat (BooK).
use strict;
use warnings;
use utf8;
use Cwd qw/cwd abs_path/;
use File::Spec;
use File::Temp qw/tempdir/;
use Sys::Cmd ':all';
use Test2::V0;

use constant MSWin32 => $^O eq 'MSWin32';

$ENV{TO_BE_DELETED} = 'LATER';
{
    # Environment variables are not passed in and out of Perl like
    # string scalars so make sure they stay as bytes.
    no utf8;
    $ENV{UTF8_CHECK} = 'Défaut';
}

my $dir     = abs_path( tempdir( CLEANUP => 1 ) );
my $cwd     = cwd;
my $info_pl = File::Spec->catfile( $cwd, 't', 'info.pl' );
my @tests   = (
    {
        test    => 'standard',
        cmdline => [$info_pl],
        result  => {},
    },
    {
        test    => 'utf8 arguments',
        cmdline => [ $info_pl, 'ß' ],
        result  => {},
    },
    {
        test    => 'env',
        cmdline => [ $info_pl, { env => { SYS_CMD => 'Sys::Cmd' } } ],
        result  => { env => { SYS_CMD => 'Sys::Cmd' } },
    },
    {
        test    => 'dir',
        cmdline =>
          [ $info_pl, { dir => $dir, env => { SYS_CMD => 'Sys::Cmd' } }, ],
        result => {
            env => { SYS_CMD => 'Sys::Cmd' },
            dir => $dir,
        },
    },
    {
        test    => 'delete env',
        cmdline => [
            $info_pl,
            {
                env => {
                    SYS_CMD       => 'Sys::Cmd',
                    TO_BE_DELETED => undef,
                    OTHER_ENV     => 'something else',
                }
            },
        ],
        result => {
            env => {
                OTHER_ENV     => 'something else',
                SYS_CMD       => 'Sys::Cmd',
                TO_BE_DELETED => undef,
            }
        },
    },
    {
        test    => 'empty input',
        cmdline => [
            $info_pl,
            {
                env   => { 'SYS_CMD_INPUT' => 1, 'TO_BE_DELETED' => undef },
                input => ''
            }
        ],
        result => {
            env   => { 'SYS_CMD_INPUT' => 1, 'TO_BE_DELETED' => undef },
            input => ''
        }
    },
    {
        test    => 'input scalar',
        cmdline => [
            $info_pl, { env => { 'SYS_CMD_INPUT' => 1 }, input => 'test input' }
        ],
        result => {
            env   => { 'SYS_CMD_INPUT' => 1 },
            input => 'test input',
        }
    },
    {
        test    => 'input list',
        cmdline => [
            $info_pl,
            {
                env   => { 'SYS_CMD_INPUT' => 1 },
                input => [ "line1\n", "line2\n" ],
            }
        ],
        result => {
            env   => { 'SYS_CMD_INPUT' => 1 },
            input => "line1\nline2\n",
        }
    },
    {
        test    => 'error output',
        cmdline => [ $info_pl, { env => { SYS_CMD_ERR => 'Meh!' } } ],
        result  => { err => "Meh!\n" },
    },
);

my @fail = (
    {
        test    => 'chdir fail',
        cmdline =>
          [ $info_pl, { dir => File::Spec->catdir( $dir, 'nothere' ) } ],
        fail   => qr/directory not found/,
        result => {},
    },
    {
        test    => 'command not found',
        cmdline => ['no_command_x77328efe'],
        fail    => qr/^command not found/,
        result  => {},
    },
    {
        test    => 'not executable',
        cmdline => [__FILE__],
        fail    => qr/^command not executable/,
        result  => {},
    },
    {
        test    => 'execute a directory',
        cmdline => ['t'],
        fail    => qr/^command not found/,
        result  => {},
    },
);

sub do_test {
    my $t = shift;

    # run the command
    my $cmd = eval { spawn( @{ $t->{cmdline} } ) };
    if ( $t->{fail} ) {
        ok( !$cmd,
            $t->{test} . ': command failed: ' . ( defined $cmd ? $cmd : '' ) );
        like( $@, $t->{fail}, $t->{test} . ': expected error message' );
        return;
    }
    die $@ if $@;

    isa_ok( $cmd, 'Sys::Cmd' );

    # test the handles
    for my $handle (qw( stdin stdout stderr )) {
        isa_ok( $cmd->$handle, 'IO::Handle' );
        if ( $handle eq 'stdin' ) {
            my $opened = !exists $t->{result}{input};
            is( $cmd->$handle->opened, $opened,
                "$t->{test}: $handle @{[ !$opened && 'not ']}opened" );
        }
        else {
            ok( $cmd->$handle->opened, "$t->{test}: $handle opened" );
        }
    }

    my @argv = grep { !ref } @{ $t->{cmdline} };
    is( [ $cmd->cmdline ], \@argv, $t->{test} . ': cmdline' );

    # Set @argv to just the script arguments
    shift @argv;

    # get the output
    my $output = join '', $cmd->stdout->getlines();
    my $errput = join '', $cmd->stderr->getlines();
    is( $errput, $t->{result}->{err} // '', $t->{test} . ': stderr match' );

    my $env = { %ENV, %{ $t->{result}{env} || {} } };
    if ( exists $t->{result}->{dir} and $^O eq 'MSWin32' ) {
        $env->{PWD} = $t->{result}->{dir};
    }
    delete $env->{$_}
      for grep { !defined $t->{result}{env}{$_} }
      keys %{ $t->{result}{env} || {} };
    my $info;
    eval $output;

    is(
        $info,
        {
            argv  => \@argv,
            cwd   => lc( $t->{result}{dir} || $cwd ),
            env   => $env,
            input => $t->{result}{input} || '',
            pid   => $cmd->pid,
        },
        "$t->{test}: perl $info_pl"
    );

    # close and check
    $cmd->close();
    $cmd->wait_child();
    is( $cmd->exit,   0,               $t->{test} . ': exit 0' );
    is( $cmd->signal, 0,               $t->{test} . ': no signal received' );
    is( $cmd->core,   $t->{core} || 0, $t->{test} . ': no core dumped' );
}

for my $t ( @tests, @fail ) {
    subtest $t->{test}, \&do_test, $t;
}

subtest 'reaper', sub {
    my $proc2 = spawn($^X);
    my $proc  = spawn(
        $^X,
        {
            on_exit => sub { kill 9, $proc2->pid }
        }
    );

    kill 9, $proc->pid;
    $proc->wait_child;
    $proc2->wait_child;

    ok( ( defined $proc2->exit ),
        'reaper: reaper worked on PID ' . $proc2->pid );
    ok( ( defined $proc->exit ), 'reaper: reaper worked on PID ' . $proc->pid );

  SKIP: {
        skip 'signals do not work on Win32', 1 if $^O eq 'MSWin32';

        is $proc->signal,  9, 'matching signal PID ' . $proc->pid;
        is $proc2->signal, 9, 'matching signal PID ' . $proc2->pid;
    }

};

SKIP: {
    skip "coderefs not supported on Win32", 1 if $^O eq 'MSWin32';

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
            chomp $res if defined $res;
            is $res, $i, "coderef: echo $i";
        }

        $proc->close;
        $proc->wait_child;
        is $proc->exit, 3, 'coderef: exit 3';
    };
}

subtest 'run', sub {
    my ( $out, $err, $info );

    $info = $out = $err = undef;
    $out  = run($info_pl);
    eval $out;
    die $@ if $@;
    is ref($info), 'HASH', 'run() returned $info = { ... }';

    {
        local $SIG{__WARN__} = sub {
            $err = shift;
        };
        run(
            $info_pl,
            {
                env => { SYS_CMD_ERR => 'Complain!' },
            }
        );
        eval $out;
        die $@ if $@;
        is ref($info), 'HASH', 'run() returned $info = { ... }';
        like $err, qr/Complain!/, 'stderr raised as warnings';
    }

    $info = $out = $err = undef;
    run(
        $info_pl,
        {
            out => \$out,
            err => \$err,
        }
    );
    eval $out;
    die $@ if $@;
    is ref($info), 'HASH', 'run() put $info into \$out';
    is $err,       '',     'run() $err empty on zero warnings';

    $info = $out = $err = undef;
    run(
        $info_pl,
        {
            out => \$out,
            err => \$err,
            env => { SYS_CMD_ERR => 'Complain!' },
        }
    );
    eval $out;
    die $@ if $@;
    is ref($info), 'HASH',        'run() put $info into \$out';
    is $err,       "Complain!\n", '$err is set';

    # Test early ->core. Cannot test ->exit here, as even on exception
    # $proc->{exit} jumps into existance, and wait_child uses
    # ->has_exit.
    $info = $out = $err = undef;
    my $proc = spawn($info_pl);
    eval { $proc->core };
    like(
        $@,
        qr/before wait_child/,
        'exit,core,signal only valid after wait_child'
    );
    $proc->wait_child;
    is $proc->core, 0, 'core status 0';
};

SKIP: {
    my $ls = eval { syscmd( 'ls', { dir => 't' } ) };
    skip "No ls?: $@", 1 if $@;

    subtest 'Sys::Cmd', sub {
        my ( $out, @out );
        @out = $ls->run();
        is scalar @out, 2, 'ls in t';

        @out = ();
        $ls->run( '../lib', { out => \$out } );
        @out = split /\n/, $out;
        is scalar @out, 1, 'ls ../lib -> $out';
    };
}

subtest 'mock run', sub {
    my $cmd = syscmd(
        'junk',
        {
            input => "input here\n",
            mock  => sub {
                my $proc = shift;
                like $proc->input, qr/in/, 'in is ' . $proc->input;

                #                diag 'mocked: ' . $proc->cmdline;
                [
                    $proc->cmd->[1],         # out
                    $proc->cmd->[2],         # err
                    $proc->cmd->[3] // 0,    # exit
                    $proc->cmd->[4] // 0,    # core
                    $proc->cmd->[5] // 0,    # signal
                ];

            }
        }
    );

    my ( $out, $err );
    $out = $err = undef;
    $out = $cmd->run( "out1\n", '', 0, 0, 0 );
    is $out, "out1\n", 'mock scalar out';

    $out = $err = undef;
    $cmd->run(
        "out1\n", "err1\n", 0, 0, 0,
        {
            input => 'in1',
            out   => \$out,
            err   => \$err,
        }
    );
    is $out, "out1\n", 'mock ref out';
    is $err, "err1\n", 'mock ref err';

    $out = $err = undef;
    my $proc = $cmd->spawn( "out1\n", "err1\n", 13, 23, 33 );
    $out = $proc->stdout->getline;
    $err = $proc->stderr->getline;
    $proc->wait_child;
    is $out, "out1\n", 'mock ref out';
    is $err, "err1\n", 'mock ref err';
    is( $proc->exit,   13, 'mock exit' );
    is( $proc->core,   33, 'mock core' );
    is( $proc->signal, 23, 'mock signal' );
};

done_testing();
