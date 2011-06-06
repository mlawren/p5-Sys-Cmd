package Sys::Command::Process;
use strict;
use warnings;
use 5.006;
use Moo;
use Carp qw/carp confess croak/;
use Cwd qw( cwd );
use IO::Handle;
use IPC::Open3 qw( open3 );
use Log::Any qw/$log/;

our $VERSION = '0.02';

# MSWin32 support
use constant MSWin32 => $^O eq 'MSWin32';
if (MSWin32) {
    require Socket;
    Socket->import(qw( AF_UNIX SOCK_STREAM PF_UNSPEC ));
}

# Trap the real STDIN/ERR/OUT file handles in case someone
# *COUGH* Catalyst *COUGH* screws with them which breaks open3
my ( $REAL_STDIN, $REAL_STDOUT, $REAL_STDERR );

BEGIN {
    open $REAL_STDIN,  "<&=" . fileno(*STDIN);
    open $REAL_STDOUT, ">>&=" . fileno(*STDOUT);
    open $REAL_STDERR, ">>&=" . fileno(*STDERR);
}

has 'cmd' => (
    is  => 'rw',
    isa => sub {
        ref $_[0] eq 'ARRAY' || confess "cmd must be ARRAYREF";
        @{ $_[0] } || confess "Missing cmd elements";
    },
    required => 1,
);
has 'encoding' => (
    is      => 'rw',
    default => sub { 'utf8' },
);
has 'env' => (
    is  => 'rw',
    isa => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
    default   => sub { {} },
    predicate => 'have_env',
);
has 'dir' => (
    is      => 'rw',
    default => sub { cwd },
);
has 'input' => (
    is        => 'rw',
    predicate => 'have_input',
);
has 'pid' => (
    is       => 'rw',
    init_arg => undef,
);
has 'stdin' => (
    is       => 'rw',
    init_arg => undef,
);
has 'stdout' => (
    is       => 'rw',
    init_arg => undef,
);
has 'stderr' => (
    is       => 'rw',
    init_arg => undef,
);
has 'exit' => (
    is        => 'rw',
    init_arg  => undef,
    predicate => 'have_exit',
);
has 'signal' => (
    is       => 'rw',
    init_arg => undef,
);
has 'core' => (
    is       => 'rw',
    init_arg => undef,
);

sub _pipe {
    socketpair( $_[0], $_[1], AF_UNIX(), SOCK_STREAM(), PF_UNSPEC() )
      or return undef;
    shutdown( $_[0], 1 );    # No more writing for reader
    shutdown( $_[1], 0 );    # No more reading for writer
    return 1;
}

sub BUILD {
    my $self = shift;

    # keep changes to the environment local
    local %ENV = %ENV;

    # chdir to the expected directory
    my $orig = cwd;
    if ( $self->dir ne $orig ) {
        chdir $self->dir or confess "Can't chdir to " . $self->dir . ": $!";
    }

    # turn us into a dumb terminal
    delete $ENV{TERM};

    # update the environment
    if ( $self->have_env ) {
        @ENV{ keys %{ $self->env } } = values %{ $self->env };
    }

    # spawn the command
    $log->debug( $self->cmd_line );
    $self->_spawn;

    # some input was provided
    if ( $self->have_input ) {
        local $SIG{PIPE} =
          sub { croak "Broken pipe when writing to:" . $self->cmdline };

        print { $self->stdin } $self->input if length $self->input;

        if (MSWin32) {
            $self->stdin->flush;
            shutdown( $self->stdin, 2 );
        }
        else {
            $self->stdin->close;
        }
    }

    # chdir back to origin
    if ( $self->dir ne $orig ) {
        chdir $orig or croak "Can't chdir back to $orig: $!";
    }
    return;
}

sub cmd_line {
    my $self = shift;
    return @{ $self->cmd };
}

sub cmdline {
    my $self = shift;
    return join( ' ', @{ $self->cmd } );
}

sub _spawn {
    my $self = shift;

    my ( $pid, $in, $out, $err );

    # save standard handles
    local *STDIN  = $REAL_STDIN;
    local *STDOUT = $REAL_STDOUT;
    local *STDERR = $REAL_STDERR;

    if (MSWin32) {

        # code from: http://www.perlmonks.org/?node_id=811650
        # discussion at: http://www.perlmonks.org/?node_id=811057
        local ( *IN_R,  *IN_W );
        local ( *OUT_R, *OUT_W );
        local ( *ERR_R, *ERR_W );
        _pipe( *IN_R,  *IN_W )  or croak "input pipe error: $^E";
        _pipe( *OUT_R, *OUT_W ) or croak "output pipe error: $^E";
        _pipe( *ERR_R, *ERR_W ) or croak "errput pipe error: $^E";

        $pid =
          eval { open3( '>&IN_R', '<&OUT_W', '<&ERR_W', $self->cmd_line ) };

        # FIXME - better check open3 error conditions
        croak $@ if !defined $pid;

        ( $in, $out, $err ) = ( *IN_W{IO}, *OUT_R{IO}, *ERR_R{IO} );
    }
    else {
        $err = Symbol::gensym;
        $pid = eval { open3( $in, $out, $err, $self->cmd_line ); };

        # FIXME - better check open3 error conditions
        croak $@ if !defined $pid;
    }

    binmode $in,  ':encoding(' . $self->encoding . ')';
    binmode $out, ':encoding(' . $self->encoding . ')';
    binmode $err, ':encoding(' . $self->encoding . ')';

    $self->pid($pid);
    $self->stdin($in);
    $self->stdout($out);
    $self->stderr($err);
    return;
}

sub close {
    my $self = shift;

    # close all pipes
    my $in  = $self->stdin;
    my $out = $self->stdout;
    my $err = $self->stderr;

    if (MSWin32) {
        $in->opened  and shutdown( $in,  2 ) || carp "error closing stdin: $!";
        $out->opened and shutdown( $out, 2 ) || carp "error closing stdout: $!";
        $err->opened and shutdown( $err, 2 ) || carp "error closing stderr: $!";
    }
    else {
        $in->opened  and $in->close  || carp "error closing stdin: $!";
        $out->opened and $out->close || carp "error closing stdout: $!";
        $err->opened and $err->close || carp "error closing stderr: $!";
    }

    # and wait for the child
    waitpid $self->pid, 0;

    # check $?
    my $ret = $?;
    $self->exit( $ret >> 8 );
    $self->signal( $ret & 127 );
    $self->core( $ret & 128 );

    return;
}

sub DESTROY {
    my $self = shift;
    $self->close if !$self->have_exit;
}

1;
