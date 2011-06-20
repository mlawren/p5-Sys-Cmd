package Sys::Cmd;
use strict;
use warnings;
use 5.006;
use Moo;
use Carp qw/carp confess croak/;
use Cwd qw/cwd/;
use Sub::Exporter -setup => { exports => [qw/spawn run runx/], };
use IO::Handle;
use IPC::Open3 qw/open3/;
use File::Which qw/which/;
use Log::Any qw/$log/;

our $VERSION = '0.98_1';
our $CONFESS;

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

sub run {
    my $proc = spawn(@_);
    my @out = $proc->stdout->getlines;
    my @err = $proc->stderr->getlines;

    $proc->close;

    if ( $proc->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' .
        $proc->exit )
          if $CONFESS;
        croak( join( '', @err ) . 'Command exited with value ' . $proc->exit );
    }

    if (wantarray) {
        return @out;
    }
    else {
        return join( '', @out );
    }
}

sub runx {
    my $proc = spawn(@_);
    my @out = $proc->stdout->getlines;
    my @err = $proc->stderr->getlines;

    $proc->close;

    if ( $proc->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' .
        $proc->exit )
          if $CONFESS;
        croak( join( '', @err ) . 'Command exited with value ' . $proc->exit );
    }

    if (wantarray) {
        return @out, @err;
    }
    else {
        return join( '', @out, @err );
    }
}

sub spawn {
    my @cmd = grep { ref $_ ne 'HASH' } @_;

    my $bin = $cmd[0];
    defined $bin || confess '$cmd must be defined';

    if ( !-f $bin and splitdir($bin) < 2 ) {
        $cmd[0] = which($bin);
    }

    my @opts = grep { ref $_ eq 'HASH' } @_;
    if (@opts > 2) {
        confess __PACKAGE__ .": only a single hashref allowed";
    }

    my %args = @opts ? %{$opts[0]} : ();
    $args{cmd} = \@cmd;

    return Sys::Cmd->new( %args );
}

has 'cmd' => (
    is  => 'ro',
    isa => sub {
        ref $_[0] eq 'ARRAY' || confess "cmd must be ARRAYREF";
        @{ $_[0] } || confess "Missing cmd elements";
    },
    required => 1,
);

has 'encoding' => (
    is      => 'ro',
    default => sub { 'utf8' },
);

has 'env' => (
    is  => 'ro',
    isa => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
#    default   => sub { {} },
    predicate => 'have_env',
);

has 'dir' => (
    is      => 'ro',
    default => sub { cwd },
);

has 'input' => (
    is        => 'ro',
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

    if ( $self->have_env ) {
        while (my ($key,$val) = each %{ $self->env } ) {
            if ( defined $val ) {
                $ENV{$key} = $val;
            }
            else {
                delete $ENV{$key};
            }
        }
    }

    my $orig = cwd;
    if ( $self->dir ne $orig ) {
        (chdir $self->dir) || confess "Can't chdir to " . $self->dir . ": $!";
    }

    # spawn the command
    $log->debug( scalar $self->cmdline );

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
          eval { open3( '>&IN_R', '<&OUT_W', '<&ERR_W', $self->cmdline ) };

        # FIXME - better check open3 error conditions
        croak $@ if !defined $pid;

        ( $in, $out, $err ) = ( *IN_W{IO}, *OUT_R{IO}, *ERR_R{IO} );
    }
    else {
        $err = Symbol::gensym;
        $pid = eval { open3( $in, $out, $err, $self->cmdline ); };

        # FIXME - better check open3 error conditions
        croak $@ if !defined $pid;
    }

    $in->autoflush(1);
    binmode $in,  ':encoding(' . $self->encoding . ')';
    binmode $out, ':encoding(' . $self->encoding . ')';
    binmode $err, ':encoding(' . $self->encoding . ')';

    $self->pid($pid);
    $self->stdin($in);
    $self->stdout($out);
    $self->stderr($err);

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
        (chdir $orig) || croak "Can't chdir back to $orig: $!";
    }
    return;
}

sub cmdline {
    my $self = shift;
    if (wantarray) {
        return @{ $self->cmd };
    }
    else {
        return join( ' ', @{ $self->cmd } );
    }
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
    my $pid = waitpid $self->pid, 0;
    # check $?
    my $ret = $?;

    if ( $pid != $self->pid ) {
        croak "Child process already reaped, check for a SIGCHLD handler";
    }

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



