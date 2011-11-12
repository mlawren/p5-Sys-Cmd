package Sys::Cmd;
use strict;
use warnings;
use 5.006;
use Moo;
use Carp qw/carp confess croak/;
use Cwd qw/cwd/;
use IO::Handle;
use File::chdir;
use File::Which qw/which/;
use Log::Any qw/$log/;
use File::Spec::Functions qw/splitdir/;
use Sub::Exporter -setup => { exports => [qw/spawn run runx/], };

our $VERSION = '0.07_2';
our $CONFESS;

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
    my @out  = $proc->stdout->getlines;
    my @err  = $proc->stderr->getlines;

    $proc->close;

    if ( $proc->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' . $proc->exit )
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
    my @out  = $proc->stdout->getlines;
    my @err  = $proc->stderr->getlines;

    $proc->close;

    if ( $proc->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' . $proc->exit )
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
        $cmd[0] = which($bin) || confess 'command not found: ' . $bin;
    }

    my @opts = grep { ref $_ eq 'HASH' } @_;
    if ( @opts > 2 ) {
        confess __PACKAGE__ . ": only a single hashref allowed";
    }

    my %args = @opts ? %{ $opts[0] } : ();
    $args{cmd} = \@cmd;

    return Sys::Cmd->new(%args);
}

has 'cmd' => (
    is  => 'ro',
    isa => sub {
        ref $_[0] eq 'ARRAY' || confess "cmd must be ARRAYREF";
        @{ $_[0] } || confess "Missing cmd elements";
        if ( grep { !defined $_ } @{ $_[0] } ) {
            confess 'cmd array cannot contain undef elements';
        }
    },
    required => 1,
);

has 'encoding' => (
    is      => 'ro',
    default => sub { 'utf8' },
);

has 'env' => (
    is        => 'ro',
    isa       => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
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

sub BUILD {
    my $self = shift;
    local $CWD = $self->dir;

    my $r_in  = IO::Handle->new;
    my $r_out = IO::Handle->new;
    my $r_err = IO::Handle->new;
    my $w_in  = IO::Handle->new;
    my $w_out = IO::Handle->new;
    my $w_err = IO::Handle->new;

    $w_in->autoflush(1);
    $w_out->autoflush(1);
    $w_err->autoflush(1);

    pipe( $r_in,  $w_in )  || die "pipe: $!";
    pipe( $r_out, $w_out ) || die "pipe: $!";
    pipe( $r_err, $w_err ) || die "pipe: $!";

    # spawn the command
    $log->debug( scalar $self->cmdline );

    my $pid = fork();
    die "fork: $!" unless defined $pid;

    if ( $pid == 0 ) {    # Child
        if ( !open $REAL_STDERR, '>&=', fileno($w_err) ) {
            print $w_err "open: $! at ", caller, "\n";
            die "open: $!";
        }
        open $REAL_STDIN,  '<&=', fileno($r_in)  || die "open: $!";
        open $REAL_STDOUT, '>&=', fileno($w_out) || die "open: $!";

        close $r_out;
        close $r_err;
        close $r_in;
        close $w_in;
        close $w_out;
        close $w_err;

        if ( $self->have_env ) {
            while ( my ( $key, $val ) = each %{ $self->env } ) {
                if ( defined $val ) {
                    $ENV{$key} = $val;
                }
                else {
                    delete $ENV{$key};
                }
            }
        }

        exec( $self->cmdline );
    }

    # Parent continues from here
    close $r_in;
    close $w_out;
    close $w_err;

    my $enc = ':encoding(' . $self->encoding . ')';

    binmode $w_in,  $enc;
    binmode $r_out, $enc;
    binmode $r_err, $enc;

    # some input was provided
    if ( $self->have_input ) {
        local $SIG{PIPE} =
          sub { warn "Broken pipe when writing to:" . $self->cmdline };

        print {$w_in} $self->input if length $self->input;

        $w_in->close;
    }

    $self->pid($pid);
    $self->stdin($w_in);
    $self->stdout($r_out);
    $self->stderr($r_err);
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

    $in->opened  and $in->close  || carp "error closing stdin: $!";
    $out->opened and $out->close || carp "error closing stdout: $!";
    $err->opened and $err->close || carp "error closing stderr: $!";

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

