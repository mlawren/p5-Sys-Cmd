package    # Trick xt/ tests into working
  Sys::Cmd::Mo;

BEGIN {
#<<< No perltidy
# use Mo qw/build is required default import/;
#   The following line of code was produced from the previous line by
#   Mo::Inline version 0.39
no warnings;my$M=__PACKAGE__.'::';*{$M.Object::new}=sub{my$c=shift;my$s=bless{@_},$c;my%n=%{$c.::.':E'};map{$s->{$_}=$n{$_}->()if!exists$s->{$_}}keys%n;$s};*{$M.import}=sub{import warnings;$^H|=1538;my($P,%e,%o)=caller.'::';shift;eval"no Mo::$_",&{$M.$_.::e}($P,\%e,\%o,\@_)for@_;return if$e{M};%e=(extends,sub{eval"no $_[0]()";@{$P.ISA}=$_[0]},has,sub{my$n=shift;my$m=sub{$#_?$_[0]{$n}=$_[1]:$_[0]{$n}};@_=(default,@_)if!($#_%2);$m=$o{$_}->($m,$n,@_)for sort keys%o;*{$P.$n}=$m},%e,);*{$P.$_}=$e{$_}for keys%e;@{$P.ISA}=$M.Object};*{$M.'build::e'}=sub{my($P,$e)=@_;$e->{new}=sub{$c=shift;my$s=&{$M.Object::new}($c,@_);my@B;do{@B=($c.::BUILD,@B)}while($c)=@{$c.::ISA};exists&$_&&&$_($s)for@B;$s}};*{$M.'is::e'}=sub{my($P,$e,$o)=@_;$o->{is}=sub{my($m,$n,%a)=@_;$a{is}or return$m;sub{$#_&&$a{is}eq'ro'&&caller ne'Mo::coerce'?die$n.' is ro':$m->(@_)}}};*{$M.'required::e'}=sub{my($P,$e,$o)=@_;$o->{required}=sub{my($m,$n,%a)=@_;if($a{required}){my$C=*{$P."new"}{CODE}||*{$M.Object::new}{CODE};no warnings 'redefine';*{$P."new"}=sub{my$s=$C->(@_);my%a=@_[1..$#_];if(!exists$a{$n}){require Carp;Carp::croak($n." required")}$s}}$m}};*{$M.'default::e'}=sub{my($P,$e,$o)=@_;$o->{default}=sub{my($m,$n,%a)=@_;exists$a{default}or return$m;my($d,$r)=$a{default};my$g='HASH'eq($r=ref$d)?sub{+{%$d}}:'ARRAY'eq$r?sub{[@$d]}:'CODE'eq$r?$d:sub{$d};my$i=exists$a{lazy}?$a{lazy}:!${$P.':N'};$i or ${$P.':E'}{$n}=$g and return$m;sub{$#_?$m->(@_):!exists$_[0]{$n}?$_[0]{$n}=$g->(@_):$m->(@_)}}};my$i=\&import;*{$M.import}=sub{(@_==2 and not$_[1])?pop@_:@_==1?push@_,grep!/import/,@f:();goto&$i};@f=qw[build is required default import];use strict;use warnings;
$INC{'Sys/Cmd/Mo.pm'} = __FILE__;
#>>>
}
1;

package Sys::Cmd;
use strict;
use warnings;
use 5.006;
use Carp qw/carp confess croak/;
use Exporter::Tidy all => [qw/spawn run runx/];
use IO::Handle;
use File::chdir;
use Log::Any qw/$log/;
use Sys::Cmd::Mo;
use POSIX qw/WNOHANG _exit/;

our $VERSION = '0.81.8';
our $CONFESS;

sub run {
    my $proc = spawn(@_);
    my @out  = $proc->stdout->getlines;
    my @err  = $proc->stderr->getlines;

    $proc->wait_child;

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

    $proc->wait_child;

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

    defined $cmd[0] || confess '$cmd must be defined';

    unless ( ref $cmd[0] eq 'CODE' ) {
        if ( !-e $cmd[0] ) {
            require File::Which;
            $cmd[0] = File::Which::which( $cmd[0] )
              || confess 'command not found: ' . $cmd[0];
        }

        if ( !-f $cmd[0] ) {
            confess 'command not a file: ' . $cmd[0];
        }

        if ( !-x $cmd[0] ) {
            confess 'command not executable: ' . $cmd[0];
        }
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
    is  => 'ro',
    isa => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
);

has 'dir' => (
    is      => 'ro',
    default => sub { $CWD },
);

has 'input' => ( is => 'ro', );

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

has on_exit => (
    is       => 'rw',
    init_arg => 'on_exit',
);

has 'exit' => (
    is       => 'rw',
    init_arg => undef,
);

has 'signal' => (
    is       => 'rw',
    init_arg => undef,
);

has 'core' => (
    is       => 'rw',
    init_arg => undef,
);

my @children;

sub BUILD {
    my $self = shift;
    local $CWD = $self->dir;
    local %ENV = %ENV;

    if ( defined( my $x = $self->env ) ) {
        while ( my ( $key, $val ) = each %$x ) {
            if ( defined $val ) {
                $ENV{$key} = $val;
            }
            else {
                delete $ENV{$key};
            }
        }
    }

    push( @children, $self );
    $SIG{CHLD} ||= \&_reap if $self->on_exit;

    if ( $^O eq 'MSWin32' ) {
        $self->_spawn;
    }
    else {
        $self->_fork;
    }

    $log->debugf( '(PID %d) %s', $self->pid, scalar $self->cmdline );

    my $enc = ':encoding(' . $self->encoding . ')';
    binmode $self->stdin,  $enc;
    binmode $self->stdout, $enc;
    binmode $self->stderr, $enc;

    # some input was provided
    if ( defined( my $input = $self->input ) ) {
        local $SIG{PIPE} =
          sub { warn "Broken pipe when writing to:" . $self->cmdline };

        print { $self->stdin } $input if length $input;

        $self->stdin->close;
    }

    return;
}

sub _spawn {
    my $self = shift;

    require Proc::FastSpawn;
    Proc::FastSpawn->import(qw/fd_inherit/);

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

    # Get handles to descriptors 0,1,2
    my $fd0 = IO::Handle->new_from_fd( 0, '<' );
    my $fd1 = IO::Handle->new_from_fd( 1, '>' );
    my $fd2 = IO::Handle->new_from_fd( 2, '>' );

    # Dup the 0,1,2 descriptors
    open my $stdin,  '<&', 0;
    open my $stdout, '>&', 1;
    open my $stderr, '>&', 2;

    # Re-open 0,1,2 by duping the pipe end
    open $fd0, '<&', fileno($r_in);
    open $fd1, '>&', fileno($w_out);
    open $fd2, '>&', fileno($w_err);

    # Make sure that 0,1,2 are inherited (probably are anyway)
    fd_inherit( 0, 1 );
    fd_inherit( 1, 1 );
    fd_inherit( 2, 1 );

    # But don't inherit these
    fd_inherit( fileno($stdin),  0 );
    fd_inherit( fileno($stdout), 0 );
    fd_inherit( fileno($stderr), 0 );
    fd_inherit( fileno($r_in),   0 );
    fd_inherit( fileno($r_out),  0 );
    fd_inherit( fileno($r_err),  0 );
    fd_inherit( fileno($w_in),   0 );
    fd_inherit( fileno($w_out),  0 );
    fd_inherit( fileno($w_err),  0 );

    my @cmd = $self->cmdline;
    my $cmd = $cmd[0];
    my @env = map { "$_=$ENV{$_}" } keys %ENV;

    $self->pid( Proc::FastSpawn::spawn( $cmd, \@cmd, \@env ) );

    # dup fd 0,1,2 again from the proper place
    open $fd0, '<&', fileno($stdin);
    open $fd1, '>&', fileno($stdout);
    open $fd2, '>&', fileno($stderr);

    # and restore things for perl
    open STDIN,  '<&=', fileno($fd0);
    open STDOUT, '>&=', fileno($fd1);
    open STDERR, '>&=', fileno($fd2);

    close($r_in);
    close($w_out);
    close($w_err);

    $self->stdin($w_in);
    $self->stdout($r_out);
    $self->stderr($r_err);
    return;
}

sub _fork {
    my $self  = shift;
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

    $self->pid( fork() );
    if ( !defined $self->pid ) {
        my $why = $!;
        pop @children;
        die "fork: $why";
    }

    if ( $self->pid == 0 ) {    # Child
        $SIG{CHLD} = 'DEFAULT';
        $self->exit(0);         # stop DESTROY() from trying to reap

        if ( !open STDERR, '>&=', fileno($w_err) ) {
            print $w_err "open: $! at ", caller, "\n";
            die "open: $!";
        }
        open STDIN,  '<&=', fileno($r_in)  || die "open: $!";
        open STDOUT, '>&=', fileno($w_out) || die "open: $!";

        close $r_out;
        close $r_err;
        close $r_in;
        close $w_in;
        close $w_out;
        close $w_err;

        if ( ref $self->cmd->[0] eq 'CODE' ) {
            my $enc = ':encoding(' . $self->encoding . ')';
            binmode STDIN,  $enc;
            binmode STDOUT, $enc;
            binmode STDERR, $enc;
            $self->cmd->[0]->();
            _exit(0);
        }

        exec( $self->cmdline );
        die "exec: $!";
    }

    # Parent continues from here
    close $r_in;
    close $w_out;
    close $w_err;

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

# A signal handler, not a method
sub _reap {
    my $sig = shift;
    my $try = shift || '';

    croak '_reap("CHLD",[$pid])' unless $sig eq 'CHLD';

    while (1) {
        my $pid;
        local $?;
        local $!;

        if ($try) {
            $pid = waitpid $try, 0;
            $try = undef;
        }
        else {
            $pid = waitpid -1, &WNOHANG;
        }

        my $ret = $?;

        if ( $pid == -1 ) {

            # No child processes running
            last;
        }
        elsif ( $pid == 0 ) {

            # child processes still running, but not ours??
            last;
        }

        if ( $ret == -1 ) {

            # So waitpid returned a PID but then sets $? to this
            # strange value? (Strange in that tests randomly show it to
            # be invalid.) Most likely a perl bug; I think that waitpid
            # got interrupted and when it restarts/resumes the status
            # is lost.
            #
            # See http://www.perlmonks.org/?node_id=641620 for a
            # possibly related discussion.
            #
            # However, since I localised $? and $! above I haven't seen
            # this problem again, so I hope that is a good enough work
            # around. Lets warn any way so that we know when something
            # dodgy is going on.
            warn __PACKAGE__
              . ' received invalid child exit status for pid '
              . $pid
              . ' Setting to 0';
            $ret = 0;

        }

        my @dead = grep { defined $_ && $_->pid == $pid } @children;
        @children = grep { defined $_ && $_->pid != $pid } @children;

        if ( @children and !@dead ) {
            warn __PACKAGE__
              . ' not our child: '
              . $pid
              . ' exit '
              . ( $ret >> 8 )
              . ' signal '
              . ( $ret & 127 )
              . ' core '
              . ( $ret & 128 );
            next;
        }

        foreach my $child (@dead) {
            $log->debugf( '(PID %d) exit: %d signal:%d core:%d',
                $child->pid, $ret >> 8, $ret & 127, $ret & 128 );

            $child->exit( $ret >> 8 );
            $child->signal( $ret & 127 );
            $child->core( $ret & 128 );

            if ( my $subref = $child->on_exit ) {
                $subref->($child);
            }
        }
    }

    return;
}

sub wait_child {
    my $self = shift;

    _reap( 'CHLD', $self->pid ) unless defined $self->exit;
    return;
}

sub close {
    my $self = shift;

    $self->stdin->opened
      && ( $self->stdin->close || carp "error closing stdin: $!" );

    $self->stdout->opened
      && ( $self->stdout->close || carp "error closing stdout: $!" );

    $self->stderr->opened
      && ( $self->stderr->close || carp "error closing stderr: $!" );

    return;
}

sub DESTROY {
    my $self = shift;
    _reap( 'CHLD', $self->pid ) unless defined $self->exit;
    return;
}

1;

__END__

=head1 NAME

Sys::Cmd - run a system command or spawn a system processes

=head1 VERSION

0.81.8 (2014-10-31) Development release

=head1 SYNOPSIS

    use Sys::Cmd qw/run spawn/;

    # Get command output, raise exception on failure:
    $output = run(@cmd);

    # Feed command some input, get output as lines,
    # raise exception on failure:
    @output = run(@cmd, { input => 'feedme' });

    # Spawn and interact with a process somewhere else:
    $proc = spawn( @cmd, { dir => '/' , encoding => 'iso-8859-3'} );

    while (my $line = $proc->stdout->getline) {
        $proc->stdin->print("thanks");
    }

    my @errors = $proc->stderr->getlines;

    $proc->close();       # Finished talking
    $proc->wait_child();  # Cleanup

    # read exit information
    $proc->exit();      # exit status
    $proc->signal();    # signal
    $proc->core();      # core dumped? (boolean)

=head1 DESCRIPTION

B<Sys::Cmd> lets you run system commands and capture their output, or
spawn and interact with a system process through its C<STDIN>,
C<STDOUT>, and C<STDERR> file handles. The following functions are
exported on demand by this module:

=over 4

=item run( @cmd, [\%opt] ) => $output | @output

Execute C<@cmd> and return what the command sent to its C<STDOUT>,
raising an exception in the event of error. In array context returns a
list instead of a plain string.

The first element of C<@cmd> will be looked up using L<File::Which> if
it doesn't exist as a relative file name is is a CODE reference (UNIX
only).  The command input and environment can be modified with an
optional hashref containing the following key/values:

=over 4

=item dir

The working directory the command will be run in.

=item encoding

An string value identifying the encoding of the input/output
file-handles. Defaults to 'utf8'.

=item env

A hashref containing key/values to be added to the current environment
at run-time. If a key has an undefined value then the key is removed
from the environment altogether.

=item input

A string which is fed to the command via its standard input, which is
then closed.

=back

=item runx( @cmd, [\%opt] ) => $outerrput | @outerrput

The same as the C<run> function but with the command's C<STDERR> output
appended to the C<STDOUT> output.

=item spawn( @cmd, [\%opt] ) => Sys::Cmd

Return a B<Sys::Cmd> object (documented below) representing the process
running @cmd, with attributes set according to the optional \%opt
hashref.  The first element of the C<@cmd> array is looked up using
L<File::Which> if it cannot be found in the file-system as a relative
file name or it is a CODE reference (UNIX only).

=back

B<Sys::Cmd> objects can of course be created using the standard C<new>
constructor if you prefer that to the C<spawn> function:

    $proc = Sys::Cmd->new(
        cmd => \@cmd,
        dir => '/',
        env => { SOME => 'VALUE' },
        enc => 'iso-8859-3',
        input => 'feedme',
        on_exit => sub {
            my $proc = shift;
            print $proc->pid .' exited with '. $proc->exit;
        },
    );

Note that B<Sys::Cmd> objects created this way will not lookup the
command using L<File::Which> the way the C<run>, C<runx> and C<spawn>
functions do.

B<Sys::Cmd> uses L<Log::Any> C<debug> calls for logging purposes.

=head1 CONSTRUCTOR

=over 4

=item new(%args) => Sys::Cmd

Spawns a process based on %args. %args must contain at least a C<cmd>
value, and optionally C<encoding>, C<env>, C<dir> and C<input> values
as defined as attributes below.

If an C<on_exit> subref argument is provided a SIGCHLD handler will be
installed (process wide!) which is called asynchronously (with the
B<Sys::Cmd> object as first argument) when the child exits.

=back

=head1 ATTRIBUTES

All attributes are read-only.

=over 4

=item cmd

An array ref containing the command or CODE reference (UNIX only) and
its arguments.

=item dir

The working directory the command will be run in.

=item encoding

An string value identifying the encoding of the input/output
file-handles. Defaults to 'utf8'.

=item env

A hashref containing key/values to be added to the current environment
at run-time. If a key has an undefined value then the key is removed
from the environment altogether.

=item input

A string which is fed to the command via its standard input, which is
then closed. This is a shortcut for printing to, and closing the
command's I<stdin> file-handle. An empty string will close the
command's standard input without writing to it. On some systems, some
commands may close standard input on startup, which will cause a
SIGPIPE when trying to write to it. This will raise an exception.

=item pid

The command's process ID.

=item stdin

The command's I<STDIN> file handle, based on L<IO::Handle> so you can
call print() etc methods on it. Autoflush is automatically enabled on
this handle.

=item stdout

The command's I<STDOUT> file handle, based on L<IO::Handle> so you can
call getline() etc methods on it.

=item stderr

The command's I<STDERR> file handle, based on L<IO::Handle> so you can
call getline() etc methods on it.

=item exit

The command's exit value, shifted by 8 (see "perldoc -f system"). Set
either when a SIGCHLD is received or after a call to C<wait_child()>.

=item signal

The signal number (if any) that terminated the command, bitwise-added
with 127 (see "perldoc -f system"). Set either when a SIGCHLD is
received or after a call to C<wait_child()>.


=item core

A boolean indicating the process core was dumped. Set either when a
SIGCHLD is received or after a call to C<wait_child()>.


=back

=head1 METHODS

=over 4

=item cmdline => @list | $str

In array context returns a list of the command and its arguments.  In
scalar context returns a string of the command and its arguments joined
together by spaces.

=item close()

Close all pipes to the child process.  This method is automatically
called when the C<Sys::Cmd> object is destroyed.  Annoyingly, this
means that in the following example C<$fh> will be closed when you
tried to use it:

    my $fh = Sys::Cmd->new( %args )->stdout;

So you have to keep track of the Sys::Cmd object manually.

=item wait_child()

Wait for the child to exit and collect the exit status. This method is
resposible for setting the I<exit>, I<signal> and I<core> attributes.

=back

=head1 SEE ALSO

L<Sys::Cmd::Template>

=head1 ALTERNATIVES

L<AnyEvent::Run>, L<AnyEvent::Util>, L<Argv>, L<Capture::Tiny>,
L<Child>, L<Forks::Super>, L<IO::Pipe>, L<IPC::Capture>, L<IPC::Cmd>,
L<IPC::Command::Multiplex>, L<IPC::Exe>, L<IPC::Open3>,
L<IPC::Open3::Simple>, L<IPC::Run>, L<IPC::Run3>,
L<IPC::RunSession::Simple>, L<IPC::ShellCmd>, L<IPC::System::Simple>,
L<POE::Pipe::TwoWay>, L<Proc::Background>, L<Proc::Fork>,
L<Proc::Spawn>, L<Spawn::Safe>, L<System::Command>

=head1 SUPPORT

This distribution is managed via github:

    https://github.com/mlawren/sys-cmd/tree/devel

This distribution follows the semantic versioning model:

    http://semver.org/

Code is tidied up on Git commit using githook-perltidy:

    http://github.com/mlawren/githook-perltidy

=head1 AUTHOR

Mark Lawrence E<lt>nomad@null.netE<gt>, based heavily on
L<Git::Repository::Command> by Philippe Bruhat (BooK).

=head1 COPYRIGHT AND LICENSE

Copyright 2011-2014 Mark Lawrence <nomad@null.net>

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3 of the License, or (at your
option) any later version.

