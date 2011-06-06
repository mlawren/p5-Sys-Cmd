package Sys::Command;
use strict;
use warnings;
use 5.006;
use Moo;
use Carp qw/croak confess/;
use Sub::Exporter -setup => { exports => [qw/spawn run runa/], };
use File::Spec::Functions qw/splitdir/;
use File::Which;
use Sys::Command::Process;
use Log::Any qw/$log/;

our $VERSION = '0.02';
our $CONFESS;

has 'cmd' => (
    is       => 'rw',
    isa      => sub { ref $_[0] eq 'ARRAY' || confess "cmd must be ARRAYREF" },
    required => 1,
);
has 'encoding' => (
    is      => 'rw',
    default => sub { 'utf8' },
);
has 'env' => (
    is  => 'rw',
    isa => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
    default => sub { {} },
);
has 'dir' => ( is => 'rw', );

sub BUILD {
    my $self = shift;

    # Find the full executable if it appears to not already be an
    # absolute path
    my $bin = $self->cmd->[0];
    if ( !-f $bin and splitdir($bin) < 2 ) {
        $self->cmd->[0] = which($bin);
    }
}

sub spawn {
    my @cmd;
    my $env = {};
    my $input;
    my $encoding = 'utf8';

    my $checkcmd;

    # FIXME some stupid Moo bug make this impossible:
    #    if ( eval { $_[0]->isa(__PACKAGE__) } ) {
    if ( ref $_[0] ) {
        my $self = shift;
        unshift @_, @{ $self->cmd };
        $env      = $self->env;
        $encoding = $self->encoding;
    }
    elsif ( defined $_[0] && $_[0] eq __PACKAGE__ ) {
        shift;
    }
    else {
        $checkcmd = 1;
    }

    if ( my $option = ( grep { ref $_ eq 'HASH' } @_ )[0] ) {
        if ( exists $option->{input} ) {
            $input = $option->{input};
        }
        if ( exists $option->{env} ) {
            map { $env->{$_} = $option->{env}->{$_} } keys %{ $option->{env} };
        }
        if ( exists $option->{encoding} ) {
            $encoding = $option->{encoding};
        }
    }

    @cmd = grep { ref $_ ne 'HASH' } @_;

    if ($checkcmd) {
        my $bin = $cmd[0];
        defined $bin || confess '$cmd must be defined';
        if ( !-f $bin and splitdir($bin) < 2 ) {
            $cmd[0] = which($bin);
        }
    }

    return Sys::Command::Process->new(
        cmd => \@cmd,
        defined $input ? ( input => $input ) : (),
        env      => $env,
        encoding => $encoding,
    );
}

sub run {
    my $run = spawn(@_);
    my $out = $run->stdout;
    my $err = $run->stderr;
    my @ret = <$out>;
    my @err = <$err>;

    $run->close;

    if ( $run->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' . $run->exit )
          if $CONFESS;
        croak( join( '', @err ) . 'Command exited with value ' . $run->exit );
    }

    if (wantarray) {
        return @ret;
    }
    else {
        return join( '', @ret );
    }
}

sub runa {
    my $run = spawn(@_);
    my $out = $run->stdout;
    my $err = $run->stderr;
    my @ret = <$out>;
    my @err = <$err>;

    $run->close;

    if ( $run->exit != 0 ) {
        confess( join( '', @err ) . 'Command exited with value ' . $run->exit )
          if $CONFESS;
        croak( join( '', @err ) . 'Command exited with value ' . $run->exit );
    }

    if (wantarray) {
        return @ret, @err;
    }
    else {
        return join( '', @ret, @err );
    }
}

1;
