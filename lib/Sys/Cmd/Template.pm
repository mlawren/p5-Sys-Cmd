package Sys::Cmd::Template;
use strict;
use warnings;
use 5.006;
use Carp qw/croak confess/;
use Exporter::Tidy default => [qw/cmd_template/];
use File::Spec::Functions qw/splitdir/;
use File::Which;
use Sys::Cmd::Mo qw/is default/;
use Sys::Cmd;

our $VERSION = '0.81.6';
our $CONFESS;

sub cmd_template {
    my @cmd = grep { ref $_ ne 'HASH' } @_;

    my $bin = $cmd[0];
    defined $bin || confess '$cmd must be defined';

    if ( !-f $bin and splitdir($bin) < 2 ) {
        $cmd[0] = which($bin);
    }

    my $opts = ( grep { ref $_ eq 'HASH' } @_ )[0];

    my %args = $opts ? %$opts : ();
    $args{cmd} = \@cmd;

    return Sys::Cmd::Template->new(%args);
}

has 'cmd' => (
    is  => 'rw',
    isa => sub { ref $_[0] eq 'ARRAY' || confess "cmd must be ARRAYREF" },
    default => sub { [] },
);

has 'dir' => (
    is        => 'rw',
    predicate => 'have_dir',
);

has 'encoding' => (
    is        => 'rw',
    predicate => 'have_encoding',
);

has 'env' => (
    is        => 'rw',
    isa       => sub { ref $_[0] eq 'HASH' || confess "env must be HASHREF" },
    predicate => 'have_env',
);

has 'input' => (
    is        => 'rw',
    predicate => 'have_input',
);

sub run {
    my $self = shift;
    my $proc = $self->spawn(@_);
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
    my $self = shift;
    my $proc = $self->spawn(@_);
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
    my $self = shift;
    my %args = ( cmd => [ @{ $self->cmd }, grep { ref $_ ne 'HASH' } @_ ], );

    my $opts = ( grep { ref $_ eq 'HASH' } @_ )[0] || {};

    if ( exists $opts->{dir} ) {
        $args{dir} = $opts->{dir};
    }
    elsif ( $self->have_dir ) {
        $args{dir} = $self->dir;
    }

    if ( exists $opts->{encoding} ) {
        $args{encoding} = $opts->{encoding};
    }
    elsif ( $self->have_encoding ) {
        $args{encoding} = $self->encoding;
    }

    if ( $self->have_env ) {
        $args{env} = { %{ $self->env } };
    }

    if ( exists $opts->{env} ) {
        while ( my ( $key, $val ) = each %{ $opts->{env} } ) {
            $args{env}->{$key} = $val;
        }
    }

    if ( exists $opts->{input} ) {
        $args{input} = $opts->{input};
    }
    elsif ( $self->have_input ) {
        $args{input} = $self->input;
    }

    return Sys::Cmd->new(%args);
}

1;
