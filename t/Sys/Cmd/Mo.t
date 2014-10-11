use strict;
use warnings;
use Test::More;

our $count;

package Foo;
use Sys::Cmd::Mo qw/build/;
has 'foo' => ( is => 'rw' );

sub BUILD {
    my $self = shift;
    $self->foo( $main::count++ );
}

package Bar;
use Sys::Cmd::Mo qw/default/;
extends 'Foo';
has 'bar' => ( is => 'rw', default => sub { 13 }, );

package Baz;
use Sys::Cmd::Mo qw/build/;
extends 'Bar';
has 'baz' => ( is => 'rw' );

sub BUILD {
    my $self = shift;
    $self->baz( $main::count++ );
}

package Gorch;
use Sys::Cmd::Mo qw/required build/;
extends 'Baz';
has 'gorch' => ( is => 'rw', required => 1 );

package main;

$count = 1;

eval { Gorch->new };
like $@, qr/required/, 'gorch is required';

$count = 1;
my $g = Gorch->new( gorch => undef );
is $g->foo, 1,  'foo builds first';
is $g->baz, 2,  'baz builds second';
is $g->bar, 13, 'bar has default';

done_testing();
