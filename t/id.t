#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 30;

my $x;

###############################################################################

note("test id()");

$x = Math::Matrix -> id(0);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [], '$x has the right values');

$x = Math::Matrix -> id(1);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1]], '$x has the right values');

$x = Math::Matrix -> id(2);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0],
                    [0, 1]], '$x has the right values');

$x = Math::Matrix -> id(3);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]], '$x has the right values');

$x = Math::Matrix -> id(4);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0, 0],
                    [0, 1, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]], '$x has the right values');

###############################################################################

note("test new_identity()");

$x = Math::Matrix -> new_identity(0);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [], '$x has the right values');

$x = Math::Matrix -> new_identity(1);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1]], '$x has the right values');

$x = Math::Matrix -> new_identity(2);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0],
                    [0, 1]], '$x has the right values');

$x = Math::Matrix -> new_identity(3);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]], '$x has the right values');

$x = Math::Matrix -> new_identity(4);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0, 0],
                    [0, 1, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]], '$x has the right values');

###############################################################################

note("test eye()");

$x = Math::Matrix -> eye(0);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [], '$x has the right values');

$x = Math::Matrix -> eye(1);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1]], '$x has the right values');

$x = Math::Matrix -> eye(2);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0],
                    [0, 1]], '$x has the right values');

$x = Math::Matrix -> eye(3);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0],
                    [0, 1, 0],
                    [0, 0, 1]], '$x has the right values');

$x = Math::Matrix -> eye(4);
is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
is_deeply([ @$x ], [[1, 0, 0, 0],
                    [0, 1, 0, 0],
                    [0, 0, 1, 0],
                    [0, 0, 0, 1]], '$x has the right values');
