#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 15;

my ($x, $nrow, $ncol);

# constant()

$x = Math::Matrix -> constant(7, 2, 3);
is_deeply([ @$x ], [[7, 7, 7],[7, 7, 7]],
          '$x = Math::Matrix -> constant(7, 2, 3);');

$x = Math::Matrix -> constant(7, 1, 3);
is_deeply([ @$x ], [[7, 7, 7]],
          '$x = Math::Matrix -> constant(7, 1, 3);');

$x = Math::Matrix -> constant(7, 3, 1);
is_deeply([ @$x ], [[7], [7], [7]],
          '$x = Math::Matrix -> constant(7, 3, 1);');

$x = Math::Matrix -> constant(7, 3);
is_deeply([ @$x ], [[7], [7], [7]],
          '$x = Math::Matrix -> constant(7, 3);');

$x = Math::Matrix -> constant(7);
is_deeply([ @$x ], [],
          '$x = Math::Matrix -> constant(7);');

# zeros()

$x = Math::Matrix -> zeros(2, 3);
is_deeply([ @$x ], [[0, 0, 0],[0, 0, 0]],
          '$x = Math::Matrix -> zeros(2, 3);');

$x = Math::Matrix -> zeros(1, 3);
is_deeply([ @$x ], [[0, 0, 0]],
          '$x = Math::Matrix -> zeros(1, 3);');

$x = Math::Matrix -> zeros(3, 1);
is_deeply([ @$x ], [[0], [0], [0]],
          '$x = Math::Matrix -> zeros(3, 1);');

$x = Math::Matrix -> zeros(3);
is_deeply([ @$x ], [[0], [0], [0]],
          '$x = Math::Matrix -> zeros(3);');

$x = Math::Matrix -> zeros();
is_deeply([ @$x ], [],
          '$x = Math::Matrix -> zeros();');

# ones()

$x = Math::Matrix -> ones(2, 3);
is_deeply([ @$x ], [[1, 1, 1],[1, 1, 1]],
          '$x = Math::Matrix -> ones(2, 3);');

$x = Math::Matrix -> ones(1, 3);
is_deeply([ @$x ], [[1, 1, 1]],
          '$x = Math::Matrix -> ones(1, 3);');

$x = Math::Matrix -> ones(3, 1);
is_deeply([ @$x ], [[1], [1], [1]],
          '$x = Math::Matrix -> ones(3, 1);');

$x = Math::Matrix -> ones(3);
is_deeply([ @$x ], [[1], [1], [1]],
          '$x = Math::Matrix -> ones(3);');

$x = Math::Matrix -> ones();
is_deeply([ @$x ], [],
          '$x = Math::Matrix -> ones();');
