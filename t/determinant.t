#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 2;

my $x = Math::Matrix -> new([1, 3],
                            [5, 4]);

my $y = $x -> determinant();
cmp_ok($y, '==', -11, '$y has the right value');

my $z = Math::Matrix -> new([3, 1, 2],
                            [4, 9, 7],
                            [5, 0, 8]);

my $w = $z -> determinant();
cmp_ok($w, '==', 129, '$w has the right value');
