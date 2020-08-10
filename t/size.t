#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 10;

my ($x, $nrow, $ncol);

$x = Math::Matrix -> new([[1,2,3],[4,5,6]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 2, 'Number of rows in 2-by-3 matrix');
cmp_ok($ncol, '==', 3, 'Number of columns in 2-by-3 matrix');

$x = Math::Matrix -> new([[1,2,3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 1, 'Number of rows in 1-by-3 matrix');
cmp_ok($ncol, '==', 3, 'Number of columns in 1-by-3 matrix');

$x = Math::Matrix -> new([[1],[2],[3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 3, 'Number of rows in 3-by-1 matrix');
cmp_ok($ncol, '==', 1, 'Number of columns in 3-by-1 matrix');

$x = Math::Matrix -> new([[3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 1, 'Number of rows in 1-by-1 matrix');
cmp_ok($ncol, '==', 1, 'Number of columns in 1-by-1 matrix');

$x = Math::Matrix -> new([]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 0, 'Number of rows in empty matrix');
cmp_ok($ncol, '==', 0, 'Number of columns in empty matrix');
