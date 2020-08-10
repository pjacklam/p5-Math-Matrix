#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 35;

my ($x, $nrow, $ncol);

$x = Math::Matrix -> new([[1,2,3],[4,5,6]]);

($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 2, 'Number of rows in 2-by-3 matrix');
cmp_ok($ncol, '==', 3, 'Number of columns in 2-by-3 matrix');

cmp_ok($x -> nrow(), '==', 2, 'Number of rows in 2-by-3 matrix');
cmp_ok($x -> ncol(), '==', 3, 'Number of columns in 2-by-3 matrix');
cmp_ok($x -> nelm(), '==', 6, 'Number of elements in 2-by-3 matrix');
cmp_ok($x -> npag(), '==', 1, 'Number of pages in 2-by-3 matrix');
cmp_ok($x -> ndim(), '==', 2, 'Number of dimensions in 2-by-3 matrix');

$x = Math::Matrix -> new([[1,2,3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 1, 'Number of rows in 1-by-3 matrix');
cmp_ok($ncol, '==', 3, 'Number of columns in 1-by-3 matrix');

cmp_ok($x -> nrow(), '==', 1, 'Number of rows in 1-by-3 matrix');
cmp_ok($x -> ncol(), '==', 3, 'Number of columns in 1-by-3 matrix');
cmp_ok($x -> nelm(), '==', 3, 'Number of elements in 1-by-3 matrix');
cmp_ok($x -> npag(), '==', 1, 'Number of pages in 1-by-3 matrix');
cmp_ok($x -> ndim(), '==', 1, 'Number of dimensions in 1-by-3 matrix');

$x = Math::Matrix -> new([[1],[2],[3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 3, 'Number of rows in 3-by-1 matrix');
cmp_ok($ncol, '==', 1, 'Number of columns in 3-by-1 matrix');

cmp_ok($x -> nrow(), '==', 3, 'Number of rows in 3-by-1 matrix');
cmp_ok($x -> ncol(), '==', 1, 'Number of columns in 3-by-1 matrix');
cmp_ok($x -> nelm(), '==', 3, 'Number of elements in 3-by-1 matrix');
cmp_ok($x -> npag(), '==', 1, 'Number of pages in 3-by-1 matrix');
cmp_ok($x -> ndim(), '==', 1, 'Number of dimensions in 3-by-1 matrix');

$x = Math::Matrix -> new([[3]]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 1, 'Number of rows in 1-by-1 matrix');
cmp_ok($ncol, '==', 1, 'Number of columns in 1-by-1 matrix');

cmp_ok($x -> nrow(), '==', 1, 'Number of rows in 1-by-1 matrix');
cmp_ok($x -> ncol(), '==', 1, 'Number of columns in 1-by-1 matrix');
cmp_ok($x -> nelm(), '==', 1, 'Number of elements in 1-by-1 matrix');
cmp_ok($x -> npag(), '==', 1, 'Number of pages in 1-by-1 matrix');
cmp_ok($x -> ndim(), '==', 0, 'Number of dimensions in 1-by-1 matrix');

$x = Math::Matrix -> new([]);
($nrow, $ncol) = $x -> size();
cmp_ok($nrow, '==', 0, 'Number of rows in empty matrix');
cmp_ok($ncol, '==', 0, 'Number of columns in empty matrix');

cmp_ok($x -> nrow(), '==', 0, 'Number of rows in empty matrix');
cmp_ok($x -> ncol(), '==', 0, 'Number of columns in empty matrix');
cmp_ok($x -> nelm(), '==', 0, 'Number of elements in empty matrix');
cmp_ok($x -> npag(), '==', 0, 'Number of pages in empty matrix');
cmp_ok($x -> ndim(), '==', 2, 'Number of dimensions in empty matrix');
