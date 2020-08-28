#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 8;

my $x;

$x = Math::Matrix -> new([[0]]);
cmp_ok($x -> bandwidth(), '==', 0,
       'Math::Matrix -> new([[0]])');

$x = Math::Matrix -> new([[1]]);
cmp_ok($x -> bandwidth(), '==', 0,
       'Math::Matrix -> new([[1]])');

$x = Math::Matrix -> new([[1,0],
                          [0,2]]);
cmp_ok($x -> bandwidth(), '==', 0,
       'Math::Matrix -> new([[1,0],[0,2]])');

$x = Math::Matrix -> new([[1,3],
                          [4,2]]);
cmp_ok($x -> bandwidth(), '==', 1,
       'Math::Matrix -> new([[1,3],[4,2]])');

$x = Math::Matrix -> new([[1,3,0],
                          [4,2,5],
                          [0,6,7]]);
cmp_ok($x -> bandwidth(), '==', 1,
       'Math::Matrix -> new([[1,3,0],[4,2,5],[0,6,7]])');

$x = Math::Matrix -> new([[1,3,8],
                          [4,2,5],
                          [9,6,7]]);
cmp_ok($x -> bandwidth(), '==', 2,
       'Math::Matrix -> new([[1,3,8],[4,2,5],[9,6,7]])');

$x = Math::Matrix -> new([[1,3,8,0],
                          [4,2,5,2],
                          [9,6,7,1],
                          [0,3,5,4]]);
cmp_ok($x -> bandwidth(), '==', 2,
       'Math::Matrix -> new([[1,3,8,0],[4,2,5,2],[9,6,7,1],[0,3,5,4]])');


$x = Math::Matrix -> new([[1,3,8,9],
                          [4,2,5,2],
                          [9,6,7,1],
                          [2,3,5,4]]);
cmp_ok($x -> bandwidth(), '==', 3,
       'Math::Matrix -> new([[1,3,8,9],[4,2,5,2],[9,6,7,1],[3,3,5,4]])');
