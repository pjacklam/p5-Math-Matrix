#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 8;

my ($x, $xdata, $y, $ydata);

# Class method.

$xdata = [[1, 2, 3], [4, 5, 6]];
$x = Math::Matrix -> new(@$xdata);
isa_ok($x, 'Math::Matrix');
is_deeply([ @$x ], $xdata, '$x has the right values');

# Instance method without arguments.

$y = $x -> new();
isa_ok($y, 'Math::Matrix');
is_deeply([ @$x ], $xdata, '$x is unmodified');
is_deeply([ @$y ], [[0, 0, 0], [0, 0, 0]], '$y is a zero matrix');

# Instance method with arguments.

$ydata = [[9, 8, 7], [6, 5, 4]];
$y = $x -> new(@$ydata);
isa_ok($y, 'Math::Matrix');
is_deeply([ @$x ], $xdata, '$x is unmodified');
is_deeply([ @$y ], $ydata, '$y has the right values');
