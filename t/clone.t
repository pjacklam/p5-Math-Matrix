#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 5;

my ($x, $xdata, $y, $ydata);

# Create an object.

$xdata = [[1, 2, 3], [4, 5, 6]];
$x = Math::Matrix -> new(@$xdata);
isa_ok($x, 'Math::Matrix');
is_deeply([ @$x ], $xdata, '$x has the right values');

# Create the clone.

$y = $x -> clone();
isa_ok($y, 'Math::Matrix');
is_deeply([ @$x ], $xdata, '$x is unmodified');
is_deeply([ @$y ], $xdata, '$y has the same values as $x');
