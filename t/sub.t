#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 12;

{
    my $x = Math::Matrix -> new([[ 1, 2, 3],
                                 [ 4, 5, 6]]);
    my $y = Math::Matrix -> new([[ 6, 5, 4,],
                                 [ 3, 2, 1]]);
    my $z = $x -> sub($y);

    is(ref($z), 'Math::Matrix', '$z is a Math::Matrix');
    is_deeply([ @$z ], [[-5, -3, -1], [1, 3, 5]],
              '$z has the right values');

    # Verify that modifying $z does not modify $x or $y.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $z -> [$i][$j] += 10;
        }
    }

    is_deeply([ @$x ], [[ 1, 2, 3],
                        [ 4, 5, 6]], '$x is unmodified');
    is_deeply([ @$y ], [[ 6, 5, 4],
                        [ 3, 2, 1]], '$y is unmodified');
}

{
    my $x = Math::Matrix -> new([]);
    my $y = Math::Matrix -> new([]);
    my $z = $x -> sub($y);

    is(ref($z), 'Math::Matrix', '$z is a Math::Matrix');
    is_deeply([ @$z ], [],
              '$z has the right values');
}

# Test overloading.

{
    my $x = Math::Matrix -> new([[3]]);
    my $y = $x - 4;
    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[-1]],
              '$y has the right values');
}

{
    my $x = Math::Matrix -> new([[3]]);
    my $y = 4 - $x;
    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1]],
              '$y has the right values');
}

{
    my $x = Math::Matrix -> new([[3]]);
    $x -= 4;
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    is_deeply([ @$x ], [[-1]],
              '$x has the right values');
}
