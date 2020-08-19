#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 5;

{
    my $x = Math::Matrix -> new([[1, 2, 3, 4],
                                 [5, 6, 7, 8]]);
    my $y = $x -> rot270();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[5, 1],
                        [6, 2],
                        [7, 3],
                        [8, 4]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 10;
        }
    }

    is_deeply([ @$x ], [[1, 2, 3, 4],
                        [5, 6, 7, 8]], '$x is unmodified');
}

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> rot270();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [],
              '$y has the right values');
}
