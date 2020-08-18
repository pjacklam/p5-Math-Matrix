#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 11;

{
    my $x = Math::Matrix -> new([[1, 3, 5, 7],
                                 [2, 4, 6, 8]]);
    my $y = $x -> reshape(1, 8);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 2, 3, 4, 5, 6, 7, 8]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 10;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5, 7],
                        [2, 4, 6, 8]], '$x is unmodified');
}

{
    my $x = Math::Matrix -> new([[1, 3, 5, 7],
                                 [2, 4, 6, 8]]);
    my $y = $x -> reshape(4, 2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 5], [2, 6], [3, 7], [4, 8]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 10;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5, 7],
                        [2, 4, 6, 8]], '$x is unmodified');
}

{
    my $x = Math::Matrix -> new([[1, 3, 5, 7],
                                 [2, 4, 6, 8]]);
    my $y = $x -> reshape(8, 1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1], [2], [3], [4], [5], [6], [7], [8]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 10;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5, 7],
                        [2, 4, 6, 8]], '$x is unmodified');
}

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> reshape(0, 0);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [],
              '$y has the right values');
}
