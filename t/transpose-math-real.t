#!perl

use strict;
use warnings;

use Test::More;

use lib 't/lib';
use Math::Matrix::Real;

plan tests => 12;

{
    my $x = Math::Matrix::Real -> new([[1, 2, 3],
                                       [4, 5, 6],
                                       [7, 8, 9]]);
    my $y = $x -> transpose($x);

    is(ref($y), 'Math::Matrix::Real', '$y is a Math::Matrix::Real');
    is_deeply([ @$y ], [[1, 4, 7],
                        [2, 5, 8],
                        [3, 6, 9]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 2, 3],
                        [4, 5, 6],
                        [7, 8, 9]], '$x is unmodified');
}

{
    my $x = Math::Matrix::Real -> new([[1, 2],
                                       [4, 5]]);
    my $y = $x -> transpose($x);

    is(ref($y), 'Math::Matrix::Real', '$y is a Math::Matrix::Real');
    is_deeply([ @$y ], [[1, 4],
                        [2, 5]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 2],
                        [4, 5]], '$x is unmodified');
}

{
    my $x = Math::Matrix::Real -> new([[1]]);
    my $y = $x -> transpose($x);

    is(ref($y), 'Math::Matrix::Real', '$y is a Math::Matrix::Real');
    is_deeply([ @$y ], [[1]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1]], '$x is unmodified');
}

{
    my $x = Math::Matrix::Real -> new([]);
    my $y = $x -> transpose($x);

    is(ref($y), 'Math::Matrix::Real', '$y is a Math::Matrix::Real');
    is_deeply([ @$y ], [], '$y has the right values');
    is_deeply([ @$x ], [], '$x is unmodified');
}
