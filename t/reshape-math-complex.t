#!perl

use strict;
use warnings;

use Test::More;

# Ensure a recent version of Math::Complex. Math Complex didn't support any way
# of cloning/copying Math::Complex objects before version 1.57.

my $min_math_complex_ver = 1.57;
eval "use Math::Complex $min_math_complex_ver";
plan skip_all => "Math::Complex $min_math_complex_ver required" if $@;

use lib 't/lib';
use Math::Matrix::Complex;

plan tests => 11;

{
    my $x = Math::Matrix::Complex -> new([[1, 3, 5, 7],
                                          [2, 4, 6, 8]]);
    my $y = $x -> reshape(1, 8);

    is(ref($y), 'Math::Matrix::Complex', '$y is a Math::Matrix::Complex');
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
    my $x = Math::Matrix::Complex -> new([[1, 3, 5, 7],
                                          [2, 4, 6, 8]]);
    my $y = $x -> reshape(4, 2);

    is(ref($y), 'Math::Matrix::Complex', '$y is a Math::Matrix::Complex');
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
    my $x = Math::Matrix::Complex -> new([[1, 3, 5, 7],
                                          [2, 4, 6, 8]]);
    my $y = $x -> reshape(8, 1);

    is(ref($y), 'Math::Matrix::Complex', '$y is a Math::Matrix::Complex');
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
    my $x = Math::Matrix::Complex -> new([]);
    my $y = $x -> reshape(0, 0);

    is(ref($y), 'Math::Matrix::Complex', '$y is a Math::Matrix::Complex');
    is_deeply([ @$y ], [],
              '$y has the right values');
}
