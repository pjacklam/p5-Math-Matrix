#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 48;

################################################################

note('sum() on a 3-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ],
                                 [ 4,  0,  2, -3,  1 ],
                                 [ 2,  6, -5,  1, -2 ]]);
    my $y = $x -> sum();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 9, 5, 2, 0, 7 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ],
                        [ 4,  0,  2, -3,  1 ],
                        [ 2,  6, -5,  1, -2 ]], '$x is unmodified');
}

note('sum(1) on a 3-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ],
                                 [ 4,  0,  2, -3,  1 ],
                                 [ 2,  6, -5,  1, -2 ]]);
    my $y = $x -> sum(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 9, 5, 2, 0, 7 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ],
                        [ 4,  0,  2, -3,  1 ],
                        [ 2,  6, -5,  1, -2 ]], '$x is unmodified');
}

note('sum(2) on a 3-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ],
                                 [ 4,  0,  2, -3,  1 ],
                                 [ 2,  6, -5,  1, -2 ]]);
    my $y = $x -> sum(2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 17 ],
                        [  4 ],
                        [  2 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ],
                        [ 4,  0,  2, -3,  1 ],
                        [ 2,  6, -5,  1, -2 ]], '$x is unmodified');
}

note('sum(3) on a 3-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ],
                                 [ 4,  0,  2, -3,  1 ],
                                 [ 2,  6, -5,  1, -2 ]]);
    my $y = $x -> sum(3);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 3, -1,  5,  2,  8 ],
                        [ 4,  0,  2, -3,  1 ],
                        [ 2,  6, -5,  1, -2 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ],
                        [ 4,  0,  2, -3,  1 ],
                        [ 2,  6, -5,  1, -2 ]], '$x is unmodified');
}

################################################################

note('sum() on a 3-by-1 matrix');

{
    my $x = Math::Matrix -> new([[ 3 ],
                                 [ 4 ],
                                 [ 2 ]]);
    my $y = $x -> sum();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 9 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$x is unmodified');
}

note('sum(1) on a 3-by-1 matrix');

{
    my $x = Math::Matrix -> new([[ 3 ],
                                 [ 4 ],
                                 [ 2 ]]);
    my $y = $x -> sum(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 9 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$x is unmodified');
}

note('sum(2) on a 3-by-1 matrix');

{
    my $x = Math::Matrix -> new([[ 3 ],
                                 [ 4 ],
                                 [ 2 ]]);
    my $y = $x -> sum(2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$x is unmodified');
}

note('sum(3) on a 3-by-1 matrix');

{
    my $x = Math::Matrix -> new([[ 3 ],
                                 [ 4 ],
                                 [ 2 ]]);
    my $y = $x -> sum(3);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3 ],
                        [ 4 ],
                        [ 2 ]], '$x is unmodified');
}

################################################################

note('sum() on a 1-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ]]);
    my $y = $x -> sum();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 17 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ]], '$x is unmodified');
}

note('sum(1) on a 1-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ]]);
    my $y = $x -> sum(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 3, -1,  5,  2,  8 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ]], '$x is unmodified');
}

note('sum(2) on a 1-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ]]);
    my $y = $x -> sum(2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 17 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ]], '$x is unmodified');
}

note('sum(3) on a 1-by-5 matrix');

{
    my $x = Math::Matrix -> new([[ 3, -1,  5,  2,  8 ]]);
    my $y = $x -> sum(3);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[ 3, -1,  5,  2,  8 ]], '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for my $i (0 .. $nrowy - 1) {
        for my $j (0 .. $ncoly - 1) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[ 3, -1,  5,  2,  8 ]], '$x is unmodified');
}

################################################################

note('sum() on an empty matrix');

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> sum();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [], '$y has the right values');
    is_deeply([ @$x ], [], '$x is unmodified');
}

note('sum(1) on an empty matrix');

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> sum(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [], '$y has the right values');
    is_deeply([ @$x ], [], '$x is unmodified');
}

note('sum(2) on an empty matrix');

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> sum(2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [], '$y has the right values');
    is_deeply([ @$x ], [], '$x is unmodified');
}

note('sum(3) on an empty matrix');

{
    my $x = Math::Matrix -> new([]);
    my $y = $x -> sum(3);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [], '$y has the right values');
    is_deeply([ @$x ], [], '$x is unmodified');
}
