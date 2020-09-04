#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 36;

note('tril() on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 0, 0],
                        [2, 4, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril(-2) on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril(-2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[0, 0, 0],
                        [0, 0, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril(-1) on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril(-1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[0, 0, 0],
                        [2, 0, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril(0) on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril(0);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 0, 0],
                        [2, 4, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril(1) on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 3, 0],
                        [2, 4, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril(2) on 2-by-3 matrix');

{
    my $x = Math::Matrix -> new([[1, 3, 5],
                                 [2, 4, 6]]);
    my $y = $x -> tril(2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 3, 5],
                        [2, 4, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 3, 5],
                        [2, 4, 6]], '$x is unmodified');
}

note('tril() on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril();

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 0],
                        [2, 5],
                        [3, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}

note('tril(-3) on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril(-3);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[0, 0],
                        [0, 0],
                        [0, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}

note('tril(-2) on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril(-2);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[0, 0],
                        [0, 0],
                        [3, 0]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}

note('tril(-1) on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril(-1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[0, 0],
                        [2, 0],
                        [3, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}

note('tril(0) on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril(0);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 0],
                        [2, 5],
                        [3, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}

note('tril(1) on 3-by-2 matrix');

{
    my $x = Math::Matrix -> new([[1, 4],
                                 [2, 5],
                                 [3, 6]]);
    my $y = $x -> tril(1);

    is(ref($y), 'Math::Matrix', '$y is a Math::Matrix');
    is_deeply([ @$y ], [[1, 4],
                        [2, 5],
                        [3, 6]],
              '$y has the right values');

    # Verify that modifying $y does not modify $x.

    my ($nrowy, $ncoly) = $y -> size();
    for (my $i = 0 ; $i < $nrowy ; ++$i) {
        for (my $j = 0 ; $j < $ncoly ; ++$j) {
            $y -> [$i][$j] += 100;
        }
    }

    is_deeply([ @$x ], [[1, 4],
                        [2, 5],
                        [3, 6]], '$x is unmodified');
}
