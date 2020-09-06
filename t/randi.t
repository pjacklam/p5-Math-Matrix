#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 68;

note('Math::Matrix -> randi(7);');

{
    my $x = Math::Matrix -> randi(7);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 1, 'number of rows in $x');
    cmp_ok($ncol, '==', 1, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "0 <= \$x->[$i][$j] <= 7");
        }
    }
}

note('Math::Matrix -> randi(7, 3);');

{
    my $x = Math::Matrix -> randi(7, 3);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 3, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "0 <= \$x->[$i][$j] <= 7");
        }
    }
}

note('Math::Matrix -> randi(7, 3, 5);');

{
    my $x = Math::Matrix -> randi(7, 3, 5);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 5, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "0 <= \$x->[$i][$j] <= 7");
        }
    }
}

note('Math::Matrix -> randi([-4, 7]);');

{
    my $x = Math::Matrix -> randi([-4, 7]);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 1, 'number of rows in $x');
    cmp_ok($ncol, '==', 1, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(-4 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "-4 <= \$x->[$i][$j] <= 7");
        }
    }
}

note('Math::Matrix -> randi([-4, 7], 3);');

{
    my $x = Math::Matrix -> randi([-4, 7], 3);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 3, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(-4 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "-4 <= \$x->[$i][$j] <= 7");
        }
    }
}

note('Math::Matrix -> randi([-4, 7], 3, 5);');

{
    my $x = Math::Matrix -> randi([-4, 7], 3, 5);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 5, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(-4 <= $x->[$i][$j] && $x->[$i][$j] <= 7, "-4 <= \$x->[$i][$j] <= 7");
        }
    }
}
