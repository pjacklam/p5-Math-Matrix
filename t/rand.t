#!perl

use strict;
use warnings;

use Math::Matrix;
use Test::More tests => 39;

note('Math::Matrix -> rand(2, 3);');

{
    my $x = Math::Matrix -> rand(2, 3);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 2, 'number of rows in $x');
    cmp_ok($ncol, '==', 3, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] < 1, "0 <= \$x->[$i][$j] < 1");
        }
    }
}

note('Math::Matrix -> rand(1, 3);');

{
    my $x = Math::Matrix -> rand(1, 3);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 1, 'number of rows in $x');
    cmp_ok($ncol, '==', 3, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] < 1, "0 <= \$x->[$i][$j] < 1");
        }
    }
}

note('Math::Matrix -> rand(3, 1);');

{
    my $x = Math::Matrix -> rand(3, 1);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 1, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] < 1, "0 <= \$x->[$i][$j] < 1");
        }
    }
}

note('Math::Matrix -> rand(3);');

{
    my $x = Math::Matrix -> rand(3);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 3, 'number of rows in $x');
    cmp_ok($ncol, '==', 3, 'number of columns in $x');
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            ok(0 <= $x->[$i][$j] && $x->[$i][$j] < 1, "0 <= \$x->[$i][$j] < 1");
        }
    }
}

note('Math::Matrix -> rand(0);');

{
    my $x = Math::Matrix -> rand(0);
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 0, 'number of rows in $x');
    cmp_ok($ncol, '==', 0, 'number of columns in $x');
}

note('Math::Matrix -> rand();');

{
    my $x = Math::Matrix -> rand();
    is(ref($x), 'Math::Matrix', '$x is a Math::Matrix');
    my ($nrow, $ncol) = $x -> size();
    cmp_ok($nrow, '==', 1, 'number of rows in $x');
    cmp_ok($ncol, '==', 1, 'number of columns in $x');
}
