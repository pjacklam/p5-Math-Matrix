#                              -*- Mode: Perl -*-
# Matrix.pm --
# ITIID           : $ITI$ $Header $__Header$
# Author          : Ulrich Pfeifer
# Created On      : Tue Oct 24 18:34:08 1995
# Last Modified By: Ulrich Pfeifer
# Last Modified On: Sat May 18 22:01:31 2013
# Language        : Perl
# Update Count    : 208
# Status          : Unknown, Use with caution!
#
# Copyright (C) 2013, John M. Gamble <jgamble@ripco.com>, all rights reserved.
# Copyright (C) 2009, oshalla https://rt.cpan.org/Public/Bug/Display.html?id=42919
# Copyright (C) 2002, Bill Denney <gte273i@prism.gatech.edu>, all rights reserved.
# Copyright (C) 2001, Brian J. Watson <bjbrew@power.net>, all rights reserved.
# Copyright (C) 2001, Ulrich Pfeifer <pfeifer@wait.de>, all rights reserved.
# Copyright (C) 1995, Universität Dortmund, all rights reserved.
# Copyright (C) 2001, Matthew Brett <matthew.brett@mrc-cbu.cam.ac.uk>
#
# Permission to use this software is granted under the same
# restrictions as for Perl itself.
#
# Revision 0.8  2013/09/30 09:21
# Add support for unary minus ([rt.cpan.org #88821] support for overload of unary minus, Diab Jerius)
#
# Revision 0.7  2013/05/18 08:15
# Replaced transpose functions (https://rt.cpan.org/Public/Bug/Display.html?id=42919)
#
# Revision 0.6  2013/05/17 10:24:40
# John M. Gamble added diagonal() and tridiagonal() methods
#
# Revision 0.5  2002/06/02 15:47:40
# Bill Denney added pinvert function
#
# Revision 0.3  2001/04/17 11:10:15
# Extensions from Brian Watson
#
# Revision 0.2  1996/07/10 17:48:14  pfeifer
# Fixes from Mike Beachy <beachy@chem.columbia.edu>
#
# Revision 0.1  1995/10/25  09:48:39  pfeifer
# Initial revision
#

=pod

=head1 NAME

Math::Matrix - Multiply and invert Matrices

=head1 SYNOPSIS

use Math::Matrix;

=head1 DESCRIPTION

The following methods are available:

=head2 new

Constructor arguments are a list of references to arrays of the same length.
The arrays are copied. The method returns B<undef> in case of error.

    $a = Math::Matrix->new([rand,rand,rand],
                           [rand,rand,rand],
                           [rand,rand,rand]);

If you call C<new> with no input arguments, a zero filled matrix with identical
dimensions is returned:

    $b = $a->new();     # $b is a zero matrix with the size of $a

=head2 new_identity

Returns a new identity matrix.

    $a = Math::Matrix -> new(3);        # $a is a 3-by-3 identity matrix

=head2 eye

This is an alias for C<new_identity>.

=head2 clone

Clones a matrix and returns the clone.

    $b = $a->clone;

=head2 diagonal

A constructor method that creates a diagonal matrix from a single list or array
of numbers.

    $p = Math::Matrix->diagonal(1, 4, 4, 8);
    $q = Math::Matrix->diagonal([1, 4, 4, 8]);

The matrix is zero filled except for the diagonal members, which take the
values of the vector.

The method returns B<undef> in case of error.

=head2 tridiagonal

A constructor method that creates a matrix from vectors of numbers.

    $p = Math::Matrix->tridiagonal([1, 4, 4, 8]);
    $q = Math::Matrix->tridiagonal([1, 4, 4, 8], [9, 12, 15]);
    $r = Math::Matrix->tridiagonal([1, 4, 4, 8], [9, 12, 15], [4, 3, 2]);

In the first case, the main diagonal takes the values of the vector, while both
of the upper and lower diagonals's values are all set to one.

In the second case, the main diagonal takes the values of the first vector,
while the upper and lower diagonals are each set to the values of the second
vector.

In the third case, the main diagonal takes the values of the first vector,
while the upper diagonal is set to the values of the second vector, and the
lower diagonal is set to the values of the third vector.

The method returns B<undef> in case of error.

=head2 size

You can determine the dimensions of a matrix by calling:

    ($m, $n) = $a->size;

=head2 concat

Concatenate matrices horizontally. The matrices must have the same number or
rows. The result is a new matrix or B<undef> in case of error.

    $x = Math::Matrix -> new([1, 2], [4, 5]);   # 2-by-2 matrix
    $y = Math::Matrix -> new([3], [6]);         # 2-by-1 matrix
    $z = $x -> concat($y);                      # 2-by-3 matrix

=head2 transpose

Returns the transposed matrix. This is the matrix where colums and rows of the
argument matrix are swaped.

=head2 negative

Negate a matrix and return it.

    $a = Math::Matrix -> new([-2, 3]);
    $b = $a -> negative();                  # $b = [[2, -3]]

=head2 multiply

Multiplies two matrices where the length of the rows in the first matrix is the
same as the length of the columns in the second matrix. Returns the product or
B<undef> in case of error.

=head2 solve

Solves a equation system given by the matrix. The number of colums must be
greater than the number of rows. If variables are dependent from each other,
the second and all further of the dependent coefficients are 0. This means the
method can handle such systems. The method returns a matrix containing the
solutions in its columns or B<undef> in case of error.

=head2 invert

Invert a Matrix using C<solve>.

=head2 multiply_scalar

Multiplies a matrix and a scalar resulting in a matrix of the same dimensions
with each element scaled with the scalar.

    $a->multiply_scalar(2);  scale matrix by factor 2

=head2 add

Add two matrices of the same dimensions.

=head2 subtract

Shorthand for C<add($other-E<gt>negative)>

=head2 equal


Decide if two matrices are equal. The criterion is, that each pair of elements
differs less than $Math::Matrix::eps.

=head2 slice

Extract columns:

    a->slice(1,3,5);

=head2 diagonal_vector

Extract the diagonal as an array:

    $diag = $a->diagonal_vector;

=head2 tridiagonal_vector

Extract the diagonals that make up a tridiagonal matrix:

    ($main_d, $upper_d, $lower_d) = $a->tridiagonal_vector;

=head2 determinant

Compute the determinant of a matrix.

    $a = Math::Matrix->new([3, 1],
                           [4, 2]);
    $d = $a->determinant;                   # $d = 2

=head2 dot_product

Compute the dot product of two vectors. The second operand does not have to be
an object.

    # $x and $y are both objects
    $x = Math::Matrix -> new([1, 2, 3]);
    $y = Math::Matrix -> new([4, 5, 6]);
    $p = $x -> dot_product($y);             # $p = 32

    # Only $x is an object.
    $p = $x -> dot_product([4, 5, 6]);      # $p = 32

=head2 absolute

Compute the absolute value (i.e., length) of a vector.

    $v = Math::Matrix -> new([3, 4]);
    $a = $v -> absolute();                  # $v = 5

=head2 normalize

Normalize a vector, i.e., scale a vector so its length becomes 1.

    $v = Math::Matrix -> new([3, 4]);
    $u = $v -> normalize();                 # $u = [ 0.6, 0.8 ]

=head2 cross_product

Compute the cross-product of vectors.

    $x = Math::Matrix -> new([1,3,2],
                             [5,4,2]);
    $p = $x -> cross_product();             # $p = [ -2, 8, -11 ]

=head2 as_string

Creates a string representation of the matrix and returns it.

    $x = Math::Matrix -> new([1, 2], [3, 4]);
    $s = $x -> as_string();

=head2 print

Prints the matrix on STDOUT. If the method has additional parameters, these are
printed before the matrix is printed.

=head2 pinvert

Compute the pseudo-inverse of the matrix: ((A'A)^-1)A'

=head1 OVERLOADING

The following operators are overloaded.

=over

=item C<+> and C<+=>

Matrix addition. The two operands must have the same size.

    $C  = $A + $B;      # assign $A + $B to $C
    $A += $B;           # assign $A + $B to $A

=item C<-> and C<-=>

Matrix subtraction. The two operands must have the same size.

    $C  = $A + $B;      # assign $A - $B to $C
    $A += $B;           # assign $A - $B to $A

=item C<*> and C<*=>

Matrix multiplication. The number of columns in the first operand must be equal
to the number of rows in the second operand.

    $C  = $A * $B;      # assign $A * $B to $C
    $A *= $B;           # assign $A * $B to $A

=item C<~>

Transpose.

    $B = ~$A;           # $B is the transpose of $A

=back

=head1 EXAMPLE

    use Math::Matrix;

    srand(time);
    $a = new Math::Matrix ([rand,rand,rand],
                           [rand,rand,rand],
                           [rand,rand,rand]);
    $x = new Math::Matrix ([rand,rand,rand]);
    $a->print("A\n");
    $E = $a->concat($x->transpose);
    $E->print("Equation system\n");
    $s = $E->solve;
    $s->print("Solutions s\n");
    $a->multiply($s)->print("A*s\n");

=head1 AUTHOR

Ulrich Pfeifer E<lt>F<pfeifer@ls6.informatik.uni-dortmund.de>E<gt>

Brian J. Watson E<lt>F<bjbrew@power.net>E<gt>

Matthew Brett E<lt>matthew.brett@mrc-cbu.cam.ac.ukE<gt>

=cut

package Math::Matrix;
use vars qw($VERSION $eps);
use strict;
use Carp;

$VERSION = 0.9;

use overload
  '+'  => 'add',
  '-'  => 'subtract',
  '*'  => 'multiply',
  '~'  => 'transpose',
  '""' => 'as_string',
  '='  => 'clone';

sub version {
    return "Math::Matrix $VERSION";
}

# Implement - array copy, inheritance

sub new {
    my $that = shift;
    my $class = ref($that) || $that;
    my $self = [];

    # If called as an instance method and no arguments are given, return a
    # zero matrix of the same size as the invocand.

    if (ref($that) && (@_ == 0)) {
        for (@$that) {
            push(@{$self}, [map {0} @{$_}]);
        }
    }

    # Otherwise return a new matrix based on the input arguments.

    else {
        my $len = scalar(@{$_[0]});
        for (@_) {
            return undef if scalar(@{$_}) != $len;
            push(@{$self}, [@{$_}]);
        }
    }

    bless $self, $class;
}

sub clone {
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $that = shift;
    my $self = [];

    for (@$that) {
        push(@{$self}, [@{$_}]);
    }
    bless $self, ref($that)||$that;
}

#
# Either class or object call, create a square matrix with the same
# dimensions as the passed-in list or array.
#
sub diagonal {
    my $that = shift;
    my $class = ref($that) || $that;
    my @diag = @_;
    my $self = [];

    # diagonal([2,3]) -> diagonal(2,3)
    @diag = @{$diag[0]} if (ref $diag[0] eq "ARRAY");

    my $len = scalar @diag;
    return undef if ($len == 0);

    for my $idx (0..$len-1) {
        my @r = (0) x $len;
        $r[$idx] = $diag[$idx];
        push(@{$self}, [@r]);
    }
    bless $self, $class;
}

#
# Either class or object call, create a square matrix with the same
# dimensions as the passed-in list or array.
#
sub tridiagonal {
    my $that = shift;
    my $class = ref($that) || $that;
    my(@up_d, @main_d, @low_d);
    my $self = [];

    #
    # Handle the different ways the tridiagonal vectors could
    # be passed in.
    #
    if (ref $_[0] eq "ARRAY") {
        @main_d = @{$_[0]};

        if (ref $_[1] eq "ARRAY") {
            @up_d = @{$_[1]};

            if (ref $_[2] eq "ARRAY") {
                @low_d = @{$_[2]};
            }
        }
    } else {
        @main_d = @_;
    }

    my $len = scalar @main_d;
    return undef if ($len == 0);

    #
    # Default the upper and lower diagonals if no vector
    # was passed in for them.
    #
    @up_d = (1) x ($len -1) if (scalar @up_d == 0);
    @low_d = @up_d if (scalar @low_d == 0);

    #
    # First row...
    #
    my @arow = (0) x $len;
    @arow[0..1] = ($main_d[0], $up_d[0]);
    push (@{$self}, [@arow]);

    #
    # Bulk of the matrix...
    #
    for my $idx (1 .. $#main_d - 1) {
        my @r = (0) x $len;
        @r[$idx-1 .. $idx+1] = ($low_d[$idx-1], $main_d[$idx], $up_d[$idx]);
        push (@{$self}, [@r]);
    }

    #
    # Last row.
    #
    my @zrow = (0) x $len;
    @zrow[$len-2..$len-1] = ($low_d[$#main_d -1], $main_d[$#main_d]);
    push (@{$self}, [@zrow]);

    bless $self, $class;
}

sub diagonal_vector {
    my $self = shift;
    my @diag;
    my $idx = 0;
    my($m, $n) = $self->size();

    die "Not a square matrix" if ($m != $n);

    foreach my $r (@{$self}) {
        push @diag, $r->[$idx++];
    }
    return \@diag;
}

sub tridiagonal_vector {
    my $self = shift;
    my(@main_d, @up_d, @low_d);
    my($m, $n) = $self->size();
    my $idx = 0;

    die "Not a square matrix" if ($m != $n);

    foreach my $r (@{$self}) {
        push @low_d, $r->[$idx - 1] if ($idx > 0);
        push @main_d, $r->[$idx++];
        push @up_d, $r->[$idx] if ($idx < $m);
    }
    return ([@main_d],[@up_d],[@low_d]);
}

sub size {
    my $self = shift;
    my $m = @{$self};
    my $n = @{$self->[0]};
    ($m, $n);
}

sub concat {
    my $self   = shift;
    my $other  = shift;
    my $result =  $self->clone();

    return undef if scalar(@{$self}) != scalar(@{$other});
    for my $i (0 .. $#{$self}) {
        push @{$result->[$i]}, @{$other->[$i]};
    }
    $result;
}

sub transpose {
    my ($matrix) = shift ;
    my @result = () ;
    my $lc = $#{$matrix->[0]};
    for my $col (0..$lc) {
        push @result, [map $_->[$col], @$matrix];
    }
    return( bless \@result, ref $matrix );
}

sub vekpro {
    my($a, $b) = @_;
    my $result=0;

    for my $i (0 .. $#{$a}) {
        $result += $a->[$i] * $b->[$i];
    }
    $result;
}

sub multiply {
    my $self  = shift;
    my $class = ref($self);
    my $other = shift->transpose;
    my @result;
    my $m;

    return undef if $#{$self->[0]} != $#{$other->[0]};
    for my $row (@{$self}) {
        my $rescol = [];
        for my $col (@{$other}) {
            push(@{$rescol}, vekpro($row,$col));
        }
        push(@result, $rescol);
    }
    $class->new(@result);
}

$eps = 0.00001;

sub solve {
    my $self  = shift;
    my $class = ref($self);

    my $m    = $self->clone();
    my $mr   = $#{$m};
    my $mc   = $#{$m->[0]};
    my $f;
    my $try;

    return undef if $mc <= $mr;
  ROW: for(my $i = 0; $i <= $mr; $i++) {
        $try=$i;
        # make diagonal element nonzero if possible
        while (abs($m->[$i]->[$i]) < $eps) {
            last ROW if $try++ > $mr;
            my $row = splice(@{$m},$i,1);
            push(@{$m}, $row);
        }

        # normalize row
        $f = $m->[$i]->[$i];
        for (my $k = 0; $k <= $mc; $k++) {
            $m->[$i]->[$k] /= $f;
        }
        # subtract multiple of designated row from other rows
        for (my $j = 0; $j <= $mr; $j++) {
            next if $i == $j;
            $f = $m->[$j]->[$i];
            for (my $k = 0; $k <= $mc; $k++) {
                $m->[$j]->[$k] -= $m->[$i]->[$k] * $f;
            }
        }
    }
    # Answer is in augmented column
    transpose $class->new(@{$m->transpose}[$mr+1 .. $mc]);
}

sub pinvert {
    my $self  = shift;
    my $class = ref($self);

    my $m    = $self->clone();

    $m->transpose->multiply($m)->invert->multiply($m->transpose);
}

sub print {
    my $self = shift;

    print @_ if scalar(@_);
    print $self->as_string;
}

sub as_string {
    my $self = shift;
    my $out = "";
    for my $row (@{$self}) {
        for my $col (@{$row}) {
            $out = $out . sprintf "%10.5f ", $col;
        }
        $out = $out . sprintf "\n";
    }
    $out;
}

sub new_identity {
    my $type = shift;
    my $class = ref($type) || $type;
    my $self = [];
    my $size = shift;

    for my $i (1..$size) {
        my $row = [];
        for my $j (1..$size) {
            push @$row, $i==$j ? 1 : 0;
        }
        push @$self, $row;
    }
    bless $self, $class;
}

sub eye {
    &new_identity(@_);
}

sub multiply_scalar {
    my $self = shift;
    my $factor = shift;
    my $result = $self->new();

    my $last = $#{$self->[0]};
    for my $i (0 .. $#{$self}) {
        for my $j (0 .. $last) {
            $result->[$i][$j] = $factor * $self->[$i][$j];
        }
    }
    $result;
}

sub negative {
    shift->multiply_scalar(-1);
}

sub subtract {
    my $self = shift;
    my $other = shift;

    # if $swap is present, $other operand isn't a Math::Matrix.  in
    # general that's undefined, but, if called as
    #   subtract($self,0,1)
    # we've been called as unary minus, which is defined.
    if ( @_  && $_[0] && ! ref $other && $other == 0 ) {
        $self->negative;
    } else {
        $self->add($other->negative);
    }
}

sub equal {
    my $A = shift;
    my $B = shift;
    my $ok = 1;

    my $last = $#{$A->[0]};
    for my $i (0 .. $#{$A}) {
        for my $j (0 .. $last) {
            abs($A->[$i][$j]-$B->[$i][$j])<$eps or $ok=0;
        }
    }
    $ok;
}

sub add {
    my $self = shift;
    my $other = shift;
    my $result = $self->new();

    return undef
      if $#{$self} != $#{$other};

    my $last= $#{$self->[0]};
    return undef
      if $last != $#{$other->[0]};
    for my $i (0 .. $#{$self}) {
        for my $j (0 .. $last) {
            $result->[$i][$j] = $self->[$i][$j] + $other->[$i][$j];
        }
    }
    $result;
}

sub slice {
    my $self = shift;
    my $class = ref($self);
    my $result = [];

    for my $i (0 .. $#$self) {
        push @$result, [ @{$self->[$i]}[@_] ];
    }

    bless $result, $class;
    $result -> clone();
}

sub determinant {
    my $self = shift;
    my $class = ref($self);
    my $last= $#{$self->[0]};

    return undef
      unless $last == $#{$self};

    if ($last == 0) {
        return $self->[0][0];
    } else {
        my $result = 0;
        foreach my $col (0..$last) {
            my $matrix = $self->slice(0..$col-1,$col+1..$last);
            $matrix = $class->new(@$matrix[1..$last]);
            my $term += $matrix->determinant();
            $term *= $self->[0][$col];
            $term *= $col % 2 ? -1 : 1;
            $result += $term;
        }
        return $result;
    }
}

#
# For vectors only
#

sub dot_product {
    my $vector1 = shift;
    my $class = ref $vector1;

    my $vector2 = shift;

    # Allow the input to be an ordinary array, i.e., not an object. Ideally, we
    # should use the following test, but that requires the Scalar::Util module,
    # which might not be installed.
    #
    #   $vector2 = $class -> new($vector2)
    #     unless blessed($vector2) && $vector2 -> isa($class);

    $vector2 = $class -> new($vector2)
      if ref($vector2) eq 'ARRAY';

    $vector1 = $vector1->transpose()
      unless @$vector1 == 1;
    return undef
      unless @$vector1 == 1;

    $vector2 = $vector2->transpose()
      unless @{$vector2->[0]} == 1;
    return undef
      unless @{$vector2->[0]} == 1;

    return $vector1->multiply($vector2)->[0][0];
}

sub absolute {
    my $vector = shift;
    sqrt $vector->dot_product($vector);
}

sub normalize {
    my $vector = shift;
    my $length = $vector->absolute();
    return undef
      unless $length;
    $vector->multiply_scalar(1 / $length);
}

sub cross_product {
    my $vectors = shift;
    my $class = ref($vectors);

    my $dimensions = @{$vectors->[0]};
    return undef
      unless $dimensions == @$vectors + 1;

    my @axis;
    foreach my $column (0..$dimensions-1) {
        my $tmp = $vectors->slice(0..$column-1,
                                  $column+1..$dimensions-1);
        my $scalar = $tmp->determinant;
        $scalar *= ($column % 2) ? -1 : 1;
        push @axis, $scalar;
    }
    my $axis = $class->new(\@axis);
    $axis = $axis->multiply_scalar(($dimensions % 2) ? 1 : -1);
}

sub invert {
    my $M = shift;
    my ($m, $n) = $M->size;
    my (@I);
    die "Matrix dimensions are $m X $n. -- Matrix not invertible.\n"
      if $m != $n;
    my $I = $M->new_identity($n);
    ($M->concat($I))->solve;
}

1;
