# -*- mode: perl; coding: utf-8-unix -*-

package Math::Matrix;

use strict;
use warnings;

use Carp;

our $VERSION = '0.91';
our $eps = 0.00001;

use overload
  '+'  => 'add',
  '-'  => 'subtract',
  '*'  => 'multiply',
  '~'  => 'transpose',
  '""' => 'as_string',
  '='  => 'clone';

=pod

=encoding utf8

=head1 NAME

Math::Matrix - multiply and invert matrices

=head1 SYNOPSIS

    use Math::Matrix;

    # Generate a random 3-by-3 matrix.
    srand(time);
    my $A = Math::Matrix -> new([rand, rand, rand],
                                [rand, rand, rand],
                                [rand, rand, rand]);
    $A -> print("A\n");

    # Append a fourth column to $A.
    my $x = Math::Matrix -> new([rand, rand, rand]);
    my $E = $A -> concat($x -> transpose);
    $E -> print("Equation system\n");

    # Compute the solution.
    my $s = $E -> solve;
    $s -> print("Solutions s\n");

    # Verify that the solution equals $x.
    $A -> multiply($s) -> print("A*s\n");

=head1 DESCRIPTION

This module implements various constructors and methods for creating and
manipulating matrices.

All methods return new objects, so, for example, C<$X-E<gt>add($Y)> does not
modify C<$X>.

    $X -> add($Y);         # $X not modified; output is lost
    $X = $X -> add($Y);    # this works

Some operators are overloaded (see L</OVERLOADING>) and allow the operand to be
modified directly.

    $X = $X + $Y;          # this works
    $X += $Y;              # so does this

=head1 METHODS

=head2 Constructors

=over

=item new()

Creates a new object from the input arguments and returns it.

If a single input argument is given, and that argument is a reference to array
whose first element is itself a reference to an array, it is assumed that the
argument contains the whole matrix, like this:

    $x = Math::Matrix->new([[1, 2, 3], [4, 5, 6]]); # 2-by-3 matrix
    $x = Math::Matrix->new([[1, 2, 3]]);            # 1-by-3 matrix
    $x = Math::Matrix->new([[1], [2], [3]]);        # 3-by-1 matrix

If a single input argument is given, and that argument is not a reference to an
array, a 1-by-1 matrix is returned.

    $x = Math::Matrix->new(1);                      # 1-by-1 matrix

Otherwise it is assumed that each input argument is a row, like this:

    $x = Math::Matrix->new([1, 2, 3], [4, 5, 6]);   # 2-by-3 matrix
    $x = Math::Matrix->new([1, 2, 3]);              # 1-by-3 matrix
    $x = Math::Matrix->new([1], [2], [3]);          # 3-by-1 matrix

Note that all the folling cases result in an empty matrix:

    $x = Math::Matrix->new([[], [], []]);
    $x = Math::Matrix->new([[]]);
    $x = Math::Matrix->new([]);

If C<new> is called as an instance method with no input arguments, a zero
filled matrix with identical dimensions is returned:

    $b = $a->new();     # $b is a zero matrix with the size of $a

Each row must contain the same number of elements.

In case of an erry, B<undef> is returned.

=cut

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

    # Otherwise return a new matrix based on the input arguments. The object
    # data is a blessed reference to an array containing the matrix data. This
    # array contains a list of arrays, one for each row, which in turn contains
    # a list of elements. An empty matrix has no rows.
    #
    #   [[ 1, 2, 3 ], [ 4, 5, 6 ]]  2-by-3 matrix
    #   [[ 1, 2, 3 ]]               1-by-3 matrix
    #   [[ 1 ], [ 2 ], [ 3 ]]       3-by-1 matrix
    #   [[ 1 ]]                     1-by-1 matrix
    #   []                          empty matrix

    else {

        my $data;

        # If there is a single argument, and that is not a reference,
        # assume new() has been called as, e.g., $class -> new(3).

        if (@_ == 1 && !ref($_[0])) {
            $data = [[ $_[0] ]];
        }

        # If there is a single argument, and that is a reference to an array,
        # and that array contains at least one element, and that element is
        # itself a reference to an array, then assume new() has been called
        # with the matrix as one argument, i.e., a reference to an array of
        # arrays, e.g., $class -> new([ [1, 2], [3, 4] ]) ...

        elsif (@_ == 1 && ref($_[0]) eq 'ARRAY'
               && @{$_[0]} > 0 && ref($_[0][0]) eq 'ARRAY')
        {
            $data = $_[0];
        }

        # ... otherwise assume that each argument to new() is a row. Note that
        # new() called with no arguments results in an empty matrix.

        else {
            $data = [ @_ ];
        }

        # Sanity checking.

        if (@$data) {
            my $nrow = @$data;
            my $ncol;

            for (my $i = 0 ; $i < $nrow ; ++$i) {
                my $row = $data -> [$i];

                # Verify that the row is a reference to an array.

                croak "row with index $i is not a reference to an array"
                  unless ref($row) eq 'ARRAY';

                # In the first round, get the number of elements, i.e., the
                # number of columns in the matrix. In the successive
                # rounds, verify that each row has the same number of
                # elements.

                if ($i == 0) {
                    $ncol = @$row;
                } else {
                    croak "each row must have the same number of elements"
                      unless @$row == $ncol;
                }
            }

            # Copy the data into $self only if the matrix is non-emtpy.

            @$self = map { [ @$_ ] } @$data if $ncol;
        }
    }

    bless $self, $class;
}

=pod

=item new_identity()

Returns a new identity matrix.

    $a = Math::Matrix -> new(3);        # $a is a 3-by-3 identity matrix

=cut

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

=pod

=item eye()

This is an alias for C<new_identity>.

=cut

sub eye {
    &new_identity(@_);
}

=pod

=item zeros()

Create a zero matrix.

    # Create an $m-by-1 matrix where each element is 0.
    $x = Math::Matrix -> zeros($m);

    # Create an $m-by-$n matrix where each element is 0.
    $x = Math::Matrix -> zeros($m, $n);

=cut

sub zeros {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 3;
    my $self = shift;
    $self -> constant(0, @_);
};

=pod

=item ones()

Create a matrix of ones.

    # Create an $m-by-1 matrix where each element is 1.
    $x = Math::Matrix -> ones($m);

    # Create an $m-by-$n matrix where each element is 1.
    $x = Math::Matrix -> ones($m, $n);

=cut

sub ones {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 3;
    my $self = shift;
    $self -> constant(1, @_);
};

=pod

=item constant()

Returns a constant matrix, i.e., a matrix whose elements all have the same
value.

    # Create an $m-by-1 matrix where each element is $c.
    $x = Math::Matrix -> constant($c, $m);

    # Create an $m-by-$n matrix where each element is $c.
    $x = Math::Matrix -> constant($c, $m, $n);

=cut

sub constant {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 2;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 4;
    my $class = shift;

    croak +(caller(0))[3], " is a class method, not an instance method"
      if ref $class;

    my $c = shift;
    my ($nrow, $ncol) = @_ == 0 ? (0, 0)
                      : @_ == 1 ? (@_, 1)
                      :           (@_);

    my $x = bless [], $class;
    for (my $i = 0 ; $i < $nrow ; ++ $i) {
        for (my $j = 0 ; $j < $ncol ; ++ $j) {
            $x -> [$i][$j] = $c;
        }
    }

    return $x;
}

=pod

=item clone()

Clones a matrix and returns the clone.

    $b = $a->clone;

=cut

sub clone {
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    croak +(caller(0))[3], " is an instance method, not a class method"
      unless $class;

    my $y = [ map { [ @$_ ] } @$x ];
    bless $y, $class;
}

=pod

=item diagonal()

A constructor method that creates a diagonal matrix from a single list or array
of numbers.

    $p = Math::Matrix->diagonal(1, 4, 4, 8);
    $q = Math::Matrix->diagonal([1, 4, 4, 8]);

The matrix is zero filled except for the diagonal members, which take the
values of the vector.

The method returns B<undef> in case of error.

=cut

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

=pod

=item tridiagonal()

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

=cut

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

=pod

=back

=head2 Methods for identifying matrices

=over

=item is_empty()

Returns 1 is the invocand is empty, i.e., it has no elements.

    $bool = $x -> is_empty();

=cut

sub is_empty {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> nelm() == 0;
}

=pod

=item is_scalar()

Returns 1 is the invocand is a scalar, i.e., it has one element.

    $bool = $x -> is_scalar();

=cut

sub is_scalar {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> nelm() == 1 ? 1 : 0;
}

=pod

=item is_vector()

Returns 1 is the invocand is a vector, i.e., a row vector or a column vector.

    $bool = $x -> is_vector();

=cut

sub is_vector {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> is_col() || $x -> is_row() ? 1 : 0;
}

=pod

=item is_row()

Returns 1 if the invocand has exactly one row, and 0 otherwise.

    $bool = $x -> is_row();

=cut

sub is_row {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> nrow() == 1 ? 1 : 0;
}

=pod

=item is_col()

Returns 1 if the invocand has exactly one column, and 0 otherwise.

    $bool = $x -> is_col();

=cut

sub is_col {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> ncol() == 1 ? 1 : 0;
}

=pod

=item is_square()

Returns 1 is the invocand is square, and 0 otherwise.

    $bool = $x -> is_square();

=cut

sub is_square {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my ($nrow, $ncol) = $x -> size();
    return $nrow == $ncol ? 1 : 0;
}

=pod

=item is_symmetric()

Returns 1 is the invocand is symmetric, and 0 otherwise.

    $bool = $x -> is_symmetric();

An symmetric matrix satisfies x(i,j) = x(j,i) for all i and j, for example

    [  1  2 -3 ]
    [  2 -4  5 ]
    [ -3  5  6 ]

=cut

sub is_symmetric {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my ($nrow, $ncol) = $x -> size();
    return 0 unless $nrow == $ncol;

    for (my $i = 1 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $i ; ++$j) {
            return 0 unless $x -> [$i][$j] == $x -> [$j][$i];
        }
    }

    return 1;
}

=pod

=item is_antisymmetric()

Returns 1 is the invocand is antisymmetric a.k.a. skew-symmetric, and 0
otherwise.

    $bool = $x -> is_antisymmetric();

An antisymmetric matrix satisfies x(i,j) = -x(j,i) for all i and j, for
example

    [  0  2 -3 ]
    [ -2  0  4 ]
    [  3 -4  0 ]

=cut

sub is_antisymmetric {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my ($nrow, $ncol) = $x -> size();
    return 0 unless $nrow == $ncol;

    # Check the diagonal.

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        return 0 unless $x -> [$i][$i] == 0;
    }

    # Check the off-diagonal.

    for (my $i = 1 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $i ; ++$j) {
            return 0 unless $x -> [$i][$j] == -$x -> [$j][$i];
        }
    }

    return 1;
}

=pod

=item is_zero()

Returns 1 is the invocand is a zero matrix, and 0 otherwise. A zero matrix
contains no element whose value is different from zero.

    $bool = $x -> is_zero();

=cut

sub is_zero {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> is_constant(0);
}

=pod

=item is_one()

Returns 1 is the invocand is a matrix of ones, and 0 otherwise. A matrix of
ones contains no element whose value is different from one.

    $bool = $x -> is_one();

=cut

sub is_one {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> is_constant(1);
}

=pod

=item is_constant()

Returns 1 is the invocand is a constant matrix, and 0 otherwise. A constant
matrix is a matrix where no two elements have different values.

    $bool = $x -> is_constant();

=cut

sub is_constant {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 2;
    my $x = shift;
    my $class = ref $x;

    my ($nrow, $ncol) = $x -> size();

    # An empty matrix contains no elements that are different from $c.

    return 1 if $nrow * $ncol == 0;

    my $c = @_ ? shift() : $x -> [0][0];
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            return 0 if $x -> [$i][$j] != $c;
        }
    }

    return 1;
}

=pod

=item is_identity()

Returns 1 is the invocand is an identity matrix, and 0 otherwise. An
identity matrix contains ones on the main diagonal and zeros elsewhere.

    $bool = $x -> is_identity();

=cut

sub is_identity {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my ($nrow, $ncol) = $x -> size();
    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            return 0 if $x -> [$i][$j] != ($i == $j ? 1 : 0);
        }
    }

    return 1;
}

=pod

=item is_exchg()

Returns 1 is the invocand is an exchange matrix, and 0 otherwise.

    $bool = $x -> is_exchg();

An exchange matrix contains ones on the main anti-diagonal and zeros elsewhere,
for example

    [ 0 0 1 ]
    [ 0 1 0 ]
    [ 1 0 0 ]

=cut

sub is_exchg {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my ($nrow, $ncol) = $x -> size();
    return 0 unless $nrow == $ncol;

    my $imax = $nrow - 1;
    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            return 0 if $x -> [$i][$j] != ($i + $j == $imax ? 1 : 0);
        }
    }

    return 1;
}

=pod

=item is_bool()

Returns 1 is the invocand is a boolean matrix, and 0 otherwise.

    $bool = $x -> is_bool();

A boolean matrix is a matrix is a matrix whose entries are either 0 or 1, for
example

    [ 0 1 1 ]
    [ 1 0 0 ]
    [ 0 1 0 ]

=cut

sub is_bool {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my ($nrow, $ncol) = $x -> size();

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            my $val = $x -> [$i][$j];
            return 0 if $val != 0 && $val != 1;
        }
    }

    return 1;
}

=pod

=item is_perm()

Returns 1 is the invocand is an permutation matrix, and 0 otherwise.

    $bool = $x -> is_perm();

A permutation matrix is a square matrix with exactly one 1 in each row and
column, and all other elements 0, for example

    [ 0 1 0 ]
    [ 1 0 0 ]
    [ 0 0 1 ]

=cut

sub is_perm {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my ($nrow, $ncol) = $x -> size();
    return 0 unless $nrow == $ncol;

    my $rowsum = [ (0) x $nrow ];
    my $colsum = [ (0) x $ncol ];

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            my $val = $x -> [$i][$j];
            return 0 if $val != 0 && $val != 1;
            if ($val == 1) {
                return 0 if ++$rowsum -> [$i] > 1;
                return 0 if ++$colsum -> [$j] > 1;
            }
        }
    }

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        return 0 if $rowsum -> [$i] != 1;
        return 0 if $colsum -> [$i] != 1;
    }

    return 1;
}

=pod

=item is_diag()

Returns 1 is the invocand is diagonal, and 0 otherwise.

    $bool = $x -> is_diag();

A diagonal matrix is a square matrix where all non-zero elements, if any, are
on the main diagonal. It has the following pattern, where only the elements
marked as C<x> can be non-zero,

    [ x 0 0 0 0 ]
    [ 0 x 0 0 0 ]
    [ 0 0 x 0 0 ]
    [ 0 0 0 x 0 ]
    [ 0 0 0 0 x ]

=cut

sub is_diag {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            next if $i == $j;
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_adiag()

Returns 1 is the invocand is anti-diagonal, and 0 otherwise.

    $bool = $x -> is_adiag();

A diagonal matrix is a square matrix where all non-zero elements, if any, are
on the main antidiagonal. It has the following pattern, where only the elements
marked as C<x> can be non-zero,

    [ 0 0 0 0 x ]
    [ 0 0 0 x 0 ]
    [ 0 0 x 0 0 ]
    [ 0 x 0 0 0 ]
    [ x 0 0 0 0 ]

=cut

sub is_adiag {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            next if $i + $j + 1 == $nrow;
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_tridiag()

Returns 1 is the invocand is tridiagonal, and 0 otherwise.

    $bool = $x -> is_tridiag();

A tridiagonal matrix is a square matrix with nonzero elements only on the
diagonal and slots horizontally or vertically adjacent the diagonal (i.e.,
along the subdiagonal and superdiagonal). It has the following pattern, where
only the elements marked as C<x> can be non-zero,

    [ x x 0 0 0 ]
    [ x x x 0 0 ]
    [ 0 x x x 0 ]
    [ 0 0 x x x ]
    [ 0 0 0 x x ]

=cut

sub is_tridiag {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol ; ++$j) {
            next if $i == $j || $i == $j - 1 || $i == $j + 1;
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_atridiag()

Returns 1 is the invocand is anti-tridiagonal, and 0 otherwise.

    $bool = $x -> is_tridiag();

A anti-tridiagonal matrix is a square matrix with nonzero elements only on the
anti-diagonal and slots horizontally or vertically adjacent the diagonal (i.e.,
along the anti-subdiagonal and anti-superdiagonal). It has the following
pattern, where only the elements marked as C<x> can be non-zero,

    [ 0 0 0 x x ]
    [ 0 0 x x x ]
    [ 0 x x x 0 ]
    [ x x x 0 0 ]
    [ x x 0 0 0 ]

=cut

sub is_atridiag {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    # Check upper part.

    for (my $i = $nrow - 3 ; $i >= 0 ; --$i) {
        for (my $j = $nrow - 3 - $i ; $j >= 0 ; --$j) {
            return 0 if $x -> [$i][$j] != 0;
        }
    }

    # Check lower part.

    for (my $i = 2 ; $i < $nrow ; ++$i) {
        for (my $j = $nrow - $i + 1 ; $j < $nrow ; ++$j) {
            return 0 if $x -> [$i][$j] != 0;
        }
    }

    return 1;
}

=pod

=item is_triu()

Returns 1 is the invocand is upper triangular, and 0 otherwise.

    $bool = $x -> is_triu();

An upper triangular matrix is a square matrix where all non-zero elements are
on or above the main diagonal. It has the following pattern, where only the
elements marked as C<x> can be non-zero. It has the following pattern, where
only the elements marked as C<x> can be non-zero,

    [ x x x x ]
    [ 0 x x x ]
    [ 0 0 x x ]
    [ 0 0 0 x ]

=cut

sub is_triu {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 1 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $i ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_striu()

Returns 1 is the invocand is strictly upper triangular, and 0 otherwise.

    $bool = $x -> is_striu();

A strictly upper triangular matrix is a square matrix where all non-zero
elements are strictly above the main diagonal. It has the following pattern,
where only the elements marked as C<x> can be non-zero,

    [ 0 x x x ]
    [ 0 0 x x ]
    [ 0 0 0 x ]
    [ 0 0 0 0 ]

=cut

sub is_striu {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j <= $i ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_tril()

Returns 1 is the invocand is lower triangular, and 0 otherwise.

    $bool = $x -> is_tril();

A lower triangular matrix is a square matrix where all non-zero elements are on
or below the main diagonal. It has the following pattern, where only the
elements marked as C<x> can be non-zero,

    [ x 0 0 0 ]
    [ x x 0 0 ]
    [ x x x 0 ]
    [ x x x x ]

=cut

sub is_tril {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = $i + 1 ; $j < $ncol ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_stril()

Returns 1 is the invocand is strictly lower triangular, and 0 otherwise.

    $bool = $x -> is_stril();

A strictly lower triangular matrix is a square matrix where all non-zero
elements are strictly below the main diagonal. It has the following pattern,
where only the elements marked as C<x> can be non-zero,

    [ 0 0 0 0 ]
    [ x 0 0 0 ]
    [ x x 0 0 ]
    [ x x x 0 ]

=cut

sub is_stril {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = $i ; $j < $ncol ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_atriu()

Returns 1 is the invocand is upper anti-triangular, and 0 otherwise.

    $bool = $x -> is_atriu();

An upper anti-triangular matrix is a square matrix where all non-zero elements
are on or above the main anti-diagonal. It has the following pattern, where
only the elements marked as C<x> can be non-zero,

    [ x x x x ]
    [ x x x 0 ]
    [ x x 0 0 ]
    [ x 0 0 0 ]

=cut

sub is_atriu {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 1 ; $i < $nrow ; ++$i) {
        for (my $j = $ncol - $i ; $j < $ncol ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_satriu()

Returns 1 is the invocand is strictly upper anti-triangular, and 0 otherwise.

    $bool = $x -> is_satriu();

A strictly anti-triangular matrix is a square matrix where all non-zero
elements are strictly above the main diagonal. It has the following pattern,
where only the elements marked as C<x> can be non-zero,

    [ x x x 0 ]
    [ x x 0 0 ]
    [ x 0 0 0 ]
    [ 0 0 0 0 ]

=cut

sub is_satriu {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = $ncol - $i - 1 ; $j < $ncol ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_atril()

Returns 1 is the invocand is lower anti-triangular, and 0 otherwise.

    $bool = $x -> is_atril();

A lower anti-triangular matrix is a square matrix where all non-zero elements
are on or below the main anti-diagonal. It has the following pattern, where
only the elements marked as C<x> can be non-zero,

    [ 0 0 0 x ]
    [ 0 0 x x ]
    [ 0 x x x ]
    [ x x x x ]

=cut

sub is_atril {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol - $i - 1; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=item is_satril()

Returns 1 is the invocand is strictly lower anti-triangular, and 0 otherwise.

    $bool = $x -> is_satril();

A strictly lower anti-triangular matrix is a square matrix where all non-zero
elements are strictly below the main anti-diagonal. It has the following
pattern, where only the elements marked as C<x> can be non-zero,

    [ 0 0 0 0 ]
    [ 0 0 0 x ]
    [ 0 0 x x ]
    [ 0 x x x ]

=cut

sub is_satril {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;

    my $nrow = $x -> nrow();
    my $ncol = $x -> ncol();

    return 0 unless $nrow == $ncol;

    for (my $i = 0 ; $i < $nrow ; ++$i) {
        for (my $j = 0 ; $j < $ncol - $i ; ++$j) {
            return 0 unless $x -> [$i][$j] == 0;
        }
    }

    return 1;
}

=pod

=back

=head2 Shape and size

=over

=item size()

You can determine the dimensions of a matrix by calling:

    ($m, $n) = $a->size;

=cut

sub size {
    my $self = shift;
    my $m = @{$self};
    my $n = $m ? @{$self->[0]} : 0;
    ($m, $n);
}

=pod

=item nelm()

Returns the number of elements in the matrix.

    $n = $x->nelm();

=cut

sub nelm {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return @$x ? @$x * @{$x->[0]} : 0;
}

=pod

=item nrow()

Returns the number of rows.

    $m = $x->nrow();

=cut

sub nrow {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return scalar @$x;
}

=pod

=item ncol()

Returns the number of columns.

    $n = $x->ncol();

=cut

sub ncol {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return @$x ? @{$x->[0]} : 0;
}

=pod

=item npag()

Returns the number of pages.

    $n = $x->pag();

=cut

sub npag {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return @$x ? 1 : 0;
}

=pod

=item ndim()

Returns the number of dimensions. This is the number of dimensions along which
the length is different from one.

    $n = $x->ndim();

=cut

sub ndim {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my ($nrow, $ncol) = $x -> size();
    my $ndim = 0;
    ++$ndim if $nrow != 1;
    ++$ndim if $ncol != 1;
    return $ndim;
}

=pod

=item reshape()

Returns a reshaped copy of a matrix. The reshaping is done by creating a new
matrix and looping over the elements in column major order. The new matrix must
have the same number of elements as the invocand matrix. The following returns
an C<$m>-by-C<$n> matrix,


    $y = $x -> reshape($m, $n);

The code

    $x = Math::Matrix -> new([[1, 3, 5, 7], [2, 4, 6, 8]]);
    $y = $x -> reshape(4, 2);

creates the matrix $x

    [ 1  3  5  7 ]
    [ 2  4  6  8 ]

and returns a reshaped copy $y

    [ 1  5 ]
    [ 2  6 ]
    [ 3  7 ]
    [ 4  8 ]

=cut

sub reshape {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 3;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 3;
    my $x = shift;
    my $class = ref $x;

    my ($nrowx, $ncolx) = $x -> size();
    my $nelmx = $nrowx * $ncolx;

    my ($nrowy, $ncoly) = @_;
    my $nelmy = $nrowy * $ncoly;

    croak "when reshaping, the number of elements can not change in ",
      (caller(0))[3] unless $nrowx * $ncolx == $nrowy * $ncoly;

    my $y = bless [], $class;

    for (my $k = 0 ; $k < $nelmx ; ++ $k) {
        my $ix = $k % $nrowx;
        my $jx = ($k - $ix) / $nrowx;
        my $iy = $k % $nrowy;
        my $jy = ($k - $iy) / $nrowy;
        $y -> [$iy][$jy] = $x -> [$ix][$jx];
    }

    return $y;
}

=pod

=item to_row()

Convert to a row.

    $x -> to_row();

This method reshapes the matrix into a single row.

=cut

sub to_row {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> reshape(1, $x -> nelm());
}

=pod

=item to_col()

Convert to a column.

    $y = $x -> to_col();

This method reshapes the matrix into a single column.

=cut

sub to_col {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    return $x -> reshape($x -> nelm(), 1);
}

=pod

=back

=head2 Other methods

=over

=item concat()

Concatenate matrices horizontally. The matrices must have the same number or
rows. The result is a new matrix or B<undef> in case of error.

    $x = Math::Matrix -> new([1, 2], [4, 5]);   # 2-by-2 matrix
    $y = Math::Matrix -> new([3], [6]);         # 2-by-1 matrix
    $z = $x -> concat($y);                      # 2-by-3 matrix

=cut

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

=pod

=item hcat()

Concatenate matrices horizontally. Any number of arguments is allowed.
Non-empty matrices must have the same number or rows. The result is a new
matrix.

    $x = Math::Matrix -> new([1, 2], [4, 5]);   # 2-by-2 matrix
    $y = Math::Matrix -> new([3], [6]);         # 2-by-1 matrix
    $z = $x -> hcat($y);                        # 2-by-3 matrix

=cut

sub hcat {
    my $x = shift;
    my $class = ref $x;

    my $nrowx = $x -> nrow();           # number of rows in $x
    my $z = $x -> clone();              # initialize output

    for (my $i = 0 ; $i <= $#_ ; ++$i) {
        my $y = $_[$i];
        next if $y -> is_empty();       # ignore empty $y
        $y = $y -> clone();

        if ($z -> is_empty()) {
            $z = $y;                    # $y is non-empty
            next;
        }

        croak "All operands must have the same number of rows in ",
          (caller(0))[3] unless $y -> nrow() == $nrowx;

        for (my $i = 0 ; $i < $nrowx ; ++$i) {
            push @{ $z -> [$i] }, @{ $y -> [$i] };
        }
    }

    return $z;
}

=pod

=item vcat()

Concatenate matrices vertically. Any number of arguments is allowed. Non-empty
matrices must have the same number or columns. The result is a new matrix.

    $x = Math::Matrix -> new([1, 2], [4, 5]);   # 2-by-2 matrix
    $y = Math::Matrix -> new([3, 6]);           # 1-by-2 matrix
    $z = $x -> vcat($y);                        # 3-by-2 matrix

=cut

sub vcat {
    my $x = shift;
    my $class = ref $x;

    my $ncolx = $x -> ncol();           # number of columns in $x
    my $z = $x -> clone();              # initialize output

    for (my $i = 0 ; $i <= $#_ ; ++$i) {
        my $y = $_[$i];
        next if $y -> is_empty();       # ignore empty $y
        $y = $y -> clone();

        if ($z -> is_empty()) {
            $z = $y;                    # $y is non-empty
            next;
        }

        croak "All operands must have the same number of columns in ",
          (caller(0))[3] unless $y -> ncol() == $ncolx;

        push @$z, @$y;
    }

    return $z;
}

=pod

=item swaprc()

Swap rows and columns. This method does nothing but shuffle elements around. For
real numbers, swaprc() is identical to the transpose() method.

A subclass implementing a matrix of complex numbers should provide a transpose()
method that also takes the complex conjugate of each elements. The swaprc()
method, on the other hand, should only shuffle elements around.

=cut

sub swaprc {
    my $x = shift;
    my $class = ref $x;

    my $y = bless [], $class;
    my $ncolx = $x -> ncol();
    return $y if $ncolx == 0;

    for (my $j = 0 ; $j < $ncolx ; ++$j) {
        push @$y, [ map $_->[$j], @$x ];
    }
    return $y;
}

=pod

=item flipud()

Flip upside-down, i.e., flip along dimension 1.

=cut

sub flipud {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my $y = [ reverse map { [ @$_ ] } @$x ];
    bless $y, $class;;
}

=pod

=item fliplr()

Flip left-to-right, i.e., flip along dimension 2.

=cut

sub fliplr {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 1;
    croak "Too many arguments for ", (caller(0))[3] if @_ > 1;
    my $x = shift;
    my $class = ref $x;

    my $y = [ map [ reverse @$_ ], @$x ];
    bless $y, $class;
}

=pod

=item transpose()

Returns the transposed matrix. This is the matrix where colums and rows of the
argument matrix are swapped.

=cut

sub transpose {
    my $x = shift;
    my $class = ref $x;

    my $y = bless [], $class;
    my $ncolx = $x -> ncol();
    return $y if $ncolx == 0;

    for (my $j = 0 ; $j < $ncolx ; ++$j) {
        push @$y, [ map $_->[$j], @$x ];
    }
    return $y;
}

=pod

=item negative()

Negate a matrix and return it.

    $a = Math::Matrix -> new([-2, 3]);
    $b = $a -> negative();                  # $b = [[2, -3]]

=cut

sub negative {
    shift->multiply_scalar(-1);
}

=pod

=item multiply()

Multiplies two matrices where the length of the rows in the first matrix is the
same as the length of the columns in the second matrix. Returns the product or
B<undef> in case of error.

=cut

sub multiply {
    my $self  = shift;
    my $class = ref($self);
    my $other = shift->transpose;
    my @result;

    return undef if $#{$self->[0]} != $#{$other->[0]};
    for my $row (@{$self}) {
        my $rescol = [];
        for my $col (@{$other}) {
            push(@{$rescol}, _vekpro($row,$col));
        }
        push(@result, $rescol);
    }
    $class->new(@result);
}

=pod

=item solve()

Solves a equation system given by the matrix. The number of colums must be
greater than the number of rows. If variables are dependent from each other,
the second and all further of the dependent coefficients are 0. This means the
method can handle such systems. The method returns a matrix containing the
solutions in its columns or B<undef> in case of error.

=cut

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

=pod

=item invert()

Invert a Matrix using C<solve>.

=cut

sub invert {
    my $M = shift;
    my ($m, $n) = $M->size;
    die "Matrix dimensions are $m X $n. -- Matrix not invertible.\n"
      if $m != $n;
    my $I = $M->new_identity($n);
    ($M->concat($I))->solve;
}

=pod

=item pinvert()

Compute the pseudo-inverse of the matrix: ((A'A)^-1)A'

=cut

sub pinvert {
    my $self  = shift;
    my $m    = $self->clone();

    $m->transpose->multiply($m)->invert->multiply($m->transpose);
}

=pod

=item multiply_scalar()

Multiplies a matrix and a scalar resulting in a matrix of the same dimensions
with each element scaled with the scalar.

    $a->multiply_scalar(2);  scale matrix by factor 2

=cut

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

=pod

=item add()

Add two matrices of the same dimensions.

=cut

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

=pod

=item subtract()

Shorthand for C<add($other-E<gt>negative)>

=cut

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

=pod

=item equal()

Decide if two matrices are equal. The criterion is, that each pair of elements
differs less than $Math::Matrix::eps.

=cut

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

=pod

=item slice()

Extract columns:

    a->slice(1,3,5);

=cut

sub slice {
    my $self = shift;
    my $class = ref($self);
    my $result = [];

    for my $i (0 .. $#$self) {
        push @$result, [ @{$self->[$i]}[@_] ];
    }

    bless $result, $class;
}

=pod

=item diagonal_vector()

Extract the diagonal as an array:

    $diag = $a->diagonal_vector;

=cut

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

=pod

=item tridiagonal_vector()

Extract the diagonals that make up a tridiagonal matrix:

    ($main_d, $upper_d, $lower_d) = $a->tridiagonal_vector;

=cut

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

=pod

=item determinant()

Compute the determinant of a matrix.

    $a = Math::Matrix->new([3, 1],
                           [4, 2]);
    $d = $a->determinant;                   # $d = 2

=cut

sub determinant {
    my $self = shift;
    my $class = ref($self);
    my $imax = $#$self;
    my $jmax = $#{$self->[0]};

    return undef unless $imax == $jmax;     # input must be a square matrix

    if ($imax == 0) {
        return $self->[0][0];
    } else {
        my $result = 0;

        # Create a matrix with row 0 removed. We only need to do this once.
        my $matrix0 = $class -> new(@$self[1 .. $jmax]);

        foreach my $j (0 .. $jmax) {

            # Create a matrix with row 0 and column $j removed.
            my $matrix0j = $matrix0 -> slice(0 .. $j-1, $j+1 .. $jmax);

            my $term = $matrix0j -> determinant();
            $term *= $j % 2 ? -$self->[0][$j]
                            :  $self->[0][$j];

            $result += $term;
        }
        return $result;
    }
}

=pod

=item dot_product()

Compute the dot product of two vectors. The second operand does not have to be
an object.

    # $x and $y are both objects
    $x = Math::Matrix -> new([1, 2, 3]);
    $y = Math::Matrix -> new([4, 5, 6]);
    $p = $x -> dot_product($y);             # $p = 32

    # Only $x is an object.
    $p = $x -> dot_product([4, 5, 6]);      # $p = 32

=cut

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

=pod

=item absolute()

Compute the absolute value (i.e., length) of a vector.

    $v = Math::Matrix -> new([3, 4]);
    $a = $v -> absolute();                  # $v = 5

=cut

sub absolute {
    my $vector = shift;
    sqrt $vector->dot_product($vector);
}

=pod

=item normalize()

Normalize a vector, i.e., scale a vector so its length becomes 1.

    $v = Math::Matrix -> new([3, 4]);
    $u = $v -> normalize();                 # $u = [ 0.6, 0.8 ]

=cut

sub normalize {
    my $vector = shift;
    my $length = $vector->absolute();
    return undef
      unless $length;
    $vector->multiply_scalar(1 / $length);
}

=pod

=item cross_product()

Compute the cross-product of vectors.

    $x = Math::Matrix -> new([1,3,2],
                             [5,4,2]);
    $p = $x -> cross_product();             # $p = [ -2, 8, -11 ]

=cut

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

=pod

=item sapply()

Scalar apply. Applies a subroutine to each element, or each set of corresponding
elements if multiple operands are given, and returns the result. The first argument
is the subroutine to apply. The following arguments, if any, are additional
matrices on which to apply the subroutine.

=over

=item One operand

With one operand, i.e., the invocand matrix, the subroutine is applied to each
element of the invocand matrix. The returned matrix has the same size as the
invocand. For example, multiplying the matrix C<$x> with the scalar C<$c>

    $sub = sub { $c * $_[0] };      # subroutine to multiply by $c
    $z = $x -> sapply($sub);        # multiply each element by $c

=item Two operands

When two operands are specfied, the subroutine is applied to each pair of
corresponding elements in the two operands. For example, adding two matrices can
be implemented as

    $sub = sub { $_[0] * $_[1] };
    $z = $x -> sapply($sub, $y);

Note that if the matrices have different sizes, the rows and/or columns of the
smaller are recycled to match the size of the larger. If C<$x> is a
C<$p>-by-C<$q> matrix and C<$y> is a C<$r>-by-C<$s> matrix, then C<$z> is a
max(C<$p>,C<$r>)-by-max(C<$q>,C<$s>) matrix, and

    $z -> [$i][$j] = $sub -> ($x -> [$i % $p][$j % $q],
                              $y -> [$i % $r][$j % $s]);

Because of this recycling, multiplying the matrix C<$x> with the scalar C<$c>
(see above) can also be implemented as

    $sub = sub { $_[0] * $_[1] };
    $z = $x -> sapply($sub, $c);

Generating a matrix with all combinations of C<$x**$y> for C<$x> being 4, 5, and
6 and C<$y> being 1, 2, 3, and 4 can be done with

    $c = Math::Matrix -> new([[4], [5], [6]]);      # 3-by-1 column
    $r = Math::Matrix -> new([[1, 2, 3, 4]]);       # 1-by-4 row
    $x = $c -> sapply(sub { $_[0] ** $_[1] }, $r);  # 3-by-4 matrix

=item Multiple operands

In general, the sapply() method can have any number of arguments. For example,
to compute the sum of the four matrices C<$x>, C<$y>, C<$z>, and C<$w>,

    $sub = sub {
               $sum = 0;
               for $val (@_) {
                   $sum += $val;
               };
               return $sum;
           };
    $x -> sapply($sub, $y, $z, $w);

=back

Note

=over

=item *

The number of rows in the output matrix equals the number of rows in the operand
with the largest number of rows. Ditto for columns.

=item *

For each operand that has a number of rows smaller than the maximum value, the
rows are recyled. Ditto for columns.

=item *

The subroutine is run in scalar context.

=item *

No checks are done on the return value of the subroutine.

=item *

Don't modify the variables $_[0], $_[1] etc. inside the subroutine. Otherwise,
there is a risk of modifying the operand matrices.

=item *

If the matrix elements are objects that are not cloned when the "=" (assignment)
operator is used, you might have to explicitly clone any objects used inside the
subroutine. Otherwise, the elements in the output matrix might be references to
objects in the operand matrices, rather than references to new objects.

=back

=cut

sub sapply {
    croak "Not enough arguments for ", (caller(0))[3] if @_ < 2;
    my $x = shift;
    my $class = ref $x;

    # Get the subroutine to apply on all the elements.

    my $sub = shift;
    croak "input argument must be a reference to a subroutine"
      unless ref($sub) eq 'CODE';

    my $y = bless [], $class;

    # For speed, treat a single matrix operand as a special case.

    if (@_ == 0) {
        my ($nrowx, $ncolx) = $x -> size();
        return $y if $nrowx * $ncolx == 0;      # quick exit if $x is empty

        for (my $i = 0 ; $i < $nrowx ; ++ $i) {
            for (my $j = 0 ; $j < $ncolx ; ++ $j) {
                $y -> [$i][$j] = $sub -> ($x -> [$i][$j]);
            }
        }

        return $y;
    }

    # Create some auxiliary arrays.

    my @args = ($x, @_);    # all matrices
    my @size = ();          # size of each matrix
    my @nelm = ();          # number of elements in each matrix

    # Get the size (number of rows and columns) in the output matrix.

    my $nrowy = 0;
    my $ncoly = 0;

    for (my $k = 0 ; $k <= $#args ; ++ $k) {

        # Get the number of rows, columns, and elements in the k'th argument,
        # and save this information for later.

        my ($nrowk, $ncolk) = $args[$k] -> size();
        $size[$k] = [ $nrowk, $ncolk ];
        $nelm[$k] = $nrowk * $ncolk;

        # Update the size of the output matrix.

        $nrowy = $nrowk if $nrowk > $nrowy;
        $ncoly = $ncolk if $ncolk > $ncoly;
    }

    # We only accept empty matrices if all matrices are empty.

    my $n_empty = grep { $_ == 0 } @nelm;
    return $y if $n_empty == @args;     # quick exit if all are empty

    # At ths point, we know that not all matrices are empty, but some might be
    # empty. We only continue if none are empty.

    croak "Either all or none of the matrices must be empty in ", (caller(0))[3]
      unless $n_empty == 0;

    # Loop over the subscripts into the output matrix.

    for (my $i = 0 ; $i < $nrowy ; ++ $i) {
        for (my $j = 0 ; $j < $ncoly ; ++ $j) {

            # Initialize the argument list for the subroutine call that will
            # give the value for element ($i,$j) in the output matrix.

            my @elms = ();

            # Loop over the matrices.

            for (my $k = 0 ; $k <= $#args ; ++ $k) {

                # Get the number of rows and columns in the k'th matrix.

                my $nrowk = $size[$k][0];
                my $ncolk = $size[$k][1];

                # Compute the subscripts of the element to use in the k'th
                # matrix.

                my $ik = $i % $nrowk;
                my $jk = $j % $ncolk;

                # Get the element from the k'th matrix to use in this call.

                $elms[$k] = $args[$k][$ik][$jk];
            }

            # Now we have the argument list for the subroutine call.

            $y -> [$i][$j] = $sub -> (@elms);
        }
    }

    return $y;
}

=pod

=item as_string()

Creates a string representation of the matrix and returns it.

    $x = Math::Matrix -> new([1, 2], [3, 4]);
    $s = $x -> as_string();

=cut

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

=pod

=item print()

Prints the matrix on STDOUT. If the method has additional parameters, these are
printed before the matrix is printed.

=cut

sub print {
    my $self = shift;

    print @_ if scalar(@_);
    print $self->as_string;
}

=pod

=item version()

Returns a string contining the package name and version number.

=cut

sub version {
    return "Math::Matrix $VERSION";
}

# Utility methods.

sub _vekpro {
    my($a, $b) = @_;
    my $result=0;

    for my $i (0 .. $#{$a}) {
        $result += $a->[$i] * $b->[$i];
    }
    $result;
}

=pod

=back

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

=head1 SUBCLASSING

The majority of methods work fine with numerical objects, provided that the
assignment operator C<=> returns a clone of the object and not just a reference
to the same object.

You can check the behaviour of the assignment operator by assigning a value to a
new variable, modify the new variable, and check whether this also modifies the
original value.

    $x = Some::Class -> new(0);           # create object $x
    $y = $x;                              # create new variable $y
    $y++;                                 # modify $y
    print "it's a clone\n" if $x != $y;   # is $x modified?

The subclass might need to implement some methods of its own. For instance, if
each element is a complex number, a transpose() method needs to be implemented
to take the complex conjugate of each value. An as_string() method might also be
useful for displaying the matrix in a format more suitable for the subclass.

Here is an example showing Math::Matrix::Complex, a fully-working subclass of
Math::Matrix, where each element is a Math::Complex object.

    use strict;
    use warnings;

    package Math::Matrix::Complex;

    use Math::Matrix;
    use Scalar::Util 'blessed';
    use Math::Complex 1.57;

    our @ISA = ('Math::Matrix');

    # We need a new() method to make sure every element is an object.

    sub new {
        my $self = shift;
        my $x = $self -> SUPER::new(@_);

        my $sub = sub {
            defined(blessed($_[0])) && $_[0] -> isa('Math::Complex')
              ? $_[0]
              : Math::Complex -> new($_[0]);
        };

        return $x -> sapply($sub);
    }

    # We need a transpose() method, since the transpose of a matrix
    # with complex numbers also takes the conjugate of all elements.

    sub transpose {
        my $x = shift;
        my $y = $x -> SUPER::transpose(@_);

        return $y -> sapply(sub { ~$_[0] });
    }

    # We need an as_string() method, since our parent's methods
    # doesn't format complex numbers correctly.

    sub as_string {
        my $self = shift;
        my $out = "";
        for my $row (@$self) {
            for my $elm (@$row) {
                $out = $out . sprintf "%10s ", $elm;
            }
            $out = $out . sprintf "\n";
        }
        $out;
    }

    1;

=head1 BUGS

Please report any bugs through the web interface at
L<https://rt.cpan.org/Ticket/Create.html?Queue=Math-Matrix>
(requires login). We will be notified, and then you'll automatically be
notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Math::Matrix

You can also look for information at:

=over 4

=item * GitHub Source Repository

L<https://github.com/pjacklam/p5-Math-Matrix>

=item * RT: CPAN's request tracker

L<https://rt.cpan.org/Public/Dist/Display.html?Name=Math-Matrix>

=item * CPAN Ratings

L<https://cpanratings.perl.org/dist/Math-Matrix>

=item * MetaCPAN

L<https://metacpan.org/release/Math-Matrix>

=item * CPAN Testers Matrix

L<http://matrix.cpantesters.org/?dist=Math-Matrix>

=back

=head1 LICENSE AND COPYRIGHT

Copyright (c) 2020, Peter John Acklam.

Copyright (C) 2013, John M. Gamble <jgamble@ripco.com>, all rights reserved.

Copyright (C) 2009, oshalla
https://rt.cpan.org/Public/Bug/Display.html?id=42919

Copyright (C) 2002, Bill Denney <gte273i@prism.gatech.edu>, all rights
reserved.

Copyright (C) 2001, Brian J. Watson <bjbrew@power.net>, all rights reserved.

Copyright (C) 2001, Ulrich Pfeifer <pfeifer@wait.de>, all rights reserved.
Copyright (C) 1995, Universität Dortmund, all rights reserved.

Copyright (C) 2001, Matthew Brett <matthew.brett@mrc-cbu.cam.ac.uk>

This program is free software; you may redistribute it and/or modify it under
the same terms as Perl itself.

=head1 AUTHORS

Peter John Acklam E<lt>pjacklam@gmail.comE<gt> (2020)

Ulrich Pfeifer E<lt>pfeifer@ls6.informatik.uni-dortmund.deE<gt> (1995-2013)

Brian J. Watson E<lt>bjbrew@power.netE<gt>

Matthew Brett E<lt>matthew.brett@mrc-cbu.cam.ac.ukE<gt>

=cut

1;
