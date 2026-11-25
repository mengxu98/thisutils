# Sum matrix columns by group

Sum matrix columns by group

## Usage

``` r
sum_columns_by_group(x, groups)
```

## Arguments

- x:

  A numeric matrix or a two-dimensional matrix object that supports
  multiplication by a sparse Matrix.

- groups:

  A vector of non-missing, non-empty group labels, one per column of
  `x`. Labels are converted to character.

## Value

A matrix object with one column per group in first-appearance order. Row
names are preserved. Sparse Matrix input gives sparse output, and
missing matrix values propagate within their group.

## Examples

``` r
x <- matrix(1:6, nrow = 2)
sum_columns_by_group(x, c("b", "a", "b"))
#> 2 x 2 Matrix of class "dgeMatrix"
#>      b a
#> [1,] 6 3
#> [2,] 8 4
```
