# Compute unbiased row variances

Compute unbiased sample variances for each row of a dense or sparse
matrix without densifying sparse input.

## Usage

``` r
fast_row_vars(x)
```

## Arguments

- x:

  A matrix or a \`Matrix\` sparse matrix.

## Value

A numeric vector of row variances. Row names are retained when
available. Matrices with zero or one column return \`NA\` for every row.

## Examples

``` r
fast_row_vars(matrix(c(1, 2, 3, 4), nrow = 2))
#> [1] 2 2
```
