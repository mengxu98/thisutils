# Compute row variances

Compute sample or population variances for each row of a dense or sparse
matrix without densifying sparse input.

## Usage

``` r
fast_row_vars(x, unbiased = TRUE)
```

## Arguments

- x:

  A matrix or a \`Matrix\` sparse matrix.

- unbiased:

  Whether to compute the unbiased sample variance (the default) instead
  of the population variance.

## Value

A numeric vector of row variances. Row names are retained when
available. Matrices with zero columns return \`NA\` for every row. A
one-column matrix returns \`NA\` for sample variance and \`0\` for
population variance.

## Examples

``` r
fast_row_vars(matrix(c(1, 2, 3, 4), nrow = 2))
#> [1] 2 2
```
