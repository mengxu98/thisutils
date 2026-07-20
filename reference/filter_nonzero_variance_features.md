# Keep matrix rows with nonzero variance

Subset a matrix to selected rows and discard rows with missing or zero
variance. Sparse input remains sparse.

## Usage

``` r
filter_nonzero_variance_features(x, features)
```

## Arguments

- x:

  A matrix or a \`Matrix\` sparse matrix.

- features:

  Row names or indices to retain before variance filtering.

## Value

\`x\` restricted to selected rows whose variance is positive.

## Examples

``` r
filter_nonzero_variance_features(
  matrix(c(1, 1, 1, 1, 1, 2), nrow = 2),
  features = 1:2
)
#>      [,1] [,2] [,3]
#> [1,]    1    1    2
```
