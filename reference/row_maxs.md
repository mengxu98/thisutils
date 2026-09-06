# Row maxima

Row maxima

## Usage

``` r
row_maxs(x, na.rm = TRUE)
```

## Arguments

- x:

  A dense or sparse matrix.

- na.rm:

  Whether to ignore \`NA\` values.

## Value

A numeric vector of row maxima. Implicit sparse zeros are treated as
zero.

## Examples

``` r
row_maxs(matrix(c(-2, 0, 3, 1), nrow = 2))
#> [1] 3 1
```
