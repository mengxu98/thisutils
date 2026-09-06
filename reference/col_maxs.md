# Column maxima

Column maxima

## Usage

``` r
col_maxs(x, na.rm = TRUE)
```

## Arguments

- x:

  A dense or sparse matrix.

- na.rm:

  Whether to ignore \`NA\` values.

## Value

A numeric vector of column maxima. Implicit sparse zeros are treated as
zero.

## Examples

``` r
col_maxs(matrix(c(-2, 0, 3, 1), nrow = 2))
#> [1] 0 3
```
