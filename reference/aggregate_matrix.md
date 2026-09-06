# Aggregate matrix over groups

Aggregate matrix over groups

## Usage

``` r
aggregate_matrix(x, groups = NULL, fun = "mean")
```

## Arguments

- x:

  A matrix.

- groups:

  A character vector with the groups to aggregate over. Length must be
  \`nrow(x)\` or \`1\`.

- fun:

  Summary applied to each group: \`"mean"\`, \`"sum"\`, \`"count"\`,
  \`"max"\`, \`"min"\`, or a function.

## Value

A sparse summary matrix with groups in rows.

## Examples

``` r
aggregate_matrix(
  matrix(1:6, nrow = 3),
  groups = c("a", "a", "b"),
  fun = "mean"
)
#> 2 x 2 sparse Matrix of class "dgCMatrix"
#>          
#> a 1.5 4.5
#> b 3.0 6.0
```
