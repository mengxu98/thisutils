# Fast sparse aggregation

Fast sparse aggregation

## Usage

``` r
fast_aggregate(x, groupings = NULL, form = NULL, fun = "sum", ...)
```

## Arguments

- x:

  A matrix. Dense inputs are coerced to a sparse matrix.

- groupings:

  Group labels, recycled to a one-column data frame.

- form:

  Optional model formula used to build the grouping design.

- fun:

  Aggregation: \`"sum"\`, \`"mean"\`, or \`"count"\`.

- ...:

  Unused.

## Value

A sparse matrix with groups in rows.

## Examples

``` r
fast_aggregate(
  matrix(1:6, nrow = 3),
  groupings = c("a", "a", "b"),
  fun = "sum"
)
#> 2 x 2 sparse Matrix of class "dgCMatrix"
#>      
#> a 3 9
#> b 3 6
```
