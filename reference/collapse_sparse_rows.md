# Collapse sparse matrix rows by group

Collapse sparse matrix rows by group

## Usage

``` r
collapse_sparse_rows(matrix, group)
```

## Arguments

- matrix:

  A sparse matrix.

- group:

  A vector defining the output row groups.

## Value

A sparse matrix with rows collapsed by \`group\`.

## Examples

``` r
mat <- Matrix::Matrix(
  matrix(c(1, 0, 2, 0, 3, 4), nrow = 3, byrow = TRUE),
  sparse = TRUE
)
collapse_sparse_rows(mat, c("g1", "g1", "g2"))
#> 2 x 2 sparse Matrix of class "dgCMatrix"
#>       
#> g1 3 .
#> g2 3 4
```
