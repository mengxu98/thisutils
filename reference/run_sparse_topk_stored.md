# Stored sparse entries top-k selection

For each column or row of a sparse `dgCMatrix`, rank only explicitly
stored entries. Unstored zeros never become candidates. This is useful
for sparse adjacency matrices, where stored entries represent graph
edges rather than samples from a full numeric matrix.

## Usage

``` r
run_sparse_topk_stored(x, k, by = c("col", "row"), decreasing = TRUE)
```

## Arguments

- x:

  A `dgCMatrix` (or something coercible to one).

- k:

  Number of top entries to retain per column or row. Must be a positive
  integer.

- by:

  Direction of selection: `"col"` ranks row entries independently within
  each column, while `"row"` ranks column entries independently within
  each row.

- decreasing:

  Whether to sort in decreasing order (largest values first). Default is
  `TRUE`.

## Value

A list with `idx` and `value` matrices as described in
[`run_sparse_topk()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk.md).
Groups with fewer than `k` stored entries are padded with `NA` in both
matrices.

## Examples

``` r
graph <- Matrix::sparseMatrix(
  i = c(1, 3, 2), j = c(1, 1, 2), x = c(1, 3, -2),
  dims = c(3, 2)
)
run_sparse_topk_stored(graph, k = 2, by = "col")
#> $idx
#>      [,1] [,2]
#> [1,]    3    1
#> [2,]    2   NA
#> 
#> $value
#>      [,1] [,2]
#> [1,]    3    1
#> [2,]   -2   NA
#> 
```
