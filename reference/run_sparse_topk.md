# Sparse matrix top-k selection

For each column or row of a sparse `dgCMatrix`, extract the top `k`
matrix elements and their indices. By default, unstored positions
participate with their matrix value of zero. For column-wise selection,
this gives the same semantics as
[`run_dense_topk_by_column()`](https://mengxu98.github.io/thisutils/reference/run_dense_topk_by_column.md).

## Usage

``` r
run_sparse_topk(
  x,
  k,
  by = c("col", "row"),
  decreasing = TRUE,
  include_implicit_zeros = TRUE
)
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

- include_implicit_zeros:

  Whether unstored sparse positions participate as zeros. The default,
  `TRUE`, gives ordinary matrix semantics. Set to `FALSE` to rank only
  stored entries; for that use case, prefer the more explicit
  [`run_sparse_topk_stored()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_stored.md).

## Value

A list with two components:

- idx:

  Integer matrix of 1-based indices into the opposite dimension.

- value:

  Numeric matrix of corresponding values.

Both matrices have `ncol(x)` rows when `by = "col"` and `nrow(x)` rows
when `by = "row"`.

## Examples

``` r
m <- Matrix::rsparsematrix(10, 20, density = 0.3)
run_sparse_topk(m, k = 3, by = "col")
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]    1    2    4
#>  [2,]    9    1    2
#>  [3,]    7    1    2
#>  [4,]    1    2    3
#>  [5,]   10    3    9
#>  [6,]    2    3    4
#>  [7,]    1    2    4
#>  [8,]    4    1    2
#>  [9,]    8    5    7
#> [10,]    5    7    1
#> [11,]    1    2    3
#> [12,]    9    1    2
#> [13,]   10    9    1
#> [14,]    9    8    1
#> [15,]    4    1    2
#> [16,]    8    1    2
#> [17,]    8    2    1
#> [18,]   10    1    2
#> [19,]    1    3    4
#> [20,]    2    3    1
#> 
#> $value
#>       [,1]  [,2] [,3]
#>  [1,] 0.00 0.000 0.00
#>  [2,] 1.00 0.710 0.00
#>  [3,] 0.73 0.085 0.00
#>  [4,] 0.00 0.000 0.00
#>  [5,] 1.50 1.300 1.10
#>  [6,] 0.00 0.000 0.00
#>  [7,] 0.00 0.000 0.00
#>  [8,] 0.68 0.000 0.00
#>  [9,] 0.69 0.550 0.27
#> [10,] 1.20 0.320 0.00
#> [11,] 1.20 0.000 0.00
#> [12,] 1.80 0.000 0.00
#> [13,] 0.85 0.100 0.00
#> [14,] 1.50 0.540 0.00
#> [15,] 1.40 0.490 0.00
#> [16,] 0.36 0.000 0.00
#> [17,] 0.88 0.640 0.00
#> [18,] 0.22 0.000 0.00
#> [19,] 0.00 0.000 0.00
#> [20,] 2.00 0.880 0.00
#> 
run_sparse_topk(m, k = 3, by = "row")
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]   11    2   15
#>  [2,]   20   17    9
#>  [3,]    5   20    2
#>  [4,]   15    8    1
#>  [5,]   10    9    3
#>  [6,]    1    2    3
#>  [7,]    3    5   10
#>  [8,]   17    9   14
#>  [9,]   12   14    5
#> [10,]    5   13   18
#> 
#> $value
#>       [,1] [,2] [,3]
#>  [1,] 1.20 0.71 0.49
#>  [2,] 2.00 0.64 0.17
#>  [3,] 1.30 0.88 0.00
#>  [4,] 1.40 0.68 0.00
#>  [5,] 1.20 0.55 0.00
#>  [6,] 0.00 0.00 0.00
#>  [7,] 0.73 0.44 0.32
#>  [8,] 0.88 0.69 0.54
#>  [9,] 1.80 1.50 1.10
#> [10,] 1.50 0.85 0.22
#> 
```
