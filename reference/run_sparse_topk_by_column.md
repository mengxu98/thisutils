# Sparse matrix top-k by column

For each column of a sparse `dgCMatrix`, extract the top `k` entries
(rows and values) sorted by value.

## Usage

``` r
run_sparse_topk_by_column(x, k, decreasing = TRUE)
```

## Arguments

- x:

  A `dgCMatrix` (or something coercible to one).

- k:

  Number of top entries to retain per column. Must be a positive
  integer.

- decreasing:

  Whether to sort in decreasing order (largest values first). Default is
  `TRUE`.

## Value

A list with two components:

- idx:

  Integer matrix (`ncol × k`) of 1-based row indices.

- value:

  Numeric matrix (`ncol × k`) of corresponding values.

## Examples

``` r
m <- Matrix::rsparsematrix(10, 20, density = 0.3)
run_sparse_topk_by_column(m, k = 3)
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]    3    2   NA
#>  [2,]    6   NA   NA
#>  [3,]    1   10   NA
#>  [4,]    9   10   NA
#>  [5,]    5   10    3
#>  [6,]    7    1   NA
#>  [7,]    6    3    7
#>  [8,]    5    6    4
#>  [9,]   10    8    5
#> [10,]    7    8   NA
#> [11,]    8    1    4
#> [12,]   10    9   NA
#> [13,]    6    4   10
#> [14,]    5   10    1
#> [15,]    6   10    1
#> [16,]    6    5    7
#> [17,]    1    8   10
#> [18,]    1   10    3
#> [19,]    9    1   NA
#> [20,]   10    5    2
#> 
#> $value
#>         [,1]   [,2]    [,3]
#>  [1,]  0.220 -1.000  0.0000
#>  [2,]  1.100  0.000  0.0000
#>  [3,] -0.750 -1.200  0.0000
#>  [4,]  1.200 -0.052  0.0000
#>  [5,]  1.800  0.850 -0.2900
#>  [6,] -0.360 -1.100  0.0000
#>  [7,]  2.400  1.500 -1.0000
#>  [8,]  1.100 -0.180 -1.1000
#>  [9,]  2.000  0.320 -0.0062
#> [10,]  0.085 -0.780  0.0000
#> [11,]  0.640 -1.200 -2.1000
#> [12,]  0.490 -1.000  0.0000
#> [13,]  0.600 -0.580 -1.1000
#> [14,]  0.950  0.051 -0.6600
#> [15,]  1.100 -0.890 -1.4000
#> [16,] -0.900 -1.400 -1.6000
#> [17,]  0.880  0.550 -0.6100
#> [18,]  1.800  0.680  0.2000
#> [19,]  1.000  0.710  0.0000
#> [20,]  0.360 -0.200 -0.3400
#> 
```
