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
#>  [1,]    3    5   NA
#>  [2,]    9    1    5
#>  [3,]    7    1    3
#>  [4,]    5    9   NA
#>  [5,]   10    3    9
#>  [6,]    7    1   NA
#>  [7,]    3    6   NA
#>  [8,]    4    6    9
#>  [9,]    8    5    7
#> [10,]    5    7   NA
#> [11,]    1    4    8
#> [12,]    9    4   10
#> [13,]   10    9   NA
#> [14,]    9    8   NA
#> [15,]    4    1   10
#> [16,]    8   NA   NA
#> [17,]    8    2    6
#> [18,]   10    7   NA
#> [19,]    2    5    7
#> [20,]    2    3    7
#> 
#> $value
#>        [,1]   [,2]    [,3]
#>  [1,] -0.66 -1.400  0.0000
#>  [2,]  1.00  0.710 -0.0220
#>  [3,]  0.73  0.085 -1.4000
#>  [4,] -0.70 -0.750  0.0000
#>  [5,]  1.50  1.300  1.1000
#>  [6,] -1.10 -1.200  0.0000
#>  [7,] -0.58 -0.670  0.0000
#>  [8,]  0.68 -0.670 -1.8000
#>  [9,]  0.69  0.550  0.2700
#> [10,]  1.20  0.320  0.0000
#> [11,]  1.20 -0.200 -0.2900
#> [12,]  1.80 -0.021 -0.3500
#> [13,]  0.85  0.100  0.0000
#> [14,]  1.50  0.540  0.0000
#> [15,]  1.40  0.490 -0.9000
#> [16,]  0.36  0.000  0.0000
#> [17,]  0.88  0.640 -0.0062
#> [18,]  0.22 -0.052  0.0000
#> [19,] -1.00 -1.100 -2.1000
#> [20,]  2.00  0.880 -0.1600
#> 
```
