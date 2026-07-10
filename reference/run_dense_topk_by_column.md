# Dense matrix top-k by column

For each column of a dense numeric matrix, extract the top `k` entries
(rows and values) sorted by value.

## Usage

``` r
run_dense_topk_by_column(x, k, decreasing = FALSE)
```

## Arguments

- x:

  A numeric matrix (or something coercible to one).

- k:

  Number of top entries to retain per column. Must be a positive
  integer.

- decreasing:

  Whether to sort in decreasing order (largest values first). Default is
  `FALSE`.

## Value

A list with two components:

- idx:

  Integer matrix (`ncol × k`) of 1-based row indices.

- value:

  Numeric matrix (`ncol × k`) of corresponding values.

## Examples

``` r
m <- matrix(rnorm(100), nrow = 10)
run_dense_topk_by_column(m, k = 3)
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]    6    3    7
#>  [2,]    7    2    4
#>  [3,]    6    2    4
#>  [4,]    9    5   10
#>  [5,]    9    4    3
#>  [6,]    2    1    7
#>  [7,]    7    1    8
#>  [8,]    8   10    6
#>  [9,]    5    3    2
#> [10,]    9    4    5
#> 
#> $value
#>             [,1]       [,2]       [,3]
#>  [1,] -2.0360025 -1.0518756 -0.7783959
#>  [2,] -1.2720655 -0.9492349 -0.7456859
#>  [3,] -1.4967374 -1.4144681 -1.2023843
#>  [4,] -1.1314671 -1.1103652 -0.3139382
#>  [5,] -1.5314701 -0.3533680 -0.2371309
#>  [6,] -1.2557505 -1.1543336 -1.0838999
#>  [7,] -2.0061115 -1.7614451 -0.4133560
#>  [8,] -1.3302356 -0.8189035 -0.7262071
#>  [9,] -0.9391066 -0.7533612 -0.4781837
#> [10,] -1.8326439 -1.4601870 -0.7505732
#> 
```
