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
#>  [1,]    6    2    4
#>  [2,]    9    5   10
#>  [3,]    9    4    3
#>  [4,]    2    1    7
#>  [5,]    7    1    8
#>  [6,]    8   10    6
#>  [7,]    5    3    2
#>  [8,]    9    4    5
#>  [9,]    1    8    4
#> [10,]    2    5    3
#> 
#> $value
#>             [,1]       [,2]        [,3]
#>  [1,] -1.4967374 -1.4144681 -1.20238430
#>  [2,] -1.1314671 -1.1103652 -0.31393823
#>  [3,] -1.5314701 -0.3533680 -0.23713089
#>  [4,] -1.2557505 -1.1543336 -1.08389985
#>  [5,] -2.0061115 -1.7614451 -0.41335601
#>  [6,] -1.3302356 -0.8189035 -0.72620714
#>  [7,] -0.9391066 -0.7533612 -0.47818370
#>  [8,] -1.8326439 -1.4601870 -0.75057316
#>  [9,] -0.5196884 -0.1307660  0.05590213
#> [10,] -1.8452639 -1.5611708 -0.82122221
#> 
```
