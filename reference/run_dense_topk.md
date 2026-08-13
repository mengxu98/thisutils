# Dense matrix top-k selection

For each column or row of a dense numeric matrix, extract the top `k`
elements and their indices. The default returns the largest values; set
`decreasing = FALSE` to retain the smallest values, as in
nearest-neighbor selection from a distance matrix.

## Usage

``` r
run_dense_topk(x, k, by = c("col", "row"), decreasing = TRUE)
```

## Arguments

- x:

  A numeric matrix (or something coercible to one).

- k:

  Number of entries to retain per column or row. Must be a positive
  integer.

- by:

  Direction of selection: `"col"` ranks row entries independently within
  each column, while `"row"` ranks column entries independently within
  each row.

- decreasing:

  Whether to sort in decreasing order (largest values first). Default is
  `TRUE`.

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
m <- matrix(rnorm(100), nrow = 10)
run_dense_topk(m, k = 3, by = "col")
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]    8    3    1
#>  [2,]    6    4    8
#>  [3,]    7    5    2
#>  [4,]    9    6    4
#>  [5,]    3    9    6
#>  [6,]    2    9    4
#>  [7,]    7    6    1
#>  [8,]   10    7    3
#>  [9,]    6    3   10
#> [10,]    7    1   10
#> 
#> $value
#>            [,1]      [,2]       [,3]
#>  [1,] 1.8053943 0.8446488 0.74003439
#>  [2,] 0.9067389 0.8855312 0.85322610
#>  [3,] 2.4990996 1.4873832 0.74177161
#>  [4,] 1.6271062 1.0842490 0.83660775
#>  [5,] 2.2037697 1.5413541 0.70062343
#>  [6,] 1.2945743 0.2552193 0.06361308
#>  [7,] 1.6258149 1.3936649 0.55401422
#>  [8,] 1.0464316 0.8316341 0.56749428
#>  [9,] 1.4679519 1.3615273 1.28436952
#> [10,] 2.4123079 0.9631529 0.90751405
#> 
run_dense_topk(m, k = 3, by = "row")
#> $idx
#>       [,1] [,2] [,3]
#>  [1,]   10    1    7
#>  [2,]    6    9    3
#>  [3,]    5    9    1
#>  [4,]    2    4    5
#>  [5,]    3    4    5
#>  [6,]    9    7    4
#>  [7,]    3   10    7
#>  [8,]    1    2    7
#>  [9,]    4    5    9
#> [10,]    9    8   10
#> 
#> $value
#>            [,1]      [,2]       [,3]
#>  [1,] 0.9631529 0.7400344 0.55401422
#>  [2,] 1.2945743 1.0535059 0.74177161
#>  [3,] 2.2037697 1.3615273 0.84464882
#>  [4,] 0.8855312 0.8366077 0.08943632
#>  [5,] 1.4873832 0.4702746 0.40537212
#>  [6,] 1.4679519 1.3936649 1.08424900
#>  [7,] 2.4990996 2.4123079 1.62581486
#>  [8,] 1.8053943 0.8532261 0.40900106
#>  [9,] 1.6271062 1.5413541 0.65319738
#> [10,] 1.2843695 1.0464316 0.90751405
#> 
run_dense_topk(m, k = 3, by = "col", decreasing = FALSE)
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
