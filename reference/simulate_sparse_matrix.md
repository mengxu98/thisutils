# Generate a simulated sparse matrix

This function generates a sparse matrix with a specified number of rows
and columns, a given sparsity level, and a distribution function for the
non-zero values.

## Usage

``` r
simulate_sparse_matrix(
  nrow,
  ncol,
  sparsity = 0.95,
  distribution_fun = function(n) stats::rpois(n, lambda = 0.5) + 1,
  decimal = 0,
  seed = 1
)
```

## Arguments

- nrow:

  Number of rows in the matrix.

- ncol:

  Number of columns in the matrix.

- sparsity:

  Proportion of zero elements (sparsity level). Default is `0.95`,
  meaning 95% of elements are zero (5% are non-zero).

- distribution_fun:

  Function to generate non-zero values.

- decimal:

  Controls the number of decimal places in the generated values. If set
  to `0`, values will be integers. When `decimal` \> 0, random decimal
  parts are uniformly distributed across the full range. Default is `0`.

- seed:

  Random seed for reproducibility.

## Value

A sparse matrix of class "dgCMatrix".

## Examples

``` r
simulate_sparse_matrix(1000, 500) |>
  check_sparsity()
#> [1] 0.95

simulate_sparse_matrix(10, 10, decimal = 1)
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘col_1’, ‘col_2’, ‘col_3’ ... ]]
#>                                   
#> row_1  1.8 . . .   . . .   . .   .
#> row_2  .   . . .   . . .   . .   .
#> row_3  .   . . .   . . .   . .   .
#> row_4  .   . . 1.5 . . .   . .   .
#> row_5  .   . . .   . . .   . .   .
#> row_6  .   . . .   . . .   . .   .
#> row_7  .   . . .   . . .   . 1.7 .
#> row_8  .   . . .   . . 2.7 . .   .
#> row_9  .   . . 2.4 . . .   . .   .
#> row_10 .   . . .   . . .   . .   .
simulate_sparse_matrix(10, 10, decimal = 5)
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘col_1’, ‘col_2’, ‘col_3’ ... ]]
#>                                                  
#> row_1  1.76984 . . .      . . .       . .       .
#> row_2  .       . . .      . . .       . .       .
#> row_3  .       . . .      . . .       . .       .
#> row_4  .       . . 1.4977 . . .       . .       .
#> row_5  .       . . .      . . .       . .       .
#> row_6  .       . . .      . . .       . .       .
#> row_7  .       . . .      . . .       . 1.71762 .
#> row_8  .       . . .      . . 2.68702 . .       .
#> row_9  .       . . 2.3841 . . .       . .       .
#> row_10 .       . . .      . . .       . .       .
```
