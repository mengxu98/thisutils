# Convert matrix into dense/sparse matrix

Convert matrix into dense/sparse matrix

## Usage

``` r
as_matrix(x, return_sparse = FALSE)
```

## Arguments

- x:

  A matrix.

- return_sparse:

  Whether to output a sparse matrix. Default is `FALSE`.

## Value

A dense or sparse matrix.

## Examples

``` r
m <- simulate_sparse_matrix(
  1000, 1000,
  decimal = 3
)

a <- as_matrix(m)
a[1:5, 1:5]
#>       col_1 col_2 col_3 col_4 col_5
#> row_1 2.266     0     0 0.000     0
#> row_2 0.000     0     0 2.467     0
#> row_3 0.000     0     0 0.000     0
#> row_4 0.000     0     0 0.000     0
#> row_5 0.000     0     0 0.000     0

b <- as_matrix(m, return_sparse = TRUE)
b[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dgCMatrix"
#>       col_1 col_2 col_3 col_4 col_5
#> row_1 2.266     .     . .         .
#> row_2 .         .     . 2.467     .
#> row_3 .         .     . .         .
#> row_4 .         .     . .         .
#> row_5 .         .     . .         .
```
