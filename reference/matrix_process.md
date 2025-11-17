# Process matrix

Process matrix

## Usage

``` r
matrix_process(
  matrix,
  method = c("raw", "zscore", "fc", "log2fc", "log1p"),
  ...
)
```

## Arguments

- matrix:

  A matrix.

- method:

  Method to use for processing the matrix.

- ...:

  Other arguments passed to the method.

## Value

A processed matrix.

## Examples

``` r
m <- simulate_sparse_matrix(10, 10)
matrix_process(m, method = "raw")
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘col_1’, ‘col_2’, ‘col_3’ ... ]]
#>                           
#> row_1  1 . . . . . . . . .
#> row_2  . . . . . . . . . .
#> row_3  . . . . . . . . . .
#> row_4  . . . 1 . . . . . .
#> row_5  . . . . . . . . . .
#> row_6  . . . . . . . . . .
#> row_7  . . . . . . . . 1 .
#> row_8  . . . . . . 2 . . .
#> row_9  . . . 2 . . . . . .
#> row_10 . . . . . . . . . .
matrix_process(m, method = "zscore")
#>             col_1      col_2      col_3      col_4      col_5      col_6
#> row_1   2.8460499 -0.3162278 -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_2         NaN        NaN        NaN        NaN        NaN        NaN
#> row_3         NaN        NaN        NaN        NaN        NaN        NaN
#> row_4  -0.3162278 -0.3162278 -0.3162278  2.8460499 -0.3162278 -0.3162278
#> row_5         NaN        NaN        NaN        NaN        NaN        NaN
#> row_6         NaN        NaN        NaN        NaN        NaN        NaN
#> row_7  -0.3162278 -0.3162278 -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_8  -0.3162278 -0.3162278 -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_9  -0.3162278 -0.3162278 -0.3162278  2.8460499 -0.3162278 -0.3162278
#> row_10        NaN        NaN        NaN        NaN        NaN        NaN
#>             col_7      col_8      col_9     col_10
#> row_1  -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_2         NaN        NaN        NaN        NaN
#> row_3         NaN        NaN        NaN        NaN
#> row_4  -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_5         NaN        NaN        NaN        NaN
#> row_6         NaN        NaN        NaN        NaN
#> row_7  -0.3162278 -0.3162278  2.8460499 -0.3162278
#> row_8   2.8460499 -0.3162278 -0.3162278 -0.3162278
#> row_9  -0.3162278 -0.3162278 -0.3162278 -0.3162278
#> row_10        NaN        NaN        NaN        NaN
#> attr(,"scaled:center")
#>  row_1  row_2  row_3  row_4  row_5  row_6  row_7  row_8  row_9 row_10 
#>    0.1    0.0    0.0    0.1    0.0    0.0    0.1    0.2    0.2    0.0 
#> attr(,"scaled:scale")
#>     row_1     row_2     row_3     row_4     row_5     row_6     row_7     row_8 
#> 0.3162278 0.0000000 0.0000000 0.3162278 0.0000000 0.0000000 0.3162278 0.6324555 
#>     row_9    row_10 
#> 0.6324555 0.0000000 
matrix_process(m, method = "fc")
#> 10 x 10 Matrix of class "dgeMatrix"
#>        col_1 col_2 col_3 col_4 col_5 col_6 col_7 col_8 col_9 col_10
#> row_1     10     0     0     0     0     0     0     0     0      0
#> row_2    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_3    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_4      0     0     0    10     0     0     0     0     0      0
#> row_5    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_6    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_7      0     0     0     0     0     0     0     0    10      0
#> row_8      0     0     0     0     0     0    10     0     0      0
#> row_9      0     0     0    10     0     0     0     0     0      0
#> row_10   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
matrix_process(m, method = "log2fc")
#> 10 x 10 Matrix of class "dgeMatrix"
#>           col_1 col_2 col_3    col_4 col_5 col_6    col_7 col_8    col_9 col_10
#> row_1  3.321928  -Inf  -Inf     -Inf  -Inf  -Inf     -Inf  -Inf     -Inf   -Inf
#> row_2       NaN   NaN   NaN      NaN   NaN   NaN      NaN   NaN      NaN    NaN
#> row_3       NaN   NaN   NaN      NaN   NaN   NaN      NaN   NaN      NaN    NaN
#> row_4      -Inf  -Inf  -Inf 3.321928  -Inf  -Inf     -Inf  -Inf     -Inf   -Inf
#> row_5       NaN   NaN   NaN      NaN   NaN   NaN      NaN   NaN      NaN    NaN
#> row_6       NaN   NaN   NaN      NaN   NaN   NaN      NaN   NaN      NaN    NaN
#> row_7      -Inf  -Inf  -Inf     -Inf  -Inf  -Inf     -Inf  -Inf 3.321928   -Inf
#> row_8      -Inf  -Inf  -Inf     -Inf  -Inf  -Inf 3.321928  -Inf     -Inf   -Inf
#> row_9      -Inf  -Inf  -Inf 3.321928  -Inf  -Inf     -Inf  -Inf     -Inf   -Inf
#> row_10      NaN   NaN   NaN      NaN   NaN   NaN      NaN   NaN      NaN    NaN
matrix_process(m, method = "log1p")
#> 10 x 10 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 10 column names ‘col_1’, ‘col_2’, ‘col_3’ ... ]]
#>                                                          
#> row_1  0.6931472 . . .         . . .        . .         .
#> row_2  .         . . .         . . .        . .         .
#> row_3  .         . . .         . . .        . .         .
#> row_4  .         . . 0.6931472 . . .        . .         .
#> row_5  .         . . .         . . .        . .         .
#> row_6  .         . . .         . . .        . .         .
#> row_7  .         . . .         . . .        . 0.6931472 .
#> row_8  .         . . .         . . 1.098612 . .         .
#> row_9  .         . . 1.0986123 . . .        . .         .
#> row_10 .         . . .         . . .        . .         .
m <- as_matrix(m)
matrix_process(m, method = function(x) x / rowMeans(x))
#>        col_1 col_2 col_3 col_4 col_5 col_6 col_7 col_8 col_9 col_10
#> row_1     10     0     0     0     0     0     0     0     0      0
#> row_2    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_3    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_4      0     0     0    10     0     0     0     0     0      0
#> row_5    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_6    NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
#> row_7      0     0     0     0     0     0     0     0    10      0
#> row_8      0     0     0     0     0     0    10     0     0      0
#> row_9      0     0     0    10     0     0     0     0     0      0
#> row_10   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN   NaN    NaN
```
