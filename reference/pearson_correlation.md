# Correlation and covariance calculation for sparse matrix

Correlation and covariance calculation for sparse matrix

## Usage

``` r
pearson_correlation(x, y = NULL, max_dense_bytes = Inf)
```

## Arguments

- x:

  A numeric matrix or sparse `Matrix` with observations in rows.

- y:

  An optional numeric matrix or sparse `Matrix` with the same number of
  rows as `x`.

- max_dense_bytes:

  Maximum estimated bytes allowed for the dense covariance, correlation,
  and cross-product matrices. The default is \`Inf\` for backward
  compatibility. Supply a finite value to enable the guard.

## Value

A list with covariance and correlation matrices.

## Details

This lower-level helper deliberately returns dense covariance and
correlation matrices and therefore requires memory proportional to
\`ncol(x) \* ncol(y)\`. Prefer \[sparse_cor()\] when a sparse, blockwise
result is sufficient.

## Examples

``` r
m1 <- simulate_sparse_matrix(
  100, 100
)
m2 <- simulate_sparse_matrix(
  100, 100,
  sparsity = 0.05
)
a <- pearson_correlation(m1, m2)
a$cov[1:5, 1:5]
#>              col_1        col_2        col_3        col_4        col_5
#> col_1 -0.005050505  0.016161616  0.002020202 -0.014646465 -0.009595960
#> col_2  0.000000000 -0.028282828 -0.056565657  0.001010101  0.061616162
#> col_3 -0.003030303 -0.002828283 -0.005656566  0.015252525 -0.003939394
#> col_4  0.004040404  0.006464646 -0.007272727  0.071919192  0.084040404
#> col_5  0.024242424  0.036767677  0.023030303 -0.049292929 -0.027070707
a$cor[1:5, 1:5]
#>             col_1       col_2       col_3        col_4       col_5
#> col_1 -0.03673592  0.10578642  0.01076701 -0.085190640 -0.06910187
#> col_2  0.00000000 -0.08413000 -0.13700493  0.002669972  0.20164105
#> col_3 -0.04828045 -0.04055059 -0.06603627  0.194325607 -0.06213840
#> col_4  0.01292449  0.01860898 -0.01704631  0.183965547  0.26614737
#> col_5  0.08475540  0.11567689  0.05899774 -0.137809434 -0.09369928
```
