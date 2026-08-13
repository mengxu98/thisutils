# Resource-controlled sparse correlation

Compute correlations from matrix-like input and return a sparse matrix.
Pearson cross-products remain sparse and correlations are evaluated in
column blocks so block-local working memory is bounded independently of
the full output dimensions.

## Usage

``` r
sparse_cor(
  x,
  y = NULL,
  method = c("pearson", "spearman", "kendall"),
  allow_neg = TRUE,
  remove_na = TRUE,
  remove_inf = TRUE,
  threshold = 0,
  block_size = 256L,
  max_dense_bytes = Inf,
  max_output_entries = Inf,
  ...
)
```

## Arguments

- x:

  A numeric matrix or sparse `Matrix` with observations in rows.

- y:

  An optional numeric matrix or sparse `Matrix` with the same number of
  rows as `x`.

- method:

  Correlation coefficient: `"pearson"`, `"spearman"`, or `"kendall"`.

- allow_neg:

  Logical. Whether to allow negative values or set them to 0.

- remove_na:

  Logical. Whether to replace NA values with 0.

- remove_inf:

  Logical. Whether to replace infinite values with 0.

- threshold:

  Non-negative absolute correlation threshold. Values with absolute
  magnitude below this threshold are omitted from the sparse output.

- block_size:

  Maximum number of target columns in each Pearson working block. The
  effective size may be reduced to honor `max_dense_bytes`.

- max_dense_bytes:

  Maximum estimated bytes allowed for block-local Pearson working arrays
  or dense rank-correlation arrays. The default is `Inf` for backward
  compatibility. Supply a finite value to enable this guard.

- max_output_entries:

  Maximum number of stored values allowed in the sparse result. The
  default, `Inf`, preserves the historical unbounded output behavior;
  set a finite value to fail before returning an output that exceeds the
  workflow's storage budget.

- ...:

  Other arguments passed to
  [`stats::cor()`](https://rdrr.io/r/stats/cor.html) for Spearman and
  Kendall correlations.

## Value

A sparse correlation matrix.

## Details

Pearson input and cross-products remain sparse. Centering, scaling, and
thresholding are fused in a column-block scan rather than materializing
a dense correlation block. The final sparse result can nevertheless
contain up to `ncol(x) * ncol(y)` stored values when `threshold = 0`;
use `threshold` and `max_output_entries` to make this boundary explicit.

Spearman and Kendall correlation require ranking and currently densify
the input and full correlation result. Their estimated peak working size
is checked against `max_dense_bytes` before conversion.

## Examples

``` r
m1 <- simulate_sparse_matrix(
  500, 100
)
m2 <- simulate_sparse_matrix(
  500, 100,
  seed = 2025
)
a <- sparse_cor(m1)
b <- sparse_cor(m1, m2)
c <- as_matrix(
  cor(as_matrix(m1)),
  return_sparse = TRUE
)
d <- as_matrix(
  cor(as_matrix(m1), as_matrix(m2)),
  return_sparse = TRUE
)

a[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dsCMatrix"
#>             col_1       col_2       col_3       col_4       col_5
#> col_1  1.00000000  0.03982146  0.03290085 -0.02022058  0.00827069
#> col_2  0.03982146  1.00000000 -0.04172518 -0.00276169 -0.03594182
#> col_3  0.03290085 -0.04172518  1.00000000  0.03481704 -0.03144989
#> col_4 -0.02022058 -0.00276169  0.03481704  1.00000000 -0.05034769
#> col_5  0.00827069 -0.03594182 -0.03144989 -0.05034769  1.00000000
c[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dsCMatrix"
#>             col_1       col_2       col_3       col_4       col_5
#> col_1  1.00000000  0.03982146  0.03290085 -0.02022058  0.00827069
#> col_2  0.03982146  1.00000000 -0.04172518 -0.00276169 -0.03594182
#> col_3  0.03290085 -0.04172518  1.00000000  0.03481704 -0.03144989
#> col_4 -0.02022058 -0.00276169  0.03481704  1.00000000 -0.05034769
#> col_5  0.00827069 -0.03594182 -0.03144989 -0.05034769  1.00000000
all.equal(a, c)
#> [1] "Attributes: < Component “i”: Numeric: lengths (5046, 5050) differ >"  
#> [2] "Attributes: < Component “p”: Mean relative difference: 0.0008776063 >"
#> [3] "Attributes: < Component “x”: Numeric: lengths (5046, 5050) differ >"  

b[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dgCMatrix"
#>              col_1        col_2       col_3       col_4        col_5
#> col_1  0.003888632 -0.036322389  0.03054830  0.04990672  0.053832055
#> col_2 -0.042598718 -0.044702907  0.05688602 -0.02310432  0.007310609
#> col_3 -0.003132754 -0.041963945 -0.04085159 -0.03183016 -0.028050257
#> col_4  0.045648545  0.009914274  0.01685729  0.03317848 -0.045443563
#> col_5 -0.048082595 -0.050457664  0.04350114 -0.05551273 -0.051113224
d[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dgCMatrix"
#>              col_1        col_2       col_3       col_4        col_5
#> col_1  0.003888632 -0.036322389  0.03054830  0.04990672  0.053832055
#> col_2 -0.042598718 -0.044702907  0.05688602 -0.02310432  0.007310609
#> col_3 -0.003132754 -0.041963945 -0.04085159 -0.03183016 -0.028050257
#> col_4  0.045648545  0.009914274  0.01685729  0.03317848 -0.045443563
#> col_5 -0.048082595 -0.050457664  0.04350114 -0.05551273 -0.051113224
all.equal(b, d)
#> [1] "Attributes: < Component “i”: Numeric: lengths (9997, 10000) differ >"
#> [2] "Attributes: < Component “p”: Mean relative difference: 0.000311165 >"
#> [3] "Attributes: < Component “x”: Numeric: lengths (9997, 10000) differ >"

m1[sample(1:500, 10)] <- NA
m2[sample(1:500, 10)] <- NA

sparse_cor(m1, m2)[1:5, 1:5]
#> 5 x 5 sparse Matrix of class "dgCMatrix"
#>       col_1        col_2       col_3       col_4        col_5
#> col_1     .  .            .           .           .          
#> col_2     . -0.044702907  0.05688602 -0.02310432  0.007310609
#> col_3     . -0.041963945 -0.04085159 -0.03183016 -0.028050257
#> col_4     .  0.009914274  0.01685729  0.03317848 -0.045443563
#> col_5     . -0.050457664  0.04350114 -0.05551273 -0.051113224

system.time(
  sparse_cor(m1)
)
#>    user  system elapsed 
#>   0.003   0.000   0.003 
system.time(
  cor(as_matrix(m1))
)
#>    user  system elapsed 
#>   0.006   0.000   0.006 

system.time(
  sparse_cor(m1, m2)
)
#>    user  system elapsed 
#>   0.002   0.000   0.002 
system.time(
  cor(as_matrix(m1), as_matrix(m2))
)
#>    user  system elapsed 
#>   0.010   0.000   0.011 
```
