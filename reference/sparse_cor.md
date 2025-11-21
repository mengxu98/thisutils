# Sparse correlation function

Safe correlation function which returns a sparse matrix.

## Usage

``` r
sparse_cor(
  x,
  y = NULL,
  method = "pearson",
  allow_neg = TRUE,
  remove_na = TRUE,
  remove_inf = TRUE,
  ...
)
```

## Arguments

- x:

  Sparse matrix or character vector.

- y:

  Sparse matrix or character vector.

- method:

  Method to use for calculating the correlation coefficient.

- allow_neg:

  Logical. Whether to allow negative values or set them to 0.

- remove_na:

  Logical. Whether to replace NA values with 0.

- remove_inf:

  Logical. Whether to replace infinite values with 1.

- ...:

  Other arguments passed to
  [stats::cor](https://rdrr.io/r/stats/cor.html) function.

## Value

A correlation matrix.

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
#> [1] "Attributes: < Component “i”: Numeric: lengths (5047, 5050) differ >"  
#> [2] "Attributes: < Component “p”: Mean relative difference: 0.0008422367 >"
#> [3] "Attributes: < Component “x”: Numeric: lengths (5047, 5050) differ >"  

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
#>    0.01    0.00    0.01 
```
