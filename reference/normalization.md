# Normalize numeric vector

Normalize numeric vector

## Usage

``` r
normalization(x, method = "max_min", na_rm = TRUE, ...)
```

## Arguments

- x:

  Input numeric vector.

- method:

  Method used for normalization.

- na_rm:

  Whether to remove `NA` values, and if setting `TRUE`, using `0`
  instead. Default is `TRUE`.

- ...:

  Parameters for other methods.

## Value

Normalized numeric vector.

## Examples

``` r
x <- c(runif(2), NA, -runif(2))
x
#> [1]  0.6870228  0.3841037         NA -0.7698414 -0.4976992
normalization(x, method = "max_min")
#> [1] 1.0000000 0.7920746 0.5284236 0.0000000 0.1868000
normalization(x, method = "maximum")
#> [1]  0.8924213  0.4989388  0.0000000 -1.0000000 -0.6464958
normalization(x, method = "sum")
#> [1]  0.2937668  0.1642404  0.0000000 -0.3291795 -0.2128132
normalization(x, method = "softmax")
#> [1] 0.46431580 0.28085023 0.14846350 0.04137441 0.06499606
normalization(x, method = "z_score")
#> [1]  1.2054220  0.7026786  0.0651962 -1.2124804 -0.7608163
normalization(x, method = "mad")
#> [1]  0.9310654  0.5205441  0.0000000 -1.0433026 -0.6744908
normalization(x, method = "unit_vector")
#> [1]  0.5686043  0.3178978  0.0000000 -0.6371479 -0.4119134
normalization(x, method = "unit_vector", na_rm = FALSE)
#> [1]  0.5686043  0.3178978         NA -0.6371479 -0.4119134
```
