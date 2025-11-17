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
#> [1]  0.6516738  0.1255551         NA -0.2672207 -0.3861141
normalization(x, method = "max_min")
#> [1] 1.0000000 0.4930383 0.3720549 0.1145643 0.0000000
normalization(x, method = "maximum")
#> [1]  1.0000000  0.1926656  0.0000000 -0.4100528 -0.5924960
normalization(x, method = "sum")
#> [1]  0.45553637  0.08776617  0.00000000 -0.18679398 -0.26990347
normalization(x, method = "softmax")
#> [1] 0.60426769 0.16512479 0.12115913 0.06268898 0.04675941
normalization(x, method = "z_score")
#> [1]  1.5458123  0.2484965 -0.0611002 -0.7200192 -1.0131894
normalization(x, method = "mad")
#> [1]  1.6448875  0.3169132  0.0000000 -0.6744908 -0.9745892
normalization(x, method = "unit_vector")
#> [1]  0.8015881  0.1544384  0.0000000 -0.3286935 -0.4749377
normalization(x, method = "unit_vector", na_rm = FALSE)
#> [1]  0.8015881  0.1544384         NA -0.3286935 -0.4749377
```
