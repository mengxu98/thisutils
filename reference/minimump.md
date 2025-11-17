# Minimum P-value

Minimum P-value

## Usage

``` r
minimump(p, alpha = 0.05, log.p = FALSE)
```

## Arguments

- p:

  A vector of P-values.

- alpha:

  The significance level.

- log.p:

  Whether to return the log of the P-value.

## Examples

``` r
p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
minimump(p)
#> $p
#> [1] 0.04900995
#> 
#> $pr
#> [1] 0.01
#> 
#> $r
#> [1] 1
#> 
#> $critp
#> [1] 0.01020622
#> 
#> $alpha
#> [1] 0.05
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
minimump(p, alpha = 0.01)
#> $p
#> [1] 0.04900995
#> 
#> $pr
#> [1] 0.01
#> 
#> $r
#> [1] 1
#> 
#> $critp
#> [1] 0.002008048
#> 
#> $alpha
#> [1] 0.01
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
minimump(p, log.p = TRUE)
#> $p
#> [1] -3.015732
#> 
#> $pr
#> [1] 0.01
#> 
#> $r
#> [1] 1
#> 
#> $critp
#> [1] 0.01020622
#> 
#> $alpha
#> [1] 0.05
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
```
