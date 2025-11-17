# Maximum P-value

Maximum P-value

## Usage

``` r
maximump(p, alpha = 0.05, log.p = FALSE)
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
maximump(p)
#> $p
#> [1] 3.125e-07
#> 
#> $pr
#> [1] 0.05
#> 
#> $r
#> [1] 5
#> 
#> $critp
#> [1] 0.5492803
#> 
#> $alpha
#> [1] 0.05
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
maximump(p, alpha = 0.01)
#> $p
#> [1] 3.125e-07
#> 
#> $pr
#> [1] 0.05
#> 
#> $r
#> [1] 5
#> 
#> $critp
#> [1] 0.3981072
#> 
#> $alpha
#> [1] 0.01
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
maximump(p, log.p = TRUE)
#> $p
#> [1] -14.97866
#> 
#> $pr
#> [1] 0.05
#> 
#> $r
#> [1] 5
#> 
#> $critp
#> [1] 0.5492803
#> 
#> $alpha
#> [1] 0.05
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
```
