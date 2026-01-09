# Vote P-value

Vote P-value

## Usage

``` r
votep(p, alpha = 0.5)
```

## Arguments

- p:

  A vector of P-values.

- alpha:

  The significance level.

## Examples

``` r
p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
votep(p)
#> $p
#> [1] 0.03125
#> 
#> $pos
#> [1] 5
#> 
#> $neg
#> [1] 0
#> 
#> $alpha
#> [1] 0.5
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
votep(p, alpha = 0.01)
#> ! [2026-01-09 07:01:58] All P-values are within specified limits of alpha
#> $p
#> [1] 1
#> 
#> $pos
#> [1] 0
#> 
#> $neg
#> [1] 0
#> 
#> $alpha
#> [1] 0.01
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
```
