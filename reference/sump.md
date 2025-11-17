# Sum P-value

Sum P-value

## Usage

``` r
sump(p)
```

## Arguments

- p:

  A vector of P-values.

## Examples

``` r
p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
sump(p)
#> $p
#> [1] 6.328125e-07
#> 
#> $conservativep
#> [1] 6.328125e-07
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
```
