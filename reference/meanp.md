# Mean P-value

Mean P-value

## Usage

``` r
meanp(p)
```

## Arguments

- p:

  A vector of P-values.

## Examples

``` r
p <- c(0.01, 0.02, 0.03, 0.04, 0.05)
meanp(p)
#> $z
#> [1] 3.640604
#> 
#> $p
#> [1] 0.0001359994
#> 
#> $validp
#> [1] 0.01 0.02 0.03 0.04 0.05
#> 
```
