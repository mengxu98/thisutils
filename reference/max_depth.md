# Maximum depth of a list

Maximum depth of a list

## Usage

``` r
max_depth(x, depth = 0)
```

## Arguments

- x:

  A list.

- depth:

  The depth of the list.

## Examples

``` r
x <- list(
  a = list(b = list(c = 1)),
  d = list(e = list(f = 2))
)
max_depth(x)
#> [1] 3
```
