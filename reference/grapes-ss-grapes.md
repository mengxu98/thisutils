# Value selection operator

This operator returns the left side if it's not `NULL`, otherwise it
returns the right side.

## Usage

``` r
a %ss% b
```

## Arguments

- a:

  The left side value to check.

- b:

  The right side value to use if `a` is `NULL`.

## Value

`a` if it is not `NULL`, otherwise `b`.

## Examples

``` r
NULL %ss% 10
#> [1] 10
5 %ss% 10
#> [1] 5
```
