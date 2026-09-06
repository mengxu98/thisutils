# Empty-value selection operator

Return the left side unless it is `NULL` or has length zero, otherwise
return the right side. Use `%ss%` when only `NULL` should fall through;
that matches the rlang `%||%` operator without masking it.

## Usage

``` r
a %|||% b
```

## Arguments

- a:

  The left side value to check.

- b:

  The right side value to use if `a` is `NULL` or length zero.

## Value

`a` if it is not `NULL` and has length greater than zero, otherwise `b`.

## Examples

``` r
NULL %|||% 10
#> [1] 10
character(0) %|||% "fallback"
#> [1] "fallback"
5 %|||% 10
#> [1] 5
```
