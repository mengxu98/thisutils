# Invoke a function with a list of arguments

Invoke a function with a list of arguments

## Usage

``` r
invoke_fun(.fn, .args = list(), ..., .env = rlang::caller_env())
```

## Arguments

- .fn:

  A function, or function name as a string.

- .args:

  A list of arguments.

- ...:

  Other arguments passed to the function.

- .env:

  Environment in which to evaluate the call. This will be most useful if
  `.fn` is a string, or the function has side-effects.

## Examples

``` r
f <- function(x, y) {
  x + y
}
invoke_fun(f, list(x = 1, y = 2))
#> [1] 3
invoke_fun("f", list(x = 1, y = 2))
#> [1] 3
invoke_fun("f", x = 1, y = 2)
#> [1] 3
```
