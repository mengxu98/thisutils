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

  A list or pairlist of argument values, or NULL.

- ...:

  Other arguments passed to the function.

- .env:

  Evaluation environment for calls without arguments, or the parent of
  the temporary argument environment for calls with arguments. Captured
  expressions and caller-frame side effects use that temporary
  environment.

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
