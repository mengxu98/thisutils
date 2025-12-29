# Try to evaluate an expression a set number of times before failing

The function is used as a fail-safe if code sometimes works and
sometimes doesn't, usually because it depends on a resource that may be
temporarily unavailable. It tries to evaluate the expression `max_tries`
times. If all the attempts fail, it throws an error; if not, the
evaluated expression is returned.

## Usage

``` r
try_get(expr, max_tries = 5, error_message = "", retry_message = "Retrying...")
```

## Arguments

- expr:

  The expression to be evaluated.

- max_tries:

  The maximum number of attempts to evaluate the expression before
  giving up. Default is `5`.

- error_message:

  Additional custom error message to be displayed when an error occurs.

- retry_message:

  Message displayed when a new try to evaluate the expression would be
  attempted.

## Value

The evaluated expression if successful, otherwise it throws an error if
all attempts are unsuccessful.

## Examples

``` r
f <- function() {
  value <- runif(1, min = 0, max = 1)
  if (value > 0.5) {
    log_message("value is larger than 0.5")
    return(value)
  } else {
    log_message(
      "value is smaller than 0.5",
      message_type = "error"
    )
  }
}
f_evaluated <- try_get(expr = f())
#> ℹ [2025-12-29 08:15:41] Value is larger than 0.5
print(f_evaluated)
#> [1] 0.7176185
```
