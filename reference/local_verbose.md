# Temporarily set the shared verbose switch

Set `options(log_message.verbose)` for the calling frame, then restore
the previous value. Nested
[`get_verbose()`](https://mengxu98.github.io/thisutils/reference/get_verbose.md)
calls, including compiled
[`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
code, see the temporary value.

## Usage

``` r
local_verbose(verbose)
```

## Arguments

- verbose:

  Single logical value.

## Value

`verbose`, invisibly.

## Examples

``` r
local({
  local_verbose(FALSE)
  get_verbose()
})
#> [1] FALSE
```
