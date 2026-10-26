# Temporarily set the shared verbose switch

Set \`options(log_message.verbose)\` for the calling frame, then restore
the previous value. Nested \`get_verbose()\` calls, including compiled
\`log_message()\` code, see the temporary value.

## Usage

``` r
local_verbose(verbose)
```

## Arguments

- verbose:

  Single logical value.

## Value

\`verbose\`, invisibly.

## Examples

``` r
local({
  local_verbose(FALSE)
  get_verbose()
})
#> [1] FALSE
```
