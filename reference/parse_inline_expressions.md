# Parse inline expressions

Parse \`\` inline expressions and evaluate them in the current
environment, while preserving outer formatting markers like \`{.val
...}\`.

## Usage

``` r
parse_inline_expressions(text, env = parent.frame())
```

## Arguments

- text:

  A character string containing inline expressions to parse.

- env:

  Environment in which to evaluate expressions. Defaults to the calling
  environment.

## Value

A character string with expressions evaluated but formatting preserved.

## Examples

``` r
i <- 1
parse_inline_expressions(
  "{.val {i}}"
)
#> [1] "{.val 1}"

x <- 5
y <- 10
parse_inline_expressions(
  "{.pkg {x + y}}"
)
#> [1] "{.pkg 15}"

name <- "testing"
name <- parse_inline_expressions(
  "{.pkg {name}}"
)
name
#> [1] "{.pkg testing}"

log_message(name)
#> ℹ [2026-01-09 07:01:55] testing
```
