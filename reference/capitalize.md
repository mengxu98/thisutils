# Capitalize the first letter of each word

Capitalize the first letter of each word

## Usage

``` r
capitalize(x, force_tolower = FALSE)
```

## Arguments

- x:

  A vector of character strings to be capitalized.

- force_tolower:

  Whether to force the remaining letters to be lowercase.

## Examples

``` r
x <- c(
  "hello world",
  "hello World"
)
capitalize(x)
#> [1] "Hello world" "Hello World"
```
