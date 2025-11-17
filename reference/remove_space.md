# Remove and normalize spaces

Remove and normalize spaces

## Usage

``` r
remove_space(
  x,
  trim_start = TRUE,
  trim_end = FALSE,
  collapse_multiple = TRUE,
  preserve_newlines = TRUE
)
```

## Arguments

- x:

  A vector of character strings.

- trim_start:

  Whether to remove leading spaces before the first word. Default is
  `TRUE`.

- trim_end:

  Whether to remove trailing spaces after the last word. Default is
  `FALSE`.

- collapse_multiple:

  Whether to collapse multiple consecutive spaces between words into a
  single space. Default is `TRUE`.

- preserve_newlines:

  Whether to preserve newline characters when collapsing spaces. Default
  is `TRUE`.

## Value

A character vector with spaces normalized according to the specified
parameters.

## Examples

``` r
x <- c(
  " hello  world ",
  "  test   case  ",
  "no space",
  "   multiple   spaces   "
)
remove_space(x)
#> [1] "hello world "     "test case "       "no space"         "multiple spaces "
remove_space(x, trim_start = FALSE)
#> [1] " hello world "     " test case "       "no space"         
#> [4] " multiple spaces "
remove_space(x, trim_end = TRUE)
#> [1] "hello world"     "test case"       "no space"        "multiple spaces"
remove_space(x, collapse_multiple = FALSE)
#> [1] "hello  world "        "test   case  "        "no space"            
#> [4] "multiple   spaces   "
remove_space(
  x,
  trim_start = FALSE,
  trim_end = FALSE,
  collapse_multiple = FALSE
)
#> [1] " hello  world "          "  test   case  "        
#> [3] "no space"                "   multiple   spaces   "

# with newlines
multiline <- c(
  "hello\n\n  world  ",
  "  first  \n  second  "
)
remove_space(multiline)
#> [1] "hello\nworld "  "first\nsecond "
remove_space(multiline, preserve_newlines = FALSE)
#> [1] "hello world "  "first second "
```
