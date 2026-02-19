# Unnest a list-column

Implement similar functions to the
[tidyr::unnest](https://tidyr.tidyverse.org/reference/unnest.html)
function.

## Usage

``` r
unnest_fun(data, cols, keep_empty = FALSE)
```

## Arguments

- data:

  A data frame.

- cols:

  Columns to unnest.

- keep_empty:

  By default, you get one row of output for each element of the list
  your unchopping/unnesting. This means that if there's a size-0 element
  (like `NULL` or an empty data frame), that entire row will be dropped
  from the output. If you want to preserve all rows, use
  `keep_empty = TRUE` to replace size-0 elements with a single row of
  missing values.

## Examples

``` r
data <- data.frame(
  id = 1:3,
  x = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
data$data <- list(
  c(1, 2),
  c(3, 4, 5),
  c(6)
)
unnest_fun(data, cols = "data")
#>   id x data
#> 1  1 a    1
#> 2  1 a    2
#> 3  2 b    3
#> 4  2 b    4
#> 5  2 b    5
#> 6  3 c    6

data2 <- data.frame(
  id = 1:3,
  x = c("a", "b", "c"),
  stringsAsFactors = FALSE
)
data2$data <- list(
  c(1, 2),
  numeric(0),
  c(6)
)
unnest_fun(data2, cols = "data")
#>   id x data
#> 1  1 a    1
#> 2  1 a    2
#> 3  3 c    6
unnest_fun(data2, cols = "data", keep_empty = TRUE)
#>   id x data
#> 1  1 a    1
#> 2  1 a    2
#> 3  2 b   NA
#> 4  3 c    6
```
