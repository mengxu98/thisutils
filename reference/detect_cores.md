# Detect the number of CPU cores

Detect the number of CPU cores

## Usage

``` r
detect_cores(
  cores = NULL,
  num_session = NULL,
  max_threads = NULL,
  logical = FALSE
)
```

## Arguments

- cores:

  Requested number of workers. Only used when `num_session` is supplied;
  invalid values fall back to `1`.

- num_session:

  Number of tasks that can be scheduled concurrently. When supplied, the
  result is the usable worker count: the detected cores minus one
  reserved for the parent, capped by `cores` and `num_session`.

- max_threads:

  Optional upper bound for the detected core count, e.g. the largest
  worker count a kernel can use. `NULL` leaves the detected value as is.

- logical:

  Whether to report logical (hyper-threaded) cores, as
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  does.

## Value

A single integer, at least `1`. A failed or unusable probe falls back to
`1` instead of propagating `NA`.

## Examples

``` r
detect_cores()
#> [1] 4
detect_cores(max_threads = 2)
#> [1] 2
detect_cores(cores = 4, num_session = 8)
#> [1] 3
```
