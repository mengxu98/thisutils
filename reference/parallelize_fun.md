# Parallelize a function

Parallelize a function

## Usage

``` r
parallelize_fun(
  x,
  fun,
  cores = 1,
  export_fun = NULL,
  clean_result = FALSE,
  throw_error = TRUE,
  progress_bar_width = 10L,
  timestamp_format = paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] "),
  verbose = TRUE,
  backend = c("auto", "fork", "psock"),
  timeout = Inf,
  total_timeout = Inf,
  seed = NULL
)
```

## Arguments

- x:

  A vector or list to apply over.

- fun:

  The function to be applied to each element.

- cores:

  The number of worker processes to use for parallelization. Default is
  `1`.

- export_fun:

  The functions to export the function to workers.

- clean_result:

  Whether to remove failed results from output. If `FALSE`, failed
  results are kept as error objects. Default is `FALSE`.

- throw_error:

  Whether to print detailed error information for failed results.
  Default is `TRUE`.

- progress_bar_width:

  Width of the verbose progress bar in characters. Default is `10L`.

- timestamp_format:

  Format string for timestamp display. Default is `"%Y-%m-%d %H:%M:%S"`.

- verbose:

  Whether to print the message. Default is `TRUE`.

- backend:

  Parallel backend. `"auto"` uses PSOCK on every platform. Use `"fork"`
  to opt in to forked workers on supported systems. `"fork"` is
  unavailable on Windows. Prefer PSOCK in long-lived sessions that also
  use child-process managers such as callr or processx, because they can
  compete with R's fork-worker signal handler.

- timeout:

  Maximum number of seconds that a parallel worker task may run. `Inf`
  disables task timeouts. This is ignored when execution uses one core.

- total_timeout:

  Maximum number of seconds allowed for the complete call. `Inf`
  disables the overall deadline. In single-core mode the deadline is
  checked between inputs but cannot interrupt a function that is already
  running.

- seed:

  Optional integer seed. When supplied, every input receives a
  deterministic independent L'Ecuyer-CMRG random-number stream, making
  results reproducible across worker counts and scheduling order. The
  caller's random number state is restored when the call finishes.

## Value

A list of computed results. If `clean_result = FALSE`, failed results
are included as error objects. If `clean_result = TRUE`, only successful
results are returned.

## Examples

``` r
parallelize_fun(1:3, function(x) {
  Sys.sleep(0.2)
  x^2
})
#> ℹ [2026-08-05 08:05:32] Using 1 core
#> ⠙ [2026-08-05 08:05:32] Running for 1 [1/3] ■■■         33% | ETA:  0s
#> ✔ [2026-08-05 08:05:32] Completed 3 tasks in 630ms
#> 
#> ℹ [2026-08-05 08:05:32] Building results
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 4
#> 
#> $`3`
#> [1] 9
#> 

parallelize_fun(list(1, 2, 3), function(x) {
  Sys.sleep(0.2)
  x^2
}, cores = 2)
#> ℹ [2026-08-05 08:05:33] Using 2 cores
#> ⠙ [2026-08-05 08:05:33] Running for [1/3] ■■■         33% | ETA:  2s
#> ✔ [2026-08-05 08:05:33] Completed 3 tasks in 1.4s
#> 
#> ℹ [2026-08-05 08:05:33] Building results
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 4
#> 
#> [[3]]
#> [1] 9
#> 

# Examples with error handling
parallelize_fun(1:5, function(x) {
  if (x == 3) stop("Error on element 3")
  x^2
}, clean_result = FALSE)
#> ℹ [2026-08-05 08:05:34] Using 1 core
#> ℹ [2026-08-05 08:05:34] Building results
#> ! [2026-08-05 08:05:34] Found 1 failed result
#> Warning: Found 1 failed result
#> ℹ [2026-08-05 08:05:34] ✖ Error details:
#> ℹ                       ✖ Error on element 3 (1): "3"
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 4
#> 
#> $`3`
#> $error
#> [1] "Error on element 3"
#> 
#> $index
#> [1] 3
#> 
#> $input
#> [1] 3
#> 
#> attr(,"class")
#> [1] "parallelize_error"
#> 
#> $`4`
#> [1] 16
#> 
#> $`5`
#> [1] 25
#> 

parallelize_fun(1:5, function(x) {
  if (x == 3) stop("Error on element 3")
  x^2
}, clean_result = TRUE)
#> ℹ [2026-08-05 08:05:34] Using 1 core
#> ℹ [2026-08-05 08:05:34] Building results
#> ! [2026-08-05 08:05:34] Found 1 failed result
#> Warning: Found 1 failed result
#> ℹ [2026-08-05 08:05:34] ✖ Error details:
#> ℹ                       ✖ Error on element 3 (1): "3"
#> ℹ [2026-08-05 08:05:34] Removed 1 failed result
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 4
#> 
#> $`4`
#> [1] 16
#> 
#> $`5`
#> [1] 25
#> 

# Control error printing
parallelize_fun(1:5, function(x) {
  if (x == 2) stop("Error on element 3")
  if (x == 4) stop("Error on element 4")
  x^2
})
#> ℹ [2026-08-05 08:05:34] Using 1 core
#> ℹ [2026-08-05 08:05:34] Building results
#> ! [2026-08-05 08:05:34] Found 2 failed results
#> Warning: Found 2 failed results
#> ℹ [2026-08-05 08:05:34] ✖ Error details:
#> ℹ                       ✖ Error on element 3 (1): "2"
#> ℹ                       ✖ Error on element 4 (1): "4"
#> $`1`
#> [1] 1
#> 
#> $`2`
#> $error
#> [1] "Error on element 3"
#> 
#> $index
#> [1] 2
#> 
#> $input
#> [1] 2
#> 
#> attr(,"class")
#> [1] "parallelize_error"
#> 
#> $`3`
#> [1] 9
#> 
#> $`4`
#> $error
#> [1] "Error on element 4"
#> 
#> $index
#> [1] 4
#> 
#> $input
#> [1] 4
#> 
#> attr(,"class")
#> [1] "parallelize_error"
#> 
#> $`5`
#> [1] 25
#> 

parallelize_fun(1:5, function(x) {
  if (x == 3) stop("Error on element 3")
  x^2
}, throw_error = FALSE)
#> ℹ [2026-08-05 08:05:35] Using 1 core
#> ℹ [2026-08-05 08:05:35] Building results
#> ! [2026-08-05 08:05:35] Found 1 failed result
#> Warning: Found 1 failed result
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 4
#> 
#> $`3`
#> $error
#> [1] "Error on element 3"
#> 
#> $index
#> [1] 3
#> 
#> $input
#> [1] 3
#> 
#> attr(,"class")
#> [1] "parallelize_error"
#> 
#> $`4`
#> [1] 16
#> 
#> $`5`
#> [1] 25
#> 
```
