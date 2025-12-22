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
  timestamp_format = paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] "),
  verbose = TRUE
)
```

## Arguments

- x:

  A vector or list to apply over.

- fun:

  The function to be applied to each element.

- cores:

  The number of cores to use for parallelization with
  [foreach::foreach](https://rdrr.io/pkg/foreach/man/foreach.html).
  Default is `1`.

- export_fun:

  The functions to export the function to workers.

- clean_result:

  Whether to remove failed results from output. If `FALSE`, failed
  results are kept as error objects. Default is `FALSE`.

- throw_error:

  Whether to print detailed error information for failed results.
  Default is `TRUE`.

- timestamp_format:

  Format string for timestamp display. Default is `"%Y-%m-%d %H:%M:%S"`.

- verbose:

  Whether to print the message. Default is `TRUE`.

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
#> ℹ [2025-12-22 08:07:09] Using 1 core
#> ⠙ [2025-12-22 08:07:09] Running [1/3] Processing: 1  ETA:  0s
#> ⠹ [2025-12-22 08:07:09] Running [2/3] Processing: 2  ETA:  0s
#> ✔ [2025-12-22 08:07:09] Completed 3 tasks in 640ms
#> 
#> ℹ [2025-12-22 08:07:09] Building results
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
#> ℹ [2025-12-22 08:07:09] Using 2 cores
#> ℹ [2025-12-22 08:07:09] Building results
#> $`1`
#> [1] 1
#> 
#> $`2`
#> [1] 9
#> 
#> $`3`
#> [1] 4
#> 

# Examples with error handling
parallelize_fun(1:5, function(x) {
  if (x == 3) stop("Error on element 3")
  x^2
}, clean_result = FALSE)
#> ℹ [2025-12-22 08:07:10] Using 1 core
#> ℹ [2025-12-22 08:07:10] Building results
#> ! [2025-12-22 08:07:10] Found 1 failed result
#> ℹ [2025-12-22 08:07:10] ✖ Error details:
#> ℹ                       ✖ "3": Error on element 3
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
#> ℹ [2025-12-22 08:07:10] Using 1 core
#> ℹ [2025-12-22 08:07:10] Building results
#> ! [2025-12-22 08:07:10] Found 1 failed result
#> ℹ [2025-12-22 08:07:10] ✖ Error details:
#> ℹ                       ✖ "3": Error on element 3
#> ℹ [2025-12-22 08:07:10] Removed 1 failed result
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
#> ℹ [2025-12-22 08:07:10] Using 1 core
#> ℹ [2025-12-22 08:07:10] Building results
#> ! [2025-12-22 08:07:10] Found 2 failed results
#> ℹ [2025-12-22 08:07:10] ✖ Error details:
#> ℹ                       ✖ "2": Error on element 3
#> ℹ                       ✖ "4": Error on element 4
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
#> ℹ [2025-12-22 08:07:10] Using 1 core
#> ℹ [2025-12-22 08:07:10] Building results
#> ! [2025-12-22 08:07:10] Found 1 failed result
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
