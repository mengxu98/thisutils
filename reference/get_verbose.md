# Get the verbose option

Get the verbose option from the global options or the local argument.

## Usage

``` r
get_verbose(verbose = NULL)
```

## Arguments

- verbose:

  The verbose option. Default is \`NULL\`, which means to get the
  verbose option from the global options.

## Value

The verbose option.

## Examples

``` r
get_verbose()
#> [1] TRUE
get_verbose(verbose = FALSE)
#> [1] FALSE
get_verbose(verbose = TRUE)
#> [1] TRUE

options(log_message.verbose = FALSE)
get_verbose()
#> [1] FALSE
get_verbose(verbose = TRUE)
#> [1] FALSE

options(log_message.verbose = TRUE)
get_verbose()
#> [1] TRUE

options(log_message.verbose = NULL)
```
