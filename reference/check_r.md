# Check and install R packages

Check and install R packages

## Usage

``` r
check_r(
  packages,
  lib = .libPaths()[1],
  dependencies = NA,
  force = FALSE,
  install = TRUE,
  timeout = Inf,
  load = FALSE,
  cores = NULL,
  verbose = TRUE
)
```

## Arguments

- packages:

  Package to be installed. Package source can be *CRAN*, *Bioconductor*
  or *Github*. By default, the package name is extracted according to
  the `packages` parameter.

- lib:

  The location of the library directories where to install the packages.

- dependencies:

  Which dependencies to install. Passed to
  [pak::pkg_install](https://pak.r-lib.org/reference/pkg_install.html).
  Default is `NA`, auto install hard dependencies: *Depends*, *Imports*,
  and *LinkingTo*, excluding *Suggests*.

- force:

  Whether to force the installation of packages. Default is `FALSE`.

- install:

  Whether missing or outdated packages may be installed. Set to `FALSE`
  for read-only diagnostics. Default is `TRUE` for backward
  compatibility.

- timeout:

  Maximum installation time in seconds. A finite timeout runs
  installation in a supervised R subprocess and terminates only that
  process tree on timeout. Default is `Inf`.

- load:

  Whether to load packages after successful installation. Uses
  [do.call](https://rdrr.io/r/base/do.call.html) dispatch to avoid CRAN
  static checks on [base::library](https://rdrr.io/r/base/library.html).
  Default is `FALSE`.

- cores:

  Number of workers used by
  [`pak::pkg_install()`](https://pak.r-lib.org/reference/pkg_install.html).
  Use `NULL` (the default) to let pak select its worker count
  automatically.

- verbose:

  Whether to print the message. Default is `TRUE`.

## Value

Package installation status.

## Details

GitHub packages are normally installed with `pak`. If `pak` cannot parse
a GitHub package's `DESCRIPTION` file, `check_r()` retries that package
with the optional `remotes` package. This preserves the fast dependency
resolution path while supporting legacy repositories with malformed
metadata.
