# Check and install R packages

Check and install R packages

## Usage

``` r
check_r(
  packages,
  lib = .libPaths()[1],
  dependencies = NA,
  force = FALSE,
  load = FALSE,
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

- load:

  Whether to load packages after successful installation. Uses
  [do.call](https://rdrr.io/r/base/do.call.html) dispatch to avoid CRAN
  static checks on [base::library](https://rdrr.io/r/base/library.html).
  Default is `FALSE`.

- verbose:

  Whether to print the message. Default is `TRUE`.

## Value

Package installation status.
