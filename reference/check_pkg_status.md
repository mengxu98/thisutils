# Check if a package is installed with the specified version

Check if a package is installed with the specified version

## Usage

``` r
check_pkg_status(pkg, version = NULL, lib = .libPaths()[1])
```

## Arguments

- pkg:

  Package name.

- version:

  Package version to check. If `NULL`, only checks if the package is
  installed.

- lib:

  The location of the library directories where to install the packages.

## Value

`TRUE` if the package is installed with the specified version, `FALSE`
otherwise.
