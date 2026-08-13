# **thisutils** <img src="man/figures/logo.svg" align="right" width="120"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/thisutils)](https://CRAN.R-project.org/package=thisutils) [![Conda Version](https://img.shields.io/conda/vn/conda-forge/r-thisutils?color=green)](https://anaconda.org/conda-forge/r-thisutils) [![R-hub](https://github.com/mengxu98/thisutils/actions/workflows/rhub.yaml/badge.svg)](https://github.com/mengxu98/thisutils/actions/workflows/rhub.yaml) [![R-CMD-check](https://github.com/mengxu98/thisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mengxu98/thisutils/actions/workflows/R-CMD-check.yaml) [![test-coverage](https://github.com/mengxu98/thisutils/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/mengxu98/thisutils/actions/workflows/test-coverage.yaml) [![pkgdown](https://github.com/mengxu98/thisutils/actions/workflows/pkgdown.yaml/badge.svg)](https://mengxu98.github.io/thisutils/reference/index.html) [![RStudio CRAN mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/thisutils)](https://CRAN.R-project.org/package=thisutils)

<!-- badges: end -->

## **Introduction**

[thisutils](https://mengxu98.github.io/thisutils/) provides reliable building
blocks for research workflows: sparse-matrix conversion and top-k selection,
correlations, neighborhoods and LISI scores, repeated execution with structured
messages, and optional dependency checks — with explicit semantics and bounded
resource use.

## **Installation**

Install CRAN version:

``` r
install.packages("thisutils")
# or
if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("thisutils")
```

Install development version from [GitHub](https://github.com/mengxu98/thisutils) use [pak](https://github.com/r-lib/pak):

``` r
if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("mengxu98/thisutils")
```

## **Quick start**

``` r
library(Matrix)
library(thisutils)

x <- Matrix(
  c(-3, 0, 2, -1, 4, 0),
  nrow = 3,
  sparse = TRUE,
  dimnames = list(paste0("r", 1:3), paste0("c", 1:2))
)

# Implicit zeros for ordinary matrices; stored entries only for graphs
run_sparse_topk(x, k = 2, by = "col")
run_sparse_topk_stored(x, k = 2, by = "col")

# Blockwise correlation with a bounded dense working block
sparse_cor(simulate_sparse_matrix(200, 50), threshold = 0.2, block_size = 64)

# Repeat tasks with aligned serial/parallel results and per-input seeds
parallelize_fun(
  list(first = x, second = x),
  function(mat) thisutils::sparse_cor(mat, threshold = 0.2, block_size = 64),
  cores = 2,
  backend = "psock",
  seed = 2026
)
```

See the [function reference](https://mengxu98.github.io/thisutils/reference/index.html)
for the complete API, and run
`vignette("research-package-workflows", package = "thisutils")` for a connected
example installed with the package.
