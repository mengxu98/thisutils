# Compute Local Inverse Simpson's Index (LISI)

Compute per-cell Local Inverse Simpson's Index (LISI) scores for one or
more categorical variables. This is a clean-room reimplementation of the
`immunogenomics/LISI`.

## Usage

``` r
compute_lisi(
  X,
  meta_data,
  label_colnames,
  perplexity = 30,
  nn_eps = 0,
  use_rann = TRUE,
  nn_method = c("auto", "rann", "fnn", "hnsw", "exact"),
  nn_threads = 0,
  hnsw_ef = 100,
  tol = 1e-05,
  max_iter = 50
)
```

## Arguments

- X:

  A matrix-like object with cells in rows and embedding/features in
  columns.

- meta_data:

  A data frame with one row per cell.

- label_colnames:

  Character vector of column names in `meta_data` to evaluate.

- perplexity:

  Effective neighborhood size. Defaults to `30`.

- nn_eps:

  Approximation factor passed to
  [RANN::nn2](https://jefferislab.github.io/RANN/reference/nn2.html)
  when `RANN` is available and `use_rann = TRUE`. Defaults to `0`.

- use_rann:

  Whether to prefer
  [RANN::nn2](https://jefferislab.github.io/RANN/reference/nn2.html)
  over [`FNN::get.knn`](https://rdrr.io/pkg/FNN/man/get.knn.html) when
  `nn_method = "auto"` decides not to use the package's built-in exact
  C++ backend. Defaults to `TRUE`.

- nn_method:

  Nearest-neighbor backend. Defaults to `"auto"`, which uses a simple
  heuristic: low-dimensional inputs use the package's exact C++ search,
  while larger/higher-dimensional inputs fall back to `RANN`, then
  `FNN`, then the built-in exact C++ backend. Set to `"hnsw"` to use
  `RcppHNSW` for a faster, approximate search.

- nn_threads:

  Number of threads used by the HNSW backend. Defaults to `0` which lets
  `RcppHNSW` choose automatically.

- hnsw_ef:

  Search breadth used by the HNSW backend. Larger values are more
  accurate but slower. Defaults to `100`.

- tol:

  Tolerance used in the binary search for the target perplexity.
  Defaults to `1e-5`.

- max_iter:

  Maximum number of binary-search iterations. Defaults to `50`.

## Value

A data frame with one row per cell and one column per label.

## References

Korsunsky I, Millard N, Fan J, et al. Fast, sensitive and accurate
integration of single-cell data with Harmony. *Nature Methods* (2019).
<https://www.nature.com/articles/s41592-019-0619-0>

LISI reference implementation: <https://github.com/immunogenomics/LISI>

## Examples

``` r
set.seed(1)
X <- rbind(
  matrix(stats::rnorm(100, mean = -2), ncol = 5),
  matrix(stats::rnorm(100, mean = 2), ncol = 5)
)
meta_data <- data.frame(
  batch = rep(c("A", "B"), each = 20),
  group = sample(c("g1", "g2"), 40, replace = TRUE)
)

res <- compute_lisi(
  X, meta_data,
  c("batch", "group"),
  perplexity = 10
)
#> ◌ [2026-04-01 14:12:22] Using "exact" nearest-neighbor backend for compute_lisi
head(res)
#>      batch    group
#> 1 1.001837 1.490901
#> 2 1.000010 1.439122
#> 3 1.001436 1.915155
#> 4 1.000065 1.644454
#> 5 1.000029 1.501060
#> 6 1.000000 1.560663
boxplot(res)
```
