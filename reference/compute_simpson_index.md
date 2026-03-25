# Compute Simpson index from a KNN graph

Given nearest-neighbor distances, nearest-neighbor indices, and a
categorical label for each cell, compute the local Simpson index of each
cell after matching the target perplexity.

## Usage

``` r
compute_simpson_index(
  D,
  knn_idx,
  batch_labels,
  perplexity = 15,
  tol = 1e-05,
  max_iter = 50
)
```

## Arguments

- D:

  Numeric matrix of nearest-neighbor distances with neighbors in rows
  and cells in columns.

- knn_idx:

  Integer matrix of nearest-neighbor indices with the same shape as `D`.
  One-based indices are expected; zero-based indices are also accepted
  and converted automatically.

- batch_labels:

  Integer-like label vector of length equal to the number of cells.

- perplexity:

  Effective neighborhood size. Defaults to `15`.

- tol:

  Tolerance used in the binary search for the target perplexity.
  Defaults to `1e-5`.

- max_iter:

  Maximum number of binary-search iterations. Defaults to `50`.

## Value

A numeric vector containing the local Simpson index for each cell.

## Examples

``` r
D <- matrix(
  c(0.1, 0.2, 0.2, 0.1,
    0.3, 0.4, 0.4, 0.3),
  nrow = 2,
  byrow = TRUE
)
knn_idx <- matrix(
  c(2, 1, 4, 3,
    3, 4, 2, 1),
  nrow = 2,
  byrow = TRUE
)
batch_labels <- c(1, 1, 2, 2)
compute_simpson_index(D, knn_idx, batch_labels, perplexity = 2)
#> [1] 0.5000049 0.5000049 0.5000049 0.5000049
```
