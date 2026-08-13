# Compatibility wrapper for stored column-wise sparse top-k selection

`run_sparse_stored_topk_by_column()` preserves the stored-entry
column-wise interface used in earlier releases. New code should call
[`run_sparse_topk_stored()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_stored.md)
with `by = "col"`.

## Usage

``` r
run_sparse_stored_topk_by_column(x, k, decreasing = TRUE)
```

## Arguments

- x:

  A `dgCMatrix` (or something coercible to one).

- k:

  Number of top entries to retain per column or row. Must be a positive
  integer.

- decreasing:

  Whether to sort in decreasing order (largest values first). Default is
  `TRUE`.

## Value

A list with `idx` and `value` matrices as described in
[`run_sparse_topk_stored()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_stored.md).
