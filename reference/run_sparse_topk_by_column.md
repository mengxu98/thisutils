# Compatibility wrapper for column-wise sparse top-k selection

`run_sparse_topk_by_column()` preserves the column-wise interface used
in earlier releases: only explicitly stored entries participate, and
missing ranks are padded with `NA` indices and zero values. New code
should call
[`run_sparse_topk()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk.md)
or
[`run_sparse_topk_stored()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_stored.md)
to select the intended sparse-zero semantics explicitly.

## Usage

``` r
run_sparse_topk_by_column(x, k, decreasing = TRUE)
```

## Arguments

- x:

  A `dgCMatrix` (or something coercible to one).

- k:

  Number of stored entries to retain per column.

- decreasing:

  Whether to sort in decreasing order. Defaults to `TRUE`.

## Value

A list with `idx` and `value` matrices. Columns with fewer than `k`
stored entries use `NA` indices and zero values for the remaining ranks.
