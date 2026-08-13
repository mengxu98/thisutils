# Research-package workflow boundaries with thisutils

`thisutils` provides a small common layer for recurring research-package
tasks. This vignette follows four boundaries: data representation,
neighborhood and metric evaluation, repeated execution, and optional
runtime dependencies.

## Preserve matrix representation and meaning

``` r

library(Matrix)
library(thisutils)

x <- Matrix(
  c(-3, 0, 2, -1, 4, 0),
  nrow = 3,
  sparse = TRUE,
  dimnames = list(paste0("r", 1:3), paste0("c", 1:2))
)

compact <- matrix_to_table(x, keep_zero = FALSE)
roundtrip <- table_to_matrix(compact, return_sparse = TRUE)
collapsed <- collapse_sparse_rows(x, c("g1", "g1", "g2"))

c(
  sparsity = check_sparsity(x),
  stored_coordinates = nrow(compact),
  collapsed_rows = nrow(collapsed),
  roundtrip_equal = isTRUE(all.equal(as.matrix(roundtrip), as.matrix(x)))
)
#>           sparsity stored_coordinates     collapsed_rows    roundtrip_equal 
#>          0.3333333          4.0000000          2.0000000          1.0000000
```

Numeric matrices and sparse graphs give unstored entries different
meanings. The two top-k interfaces make that choice visible:

``` r

run_sparse_topk(x, k = 2, by = "col")$value
#>      [,1] [,2]
#> [1,]    2    0
#> [2,]    4    0
run_sparse_topk_stored(x, k = 2, by = "col")$value
#>      [,1] [,2]
#> [1,]    2   -3
#> [2,]    4   -1
```

## Combine neighborhood and classification evaluation

The next example creates three synthetic labels in two batches. It is
small enough to run while building the vignette.

``` r

set.seed(20260810)
cell_type <- rep(c("T", "B", "Mono"), each = 40)
batch <- rep(rep(c("A", "B"), each = 20), 3)
metadata <- data.frame(batch = batch, cell_type = cell_type)
centers <- rbind(
  T = c(-3, 0, 0),
  B = c(3, 0, 0),
  Mono = c(0, 3, 0)
)
embedding <- centers[cell_type, , drop = FALSE] +
  matrix(stats::rnorm(120 * 3, sd = 0.7), ncol = 3)
```

[`compute_lisi()`](https://mengxu98.github.io/thisutils/reference/compute_lisi.md)
evaluates local label diversity, while
[`run_biocneighbors_knn()`](https://mengxu98.github.io/thisutils/reference/run_biocneighbors_knn.md)
provides a standardized neighbor result that can be fed into
[`classification_metrics_compute()`](https://mengxu98.github.io/thisutils/reference/classification_metrics_compute.md).

``` r

mixing <- compute_lisi(
  embedding,
  metadata,
  c("batch", "cell_type"),
  perplexity = 10,
  n_threads = 1,
  max_dense_bytes = 16 * 1024^2
)

neighbors <- run_biocneighbors_knn(
  embedding,
  k = 7,
  exclude_self = TRUE,
  n_threads = 1
)
predicted <- apply(neighbors$idx, 1L, function(index) {
  votes <- sort(table(metadata$cell_type[index]), decreasing = TRUE)
  names(votes)[[1L]]
})
metrics <- classification_metrics_compute(predicted, metadata$cell_type)

c(
  median_batch_lisi = median(mixing$batch),
  median_cell_type_lisi = median(mixing$cell_type),
  accuracy = metrics$accuracy,
  macro_f1 = metrics$macro_f1
)
#>     median_batch_lisi median_cell_type_lisi              accuracy 
#>              1.746606              1.000000              1.000000 
#>              macro_f1 
#>              1.000000
```

## Repeat work under an explicit execution contract

The same function can run serially or with PSOCK workers. A seed defines
one random-number stream per input, while `progress = FALSE` keeps
stable lifecycle messages without an elapsed-time display.

``` r

draw <- function(cores, verbose = FALSE) {
  parallelize_fun(
    setNames(1:4, letters[1:4]),
    function(i) stats::rnorm(2),
    cores = cores,
    backend = "psock",
    seed = 42,
    verbose = verbose,
    progress = FALSE,
    timestamp_format = ""
  )
}

serial <- draw(1)
psock <- draw(2, verbose = TRUE)
#> ℹ Using 2 cores
#> ℹ Building results
identical(serial, psock)
#> [1] TRUE
```

PSOCK tasks that reference an object from the function environment
should include that object in each task or name it in `export_fun`.

## Diagnose optional dependencies without modifying a library

``` r

status <- check_r(
  c("Matrix", "BiocNeighbors"),
  install = FALSE,
  verbose = FALSE
)
backend <- if (isTRUE(status[["BiocNeighbors"]])) {
  get_namespace_fun("BiocNeighbors", "findKNN")
} else {
  NULL
}

c(unlist(status), backend_is_function = is.function(backend))
#>              Matrix       BiocNeighbors backend_is_function 
#>               FALSE               FALSE               FALSE
```

`install = FALSE` is useful inside package workflows because
availability checking remains read-only. Installation, when
intentionally enabled, can be bounded with the `timeout` argument.
