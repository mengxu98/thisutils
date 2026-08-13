# **thisutils**

## **Introduction**

[thisutils](https://mengxu98.github.io/thisutils/) provides reliable
building blocks shared by research packages across data representation,
numerical and neighborhood evaluation, repeated execution, structured
messages, and optional runtime dependencies. Its core design goals are
explicit semantics, bounded resource use, stable result schemas, and
controllable side effects.

The main functionality is organized around four connected layers:

- preserving sparse-matrix meaning during conversion, aggregation, and
  top-k selection;
- computing stable matrix summaries, blockwise correlations, exact
  neighborhoods, LISI scores, and standardized evaluation metrics;
- repeating tasks with structured messages and aligned serial/parallel
  result, error, random-number, timeout, and cleanup behavior; and
- checking optional dependencies and namespaces without requiring an
  installation side effect.

Additional statistical, text, and presentation helpers remain available,
but they are not presented as new statistical methods.

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

Install development version from
[GitHub](https://github.com/mengxu98/thisutils) use
[pak](https://github.com/r-lib/pak):

``` r

if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("mengxu98/thisutils")
```

## **Core usage**

The workflow can begin with explicit representation conversion and
aggregation:

``` r

library(Matrix)
library(thisutils)

graph <- sparseMatrix(
  i = c(1, 3, 2), j = c(1, 1, 2), x = c(1, 3, -2),
  dims = c(3, 2)
)
compact <- matrix_to_table(graph, keep_zero = FALSE)
roundtrip <- table_to_matrix(compact, return_sparse = TRUE)
collapsed <- collapse_sparse_rows(graph, c("group1", "group1", "group2"))
```

Sparse and dense top-k operations use ordinary matrix semantics by
default. For adjacency matrices, where only stored entries represent
graph edges, use the explicitly named stored-entry helper:

``` r

run_sparse_topk(graph, k = 2, by = "col")
run_sparse_topk_stored(graph, k = 2, by = "col")
```

Correlation and neighborhood computations expose their resource
boundaries:

``` r

x <- simulate_sparse_matrix(500, 100)
correlation <- sparse_cor(
  x,
  threshold = 0.2,
  block_size = 64,
  max_dense_bytes = 256 * 1024^2
)

embedding <- matrix(rnorm(1000), ncol = 10)
metadata <- data.frame(batch = rep(c("A", "B"), each = 50))
lisi <- compute_lisi(
  embedding,
  metadata,
  "batch",
  perplexity = 10,
  knn_algorithm = "auto",
  n_threads = 1
)
```

Neighborhood results and labels can be summarized through fixed output
schemas, while optional dependencies can be inspected without modifying
the user library:

``` r

neighbors <- run_biocneighbors_knn(
  embedding,
  k = 7,
  exclude_self = TRUE,
  n_threads = 1
)
predicted <- apply(neighbors$idx, 1L, function(index) {
  names(sort(table(metadata$batch[index]), decreasing = TRUE))[[1L]]
})
metrics <- classification_metrics_compute(predicted, metadata$batch)

dependency_status <- check_r(
  c("Matrix", "BiocNeighbors"),
  install = FALSE,
  verbose = FALSE
)
```

Independent sparse workflows can then use the same execution contract in
serial or PSOCK mode. A per-input seed makes stochastic results
independent of the requested worker count:

``` r

jobs <- list(first = x, second = x[, rev(seq_len(ncol(x)))])
result <- parallelize_fun(
  jobs,
  function(mat) thisutils::sparse_cor(
    mat,
    threshold = 0.2,
    block_size = 64,
    max_dense_bytes = 256 * 1024^2
  ),
  cores = 2,
  backend = "psock",
  seed = 2026,
  verbose = TRUE,
  progress = FALSE,
  timestamp_format = ""
)
```

See the [function
reference](https://mengxu98.github.io/thisutils/reference/index.html)
for the complete API. Run
[`vignette("research-package-workflows", package = "thisutils")`](https://mengxu98.github.io/thisutils/articles/research-package-workflows.md)
for a connected example installed with the package.
