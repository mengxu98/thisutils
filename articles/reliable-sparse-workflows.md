# Semantics-aware sparse research workflows

Research packages often move the same data through sparse matrices,
tabular representations, neighborhood summaries, and parallel workers.
These transitions are easy to write but difficult to make predictable:
an unstored sparse entry may mean the numeric value zero or the absence
of an edge, dense intermediates may dominate memory, and implicit worker
selection may make the same analysis behave differently across machines.
This vignette demonstrates the four connected contracts used by
`thisutils`: semantic, numerical, resource, and execution contracts.

## Declare the meaning of sparse zeros

For an ordinary numeric sparse matrix, implicit entries are zeros. The
default top-k helper therefore compares stored values and implicit zeros
together.

``` r

x <- Matrix(
  c(-3, 0, 2,
    -1, 4, 0),
  nrow = 3,
  sparse = TRUE,
  dimnames = list(paste0("r", 1:3), paste0("c", 1:2))
)

run_sparse_topk(x, k = 2, by = "col")
#> $idx
#>      [,1] [,2]
#> [1,]    3    2
#> [2,]    2    3
#> 
#> $value
#>      [,1] [,2]
#> [1,]    2    0
#> [2,]    4    0
```

Set `by = "row"` when rows, rather than columns, define the independent
selection groups.

For a graph, however, an unstored entry normally means that no edge
exists. Use the stored-entry variant to express that different contract.

``` r

run_sparse_topk_stored(x, k = 2, by = "col")
#> $idx
#>      [,1] [,2]
#> [1,]    3    1
#> [2,]    2    1
#> 
#> $value
#>      [,1] [,2]
#> [1,]    2   -3
#> [2,]    4   -1
```

Sparse-to-table conversion retains its historical expanded default.
Compact stored-entry output is available explicitly and avoids
materializing implicit zeros.

``` r

compact <- matrix_to_table(x, keep_zero = FALSE)
expanded <- matrix_to_table(x)
c(compact_rows = nrow(compact), expanded_rows = nrow(expanded))
#>  compact_rows expanded_rows 
#>             4             6
```

## Use stable summaries

The row-variance implementation retains the established fast calculation
and uses a stable two-pass fallback for rows at risk of cancellation.
This avoids the failure that can occur when small variations are added
to a large offset without slowing ordinary inputs materially.

``` r

offset <- matrix(1e12 + c(0, 1, 2, 3, 0, 1, 2, 3), nrow = 2, byrow = TRUE)
fast_row_vars(offset)
#> [1] 1.666667 1.666667
apply(offset, 1, stats::var)
#> [1] 1.666667 1.666667
```

## Bound dense correlation work

Pearson correlation is evaluated in column blocks. `max_dense_bytes`
bounds the dense working block, while `threshold` controls output
sparsity. A finite `max_output_entries` can be supplied when a workflow
also needs a hard output budget.

``` r

set.seed(1)
a <- rsparsematrix(100, 20, density = 0.1)
b <- rsparsematrix(100, 8, density = 0.1)

cors <- sparse_cor(
  a,
  b,
  threshold = 0.15,
  block_size = 4,
  max_dense_bytes = 1024^2,
  max_output_entries = 200
)
cors
#> 20 x 8 sparse Matrix of class "dgCMatrix"
#>                                                                      
#>  [1,]  0.2064136 .         .          .          .          0.2037466
#>  [2,]  .         .         .          .          .          .        
#>  [3,]  0.1812852 .         .          .          .          .        
#>  [4,]  .         .         .          .          .          .        
#>  [5,]  .         .         .          .          .          .        
#>  [6,]  .         .         .          .          .          .        
#>  [7,]  .         .         .          .          .          .        
#>  [8,]  0.2881038 0.3290310 0.1745566  .          .          .        
#>  [9,]  .         .         .          .          .          .        
#> [10,]  .         0.2541087 .         -0.1573894  .          .        
#> [11,]  .         .         .          .          .          .        
#> [12,]  .         .         .          .          .          .        
#> [13,]  .         .         .          .          .          .        
#> [14,]  .         .         .          .          .         -0.1619997
#> [15,]  .         .         .          .          .          .        
#> [16,]  .         .         .          .          .          0.2194787
#> [17,] -0.2028482 0.1529303 .          .          .          0.2173623
#> [18,]  .         .         .          .          .          .        
#> [19,]  .         .         .          .          .         -0.1805932
#> [20,]  .         .         .          .         -0.1951721  .        
#>                            
#>  [1,] -0.3213222  .        
#>  [2,]  .         -0.2631793
#>  [3,]  .          .        
#>  [4,]  .          .        
#>  [5,]  .          .        
#>  [6,]  .          .        
#>  [7,]  .          .        
#>  [8,] -0.5026507  .        
#>  [9,]  .          .        
#> [10,]  .          .        
#> [11,]  .          .        
#> [12,]  .         -0.1951403
#> [13,]  .          .        
#> [14,]  .          .        
#> [15,]  .          .        
#> [16,]  .          .        
#> [17,]  .          .        
#> [18,]  .         -0.2298732
#> [19,]  .          .        
#> [20,]  .          .
```

Spearman and Kendall correlation currently require dense ranking and
result matrices. The same memory argument is checked before conversion,
so this boundary fails explicitly rather than depending only on
available machine memory.

## Align serial and parallel execution

The resource-bounded operation can be applied to several datasets
without changing its result contract.
[`parallelize_fun()`](https://mengxu98.github.io/thisutils/reference/parallelize_fun.md)
preserves input order and names, keeps `NULL` results, and represents
task failures consistently in serial and parallel modes. PSOCK is the
cross-platform default; selecting the backend explicitly makes the
example independent of platform-specific worker selection.

``` r

datasets <- list(first = a, second = b)
correlate <- function(mat) {
  thisutils::sparse_cor(
    mat,
    threshold = 0.15,
    block_size = 4,
    max_dense_bytes = 1024^2,
    max_output_entries = 400
  )
}

serial <- parallelize_fun(
  datasets,
  correlate,
  cores = 1,
  seed = 2026,
  verbose = FALSE
)
psock <- parallelize_fun(
  datasets,
  correlate,
  cores = 2,
  backend = "psock",
  seed = 2026,
  verbose = TRUE,
  progress = FALSE,
  timestamp_format = ""
)
#> ℹ Using 2 cores
#> ℹ Building results

vapply(
  Map(function(lhs, rhs) isTRUE(all.equal(lhs, rhs)), serial, psock),
  identity,
  logical(1)
)
#>  first second 
#>   TRUE   TRUE
```

The seed is assigned per input rather than per worker, so stochastic
tasks are also reproducible across worker counts and scheduling order.

``` r

draw <- function(cores) {
  parallelize_fun(
    1:4,
    function(i) stats::rnorm(2),
    cores = cores,
    backend = "psock",
    seed = 42,
    verbose = FALSE
  )
}

identical(draw(1), draw(2))
#> [1] TRUE
```

Per-task and total timeouts provide lifecycle bounds for multi-core
calls, and nested calls automatically run their inner level serially.
Explicit fork workers remain available on Unix-like systems, but are
opt-in because R’s fork signal handler can conflict with child-process
managers such as `callr` and `processx` in long-lived sessions.

## Scope

Together, the four contracts distinguish what sparse data mean, how
summaries are calculated, which allocations are permitted, and how
independent tasks are executed. They do not define a
distributed-computing framework: Spearman and Kendall correlation still
require dense ranking, output size must be bounded separately from
working memory, and PSOCK worker startup can outweigh parallel gains for
short tasks.
