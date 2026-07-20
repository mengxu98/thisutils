# Find nearest neighbors with BiocNeighbors

Find nearest neighbors within a reference matrix or from a query matrix
to a reference matrix. The wrapper standardizes matrix coercion,
distance metric, and self-neighbor removal across consumers.

## Usage

``` r
run_biocneighbors_knn(
  reference,
  query = NULL,
  k,
  metric = c("euclidean", "cosine"),
  exclude_self = FALSE,
  n_threads = 1L
)
```

## Arguments

- reference:

  Numeric reference matrix with observations in rows.

- query:

  Optional numeric query matrix with the same number of columns.

- k:

  Number of nearest neighbors to return.

- metric:

  Distance metric: \`"euclidean"\` or \`"cosine"\`.

- exclude_self:

  Whether to remove self-neighbors when \`query\` is \`NULL\`.

- n_threads:

  Number of BiocNeighbors threads.

## Value

A list with integer matrix \`idx\` and numeric matrix \`dist\`.

## Examples

``` r
if (requireNamespace("BiocNeighbors", quietly = TRUE)) {
  run_biocneighbors_knn(matrix(rnorm(20), ncol = 2), k = 2)
}
#> $idx
#>       [,1] [,2]
#>  [1,]    8   10
#>  [2,]    4   10
#>  [3,]    1    8
#>  [4,]   10    1
#>  [5,]    9   10
#>  [6,]    8    7
#>  [7,]    8    4
#>  [8,]    1    7
#>  [9,]    5   10
#> [10,]    4    9
#> 
#> $dist
#>            [,1]      [,2]
#>  [1,] 0.5552220 0.7201013
#>  [2,] 1.1336344 1.2341433
#>  [3,] 1.3177911 1.4062494
#>  [4,] 0.5887888 0.9649954
#>  [5,] 0.3267387 0.8255953
#>  [6,] 1.3493651 1.5473976
#>  [7,] 1.0168836 1.1886890
#>  [8,] 0.5552220 1.0168836
#>  [9,] 0.3267387 0.6488308
#> [10,] 0.5887888 0.6488308
#> 
```
