# Switch table to matrix

Switch table to matrix

## Usage

``` r
table_to_matrix(
  table,
  row_names = NULL,
  col_names = NULL,
  threshold = 0,
  return_sparse = FALSE
)
```

## Arguments

- table:

  A table with three columns: `row`, `col`, and `value`.

- row_names:

  Character vector of row names to filter by.

- col_names:

  Character vector of column names to filter by.

- threshold:

  The threshold for filtering values based on absolute values. Defaults
  to `0`.

- return_sparse:

  Whether to return a sparse matrix. Defaults to `false`.

## Value

A matrix.

## See also

[matrix_to_table](https://mengxu98.github.io/thisutils/reference/matrix_to_table.md)

## Examples

``` r
table <- data.frame(
  row = c("r1", "r2", "r3", "r4", "r5", "r6"),
  col = c("c4", "c5", "c6", "c1", "c2", "c3"),
  value = c(0.6, -0.5, -0.4, 0.3, 0.2, 0.1)
)
matrix <- table_to_matrix(table)
table_new <- matrix_to_table(matrix)
identical(table, table_new)
#> [1] FALSE

table_to_matrix(table, threshold = 0.3)
#>     c1 c2 c3  c4   c5   c6
#> r1 0.0  0  0 0.6  0.0  0.0
#> r2 0.0  0  0 0.0 -0.5  0.0
#> r3 0.0  0  0 0.0  0.0 -0.4
#> r4 0.3  0  0 0.0  0.0  0.0
#> r5 0.0  0  0 0.0  0.0  0.0
#> r6 0.0  0  0 0.0  0.0  0.0

table_to_matrix(
  table,
  row_names = c("r1", "r2"),
  col_names = c("c4", "c5")
)
#>     c4   c5
#> r1 0.6  0.0
#> r2 0.0 -0.5

sparse_matrix <- simulate_sparse_matrix(10, 10)
table_sparse <- matrix_to_table(
  sparse_matrix
)
sparse_matrix_new <- table_to_matrix(
  table_sparse,
  return_sparse = TRUE
)
identical(sparse_matrix, sparse_matrix_new)
#> [1] TRUE
```
