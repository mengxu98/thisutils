# Switch matrix to table

Switch matrix to table

## Usage

``` r
matrix_to_table(
  matrix,
  row_names = NULL,
  col_names = NULL,
  threshold = 0,
  keep_zero = TRUE
)
```

## Arguments

- matrix:

  A matrix.

- row_names:

  Character vector of row names to filter by.

- col_names:

  Character vector of column names to filter by.

- threshold:

  The threshold for filtering values based on absolute values. Defaults
  to `0`.

- keep_zero:

  Whether to keep zero values in the table. Defaults to `false`.

## Value

A table with three columns: `row`, `col`, and `value`.

## See also

[table_to_matrix](https://mengxu98.github.io/thisutils/reference/table_to_matrix.md)

## Examples

``` r
test_matrix <- simulate_sparse_matrix(10, 10)
colnames(test_matrix) <- paste0("c", 1:10)
rownames(test_matrix) <- paste0("r", 1:10)
table <- matrix_to_table(test_matrix)
matrix_new <- table_to_matrix(table)
test_matrix <- test_matrix[rownames(matrix_new), colnames(matrix_new)] |>
  as_matrix()
identical(test_matrix, matrix_new)
#> [1] TRUE

matrix_to_table(
  test_matrix,
  threshold = 2
)
#>   row col value
#> 1  r8  c7     2
#> 2  r9  c4     2

matrix_to_table(
  test_matrix,
  row_names = c("r1", "r2"),
  col_names = c("c1", "c2")
)
#>   row col value
#> 1  r1  c1     1
#> 2  r1  c2     0
#> 3  r2  c1     0
#> 4  r2  c2     0
```
