# Package index

### Package overview

- [`thisutils`](https://mengxu98.github.io/thisutils/reference/thisutils-package.md)
  [`thisutils-package`](https://mengxu98.github.io/thisutils/reference/thisutils-package.md)
  : Reliable Utilities for Reusable Research Workflows
- [`thisutils_logo()`](https://mengxu98.github.io/thisutils/reference/thisutils_logo.md)
  : The logo of thisutils
- [`print(`*`<thisutils_logo>`*`)`](https://mengxu98.github.io/thisutils/reference/print.thisutils_logo.md)
  : Print logo

### Controlled execution

- [`parallelize_fun()`](https://mengxu98.github.io/thisutils/reference/parallelize_fun.md)
  : Parallelize a function
- [`get_verbose()`](https://mengxu98.github.io/thisutils/reference/get_verbose.md)
  : Get the verbose option
- [`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
  : Print formatted message
- [`parse_inline_expressions()`](https://mengxu98.github.io/thisutils/reference/parse_inline_expressions.md)
  : Parse inline expressions

### Sparse matrix semantics

- [`as_matrix()`](https://mengxu98.github.io/thisutils/reference/as_matrix.md)
  : Convert matrix into dense/sparse matrix
- [`check_sparsity()`](https://mengxu98.github.io/thisutils/reference/check_sparsity.md)
  : Check sparsity of matrix
- [`collapse_sparse_rows()`](https://mengxu98.github.io/thisutils/reference/collapse_sparse_rows.md)
  : Collapse sparse matrix rows by group
- [`fast_row_vars()`](https://mengxu98.github.io/thisutils/reference/fast_row_vars.md)
  : Compute row variances
- [`filter_nonzero_variance_features()`](https://mengxu98.github.io/thisutils/reference/filter_nonzero_variance_features.md)
  : Keep matrix rows with nonzero variance
- [`matrix_to_table()`](https://mengxu98.github.io/thisutils/reference/matrix_to_table.md)
  : Switch matrix to table
- [`pearson_correlation()`](https://mengxu98.github.io/thisutils/reference/pearson_correlation.md)
  : Correlation and covariance calculation for sparse matrix
- [`run_dense_topk_by_column()`](https://mengxu98.github.io/thisutils/reference/run_dense_topk_by_column.md)
  : Dense matrix top-k by column
- [`run_sparse_topk()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk.md)
  : Sparse matrix top-k selection
- [`run_sparse_topk_stored()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_stored.md)
  : Stored sparse entries top-k selection
- [`sparse_cor()`](https://mengxu98.github.io/thisutils/reference/sparse_cor.md)
  : Resource-controlled sparse correlation
- [`table_to_matrix()`](https://mengxu98.github.io/thisutils/reference/table_to_matrix.md)
  : Switch table to matrix

### Compatibility interfaces

- [`run_sparse_topk_by_column()`](https://mengxu98.github.io/thisutils/reference/run_sparse_topk_by_column.md)
  : Compatibility wrapper for column-wise sparse top-k selection
- [`run_sparse_stored_topk_by_column()`](https://mengxu98.github.io/thisutils/reference/run_sparse_stored_topk_by_column.md)
  : Compatibility wrapper for stored column-wise sparse top-k selection

### Neighborhood and evaluation metrics

- [`classification_metrics_compute()`](https://mengxu98.github.io/thisutils/reference/classification_metrics_compute.md)
  : Compute classification metrics
- [`compute_lisi()`](https://mengxu98.github.io/thisutils/reference/compute_lisi.md)
  : Compute Local Inverse Simpson's Index (LISI)
- [`run_biocneighbors_knn()`](https://mengxu98.github.io/thisutils/reference/run_biocneighbors_knn.md)
  : Find nearest neighbors with BiocNeighbors

### Research-package interoperability

- [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
  : Add a package file and print package information
- [`check_pkg_status()`](https://mengxu98.github.io/thisutils/reference/check_pkg_status.md)
  : Check if a package is installed with the specified version
- [`check_r()`](https://mengxu98.github.io/thisutils/reference/check_r.md)
  : Check and install R packages
- [`get_namespace_fun()`](https://mengxu98.github.io/thisutils/reference/get_namespace_fun.md)
  : Get a function from a namespace
- [`invoke_fun()`](https://mengxu98.github.io/thisutils/reference/invoke_fun.md)
  : Invoke a function with a list of arguments
- [`remove_r()`](https://mengxu98.github.io/thisutils/reference/remove_r.md)
  : Check and remove R packages

### Additional statistical and utility helpers

- [`` `%ss%` ``](https://mengxu98.github.io/thisutils/reference/grapes-ss-grapes.md)
  : Value selection operator
- [`capitalize()`](https://mengxu98.github.io/thisutils/reference/capitalize.md)
  : Capitalize the first letter of each word
- [`check_ci_env()`](https://mengxu98.github.io/thisutils/reference/check_ci_env.md)
  : Check CI environment
- [`download()`](https://mengxu98.github.io/thisutils/reference/download.md)
  : Download file from the Internet
- [`is_outlier()`](https://mengxu98.github.io/thisutils/reference/is_outlier.md)
  : Detect outliers using MAD (Median Absolute Deviation)
- [`is_apple_silicon()`](https://mengxu98.github.io/thisutils/reference/is_apple_silicon.md)
  : Check if the system is running on Apple Silicon
- [`is_linux()`](https://mengxu98.github.io/thisutils/reference/is_linux.md)
  : Check if the operating system is Linux
- [`is_osx()`](https://mengxu98.github.io/thisutils/reference/is_osx.md)
  : Check if the operating system is macOS
- [`is_windows()`](https://mengxu98.github.io/thisutils/reference/is_windows.md)
  : Check if the operating system is Windows
- [`matrix_process()`](https://mengxu98.github.io/thisutils/reference/matrix_process.md)
  : Process matrix
- [`max_depth()`](https://mengxu98.github.io/thisutils/reference/max_depth.md)
  : Maximum depth of a list
- [`maximump()`](https://mengxu98.github.io/thisutils/reference/maximump.md)
  : Maximum P-value
- [`meanp()`](https://mengxu98.github.io/thisutils/reference/meanp.md) :
  Mean P-value
- [`minimump()`](https://mengxu98.github.io/thisutils/reference/minimump.md)
  : Minimum P-value
- [`normalization()`](https://mengxu98.github.io/thisutils/reference/normalization.md)
  : Normalize numeric vector
- [`r_square()`](https://mengxu98.github.io/thisutils/reference/r_square.md)
  : Coefficient of determination (\\R^2\\)
- [`remove_space()`](https://mengxu98.github.io/thisutils/reference/remove_space.md)
  : Remove and normalize spaces
- [`split_indices()`](https://mengxu98.github.io/thisutils/reference/split_indices.md)
  : Split indices.
- [`simulate_sparse_matrix()`](https://mengxu98.github.io/thisutils/reference/simulate_sparse_matrix.md)
  : Generate a simulated sparse matrix
- [`sump()`](https://mengxu98.github.io/thisutils/reference/sump.md) :
  Sum P-value
- [`try_get()`](https://mengxu98.github.io/thisutils/reference/try_get.md)
  : Try to evaluate an expression a set number of times before failing
- [`unnest_fun()`](https://mengxu98.github.io/thisutils/reference/unnest_fun.md)
  : Unnest a list-column
- [`votep()`](https://mengxu98.github.io/thisutils/reference/votep.md) :
  Vote P-value
- [`wilkinsonp()`](https://mengxu98.github.io/thisutils/reference/wilkinsonp.md)
  : Wilkinson's P-value

### Presentation helpers

- [`figlet()`](https://mengxu98.github.io/thisutils/reference/figlet.md)
  : The figlet function
- [`figlet_font()`](https://mengxu98.github.io/thisutils/reference/figlet_font.md)
  : Get a figlet font
