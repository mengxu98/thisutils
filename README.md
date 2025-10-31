# ***thisutils*** <img src="man/figures/logo.svg" align="right" width="120"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/thisutils)](https://CRAN.R-project.org/package=thisutils) [![develop-ver](https://img.shields.io/github/r-package/v/mengxu98/thisutils?label=develop-ver)](https://github.com/mengxu98/thisutils/) [![R-CMD-check](https://github.com/mengxu98/thisutils/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mengxu98/thisutils/actions/workflows/R-CMD-check.yaml) [![test-coverage](https://github.com/mengxu98/thisutils/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/mengxu98/thisutils/actions/workflows/test-coverage.yaml) [![pkgdown](https://github.com/mengxu98/thisutils/actions/workflows/pkgdown.yaml/badge.svg)](https://mengxu98.github.io/thisutils/reference/index.html) [![RStudio CRAN mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/thisutils)](https://CRAN.R-project.org/package=thisutils)

<!-- badges: end -->

## **Introduction**

[thisutils](https://mengxu98.github.io/thisutils/) is a comprehensive R package that provides a collection of utility functions for data analysis and scientific computing. The package is designed to streamline common workflows by offering tools for:

- **Parallel Processing**: Efficiently parallelize functions across multiple cores
- **Logging & Messaging**: Create formatted, colorful console messages with timestamps
- **Matrix Operations**: Process, normalize, and analyze sparse and dense matrices
- **Statistical Functions**: Perform correlation analysis, compute p-values, and calculate R²
- **ASCII Art**: Generate fancy text and logos for your R projects
- **Utility Functions**: Download files, handle spaces, capitalize text, and more

## **Installation**

### Install from CRAN

The stable version is available on CRAN:

``` r
install.packages("thisutils")
```

Or using the modern [pak](https://github.com/r-lib/pak) package manager:

``` r
if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("thisutils")
```

### Install Development Version

Get the latest features from [GitHub](https://github.com/mengxu98/thisutils):

``` r
if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pak("mengxu98/thisutils")
```

## **Quick Start**

Load the package:

``` r
library(thisutils)
```

## **Key Features**

### 1. Parallel Processing

The `parallelize_fun()` function makes it easy to run computations in parallel:

``` r
# Sequential execution
result <- parallelize_fun(1:10, function(x) {
  Sys.sleep(0.1)
  x^2
}, cores = 1)

# Parallel execution with 4 cores
result <- parallelize_fun(1:10, function(x) {
  Sys.sleep(0.1)
  x^2
}, cores = 4)

# With error handling
result <- parallelize_fun(1:5, function(x) {
  if (x == 3) stop("Error on element 3")
  x^2
}, clean_result = TRUE)  # Remove failed results
```

### 2. Enhanced Logging

Create beautiful, formatted log messages with `log_message()`:

``` r
# Basic messages
log_message("Processing data...")
log_message("Task completed successfully!", message_type = "success")
log_message("Warning: Check input data", message_type = "warning")

# Colored messages
log_message("Important message", text_color = "red", text_style = "bold")

# With CLI inline markup
log_message("Processing file: {.file data.csv}")
log_message("Using package: {.pkg dplyr}")
log_message("Function {.fn mean} returned {.val 42}")

# Control verbosity globally
options(log_message.verbose = FALSE)
log_message("This won't be printed")
options(log_message.verbose = TRUE)
```

### 3. Matrix Operations

Work with sparse and dense matrices efficiently:

``` r
# Generate sparse matrices
sparse_mat <- simulate_sparse_matrix(
  nrow = 1000, 
  ncol = 500, 
  sparsity = 0.95
)

# Check sparsity
check_sparsity(sparse_mat)

# Process matrices
processed <- matrix_process(sparse_mat, method = "zscore")
processed <- matrix_process(sparse_mat, method = "log1p")

# Convert sparse to dense
dense_mat <- as_matrix(sparse_mat)

# Normalize data
x <- c(1, 2, 3, 4, 5)
normalization(x, method = "max_min")
normalization(x, method = "z_score")
```

### 4. Correlation Analysis

Compute correlations efficiently, including for sparse matrices:

``` r
# Pearson correlation (optimized C++ implementation)
mat <- matrix(rnorm(1000), ncol = 10)
cor_result <- pearson_correlation(mat)

# Sparse correlation
sparse_mat <- simulate_sparse_matrix(100, 50, sparsity = 0.9)
sparse_cor_result <- sparse_cor(sparse_mat)
```

### 5. Statistical Functions

Calculate various statistical measures:

``` r
# R-squared
y_true <- rnorm(100)
y_pred <- y_true + rnorm(100, sd = 0.5)
r_square(y_true, y_pred)

# P-value combinations
p_values <- c(0.01, 0.02, 0.03, 0.04, 0.05)
wilkinsonp(p_values, r = 2)
minimump(p_values)
maximump(p_values)
meanp(p_values)
votep(p_values, alpha = 0.05)
```

### 6. ASCII Art

Create eye-catching logos and text art:

``` r
# Display package logo
thisutils_logo()

# Create ASCII art text
figlet("Hello World!")
figlet("R Stats", font = "standard")

# List available fonts
figlet_font()
```

### 7. Utility Functions

Helpful utilities for everyday tasks:

``` r
# Null-coalescing operator
NULL %ss% "default value"  # Returns "default value"
5 %ss% "default value"     # Returns 5

# Capitalize text
capitalize(c("hello world", "data science"))

# Remove/normalize spaces
remove_space("  hello   world  ")
remove_space("  test  ", trim_start = TRUE, trim_end = TRUE)

# Download files with automatic retry
download(
  url = "https://example.com/data.csv",
  destfile = "data.csv",
  max_tries = 3
)

# Try function with retries
result <- try_get(
  expr = some_unreliable_function(),
  max_tries = 5
)

# Convert between matrix formats
df <- data.frame(row = c("A", "A", "B"), col = c("X", "Y", "X"), value = c(1, 2, 3))
mat <- table_to_matrix(df)
df_back <- matrix_to_table(mat)
```

## **Documentation**

For comprehensive documentation, including detailed examples and function references, visit:

- **Function Reference**: [https://mengxu98.github.io/thisutils/reference/index.html](https://mengxu98.github.io/thisutils/reference/index.html)
- **GitHub Repository**: [https://github.com/mengxu98/thisutils](https://github.com/mengxu98/thisutils)

## **Common Use Cases**

### Parallel Data Processing

Process large datasets efficiently using parallel computation:

``` r
library(thisutils)

# Process a list of data frames in parallel
data_list <- list(
  df1 = data.frame(x = rnorm(1000)),
  df2 = data.frame(x = rnorm(1000)),
  df3 = data.frame(x = rnorm(1000))
)

results <- parallelize_fun(
  x = data_list,
  fun = function(df) {
    # Perform computations
    data.frame(
      mean = mean(df$x),
      sd = sd(df$x),
      n = nrow(df)
    )
  },
  cores = 3,
  verbose = TRUE
)
```

### Building Analysis Pipelines

Combine utilities for clean analysis workflows:

``` r
# Load and process data
log_message("Starting analysis pipeline", message_type = "info")

# Generate or load data
data_matrix <- simulate_sparse_matrix(500, 100, sparsity = 0.9)
log_message("Generated sparse matrix with {.val {check_sparsity(data_matrix)*100}%} sparsity")

# Process matrix
processed_data <- matrix_process(data_matrix, method = "zscore")
log_message("Applied z-score normalization", message_type = "success")

# Compute correlations
correlations <- sparse_cor(processed_data)
log_message("Computed correlation matrix", message_type = "success")

log_message("Analysis complete!", message_type = "success")
```

### Custom Error Handling

Robust error handling for production code:

``` r
# Function that might fail
fetch_data <- function(id) {
  try_get(
    expr = {
      # Simulate API call that might fail
      if (runif(1) < 0.3) stop("Connection error")
      log_message("Fetched data for ID: {.val {id}}", message_type = "success")
      data.frame(id = id, value = rnorm(1))
    },
    max_tries = 5,
    error_message = "Failed to fetch data from API"
  )
}

# Process multiple IDs with error handling
results <- parallelize_fun(
  x = 1:10,
  fun = fetch_data,
  clean_result = TRUE,  # Remove failed attempts
  cores = 2
)
```

## **Performance Tips**

1. **Use parallel processing** for independent computations with `cores > 1`
2. **Keep sparse matrices sparse** - use `sparse_cor()` instead of converting to dense
3. **Suppress verbose output** in production with `options(log_message.verbose = FALSE)`
4. **Use vectorized operations** where possible before parallelizing

## **Contributing**

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## **Getting Help**

If you encounter a bug or have a feature request:

1. Check the [function reference](https://mengxu98.github.io/thisutils/reference/index.html)
2. Search [existing issues](https://github.com/mengxu98/thisutils/issues)
3. Create a [new issue](https://github.com/mengxu98/thisutils/issues/new) with a reproducible example

## **License**

MIT License - see [LICENSE](LICENSE) file for details

## **Citation**

If you use thisutils in your research, please cite:

``` r
citation("thisutils")
```

## **Author**

Meng Xu (mengxu98@qq.com)

ORCID: [0000-0002-8300-1054](https://orcid.org/0000-0002-8300-1054)
