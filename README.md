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

## **Troubleshooting**

### Common Issues and Solutions

#### Parallel Processing Issues

**Problem**: Parallel processing is slower than sequential processing

**Solutions**:
- Ensure each task takes enough time to justify parallelization overhead (typically > 0.1 seconds)
- Reduce the number of cores if you have too many small tasks
- Check if you're limited by I/O rather than CPU

```r
# Bad: Tasks are too small
parallelize_fun(1:1000, function(x) x^2, cores = 4)  # Overhead dominates

# Good: Tasks are substantial
parallelize_fun(1:100, function(x) {
  Sys.sleep(0.1)
  complex_computation(x)
}, cores = 4)
```

**Problem**: "Cannot allocate vector of size..." error in parallel processing

**Solutions**:
- Reduce the number of cores to lower memory usage
- Process data in smaller chunks
- Close other applications to free up memory

```r
# Process in chunks
chunk_size <- 100
for (i in seq(1, length(data), chunk_size)) {
  chunk <- data[i:min(i + chunk_size - 1, length(data))]
  result <- parallelize_fun(chunk, fun, cores = 2)
}
```

#### Matrix Operation Issues

**Problem**: Matrix operations are consuming too much memory

**Solutions**:
- Keep matrices sparse when possible
- Use `as_matrix()` only when absolutely necessary
- Process matrices in batches

```r
# Bad: Converting large sparse matrix to dense
dense <- as.matrix(large_sparse_matrix)

# Good: Keep it sparse
result <- sparse_cor(large_sparse_matrix)
```

**Problem**: Matrix operations fail with "non-conformable arrays"

**Solution**: Check dimensions before operations

```r
# Check dimensions
log_message("Matrix dimensions: {.val {nrow(mat)}} x {.val {ncol(mat)}}")

# Ensure compatibility
if (ncol(mat1) == nrow(mat2)) {
  result <- mat1 %*% mat2
}
```

#### Logging Issues

**Problem**: Log messages are not appearing

**Solutions**:
- Check if verbose output is disabled: `getOption("log_message.verbose")`
- Enable it: `options(log_message.verbose = TRUE)`
- Ensure you're not suppressing messages: `suppressMessages()`

```r
# Check current setting
getOption("log_message.verbose")

# Enable globally
options(log_message.verbose = TRUE)
```

**Problem**: CLI inline markup not rendering correctly

**Solution**: Ensure `cli_model = TRUE` (default)

```r
# This works
log_message("Processing {.file data.csv}", cli_model = TRUE)

# This won't render markup
log_message("Processing {.file data.csv}", cli_model = FALSE)
```

#### Installation Issues

**Problem**: Package fails to install due to compilation errors

**Solutions**:
- Ensure you have a C++ compiler installed (Rtools on Windows, Xcode on macOS)
- Update R to the latest version
- Try installing from CRAN instead of GitHub

```r
# Install from CRAN (pre-compiled binaries available)
install.packages("thisutils")
```

**Problem**: Dependencies fail to install

**Solution**: Install dependencies manually

```r
# Install required packages
install.packages(c("cli", "doParallel", "foreach", "Matrix", "Rcpp", "rlang"))

# Then install thisutils
install.packages("thisutils")
```

### Performance Optimization

If you're experiencing performance issues:

1. **Profile your code** to identify bottlenecks
   ```r
   system.time({
     result <- your_function()
   })
   ```

2. **Test with small data first** before scaling up
   ```r
   # Test with subset
   small_test <- data[1:100, ]
   result <- process_data(small_test, cores = 1)
   ```

3. **Monitor resource usage** during execution
   - Use `htop` or Task Manager to check CPU and memory
   - Adjust `cores` parameter based on available resources

4. **Consider batch processing** for very large datasets
   ```r
   batch_process <- function(data, batch_size = 1000, cores = 4) {
     n_batches <- ceiling(nrow(data) / batch_size)
     
     results <- list()
     for (i in 1:n_batches) {
       start <- (i - 1) * batch_size + 1
       end <- min(i * batch_size, nrow(data))
       batch <- data[start:end, ]
       
       results[[i]] <- parallelize_fun(
         as.list(1:nrow(batch)),
         function(j) process_row(batch[j, ]),
         cores = cores
       )
     }
     
     do.call(c, results)
   }
   ```

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
