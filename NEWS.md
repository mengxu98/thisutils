# thisutils

# thisutils 0.3.1

* **docs**:
  * Update `figlet()` references to use HTTPS, addressing CRAN incoming NOTE.

# thisutils 0.3.0

* **func**:
  * Enhance `log_message()` function: Added new `plain_text` parameter to suppress level, symbol, timestamp, and message type formatting while preserving color and multiline settings, allowing for cleaner text-only output.
  * Improve `add_pkg_file()` function: Enhanced message types and formatting, using more appropriate message types (`warning`, `running`) and improved file references in log messages.

# thisutils 0.2.5

* **func**:
  * Optimize `check_sparsity()` function: Improved calculation logic by computing total counts upfront, avoiding redundant calculations for sparse and non-sparse matrices.
  * Optimize `normalization()` function: Enhanced `max_min` and `maximum` methods by caching min/max values to avoid repeated calculations.
  * Improve code quality in `parallelize_fun()`: Replaced `sapply()` with `vapply()` for type-safe return values and optimized error handling logic.
  * Optimize P-value combination functions (`meanp()`, `sump()`, `votep()`): Extracted `validp` variable to avoid repeated indexing and improved code efficiency.
  * Enhance `sump()` function: Replaced loop-based calculation with vectorized operations for better performance.
  * Improve `capitalize()` and `unnest_fun()` functions: Replaced `sapply()` with `vapply()` for type safety.
  * Enhance `add_pkg_file()` function: Now automatically checks for the existence of `src` directory and only adds `@useDynLib` directive when `src` folder is present in the package environment.

# thisutils 0.2.4

* **func**:
  * Enhance `log_message()` function: Now automatically handles non-character objects (e.g., `data.table`, `data.frame`) by formatting them with `capture.output(print(...))`, allowing direct object input without manual string conversion.

# thisutils 0.2.3

* **func**:
  * Rewrite `matrix_to_table()` C++ implementation to iterate `dgCMatrix` slots directly, avoiding dense conversion and reducing memory/time cost on large sparse matrices.
  * Align `keep_zero`/`threshold` semantics across C++/R: retain entries if `abs(value) >= threshold`; zeros are retained only when `keep_zero = TRUE` and `threshold == 0`.

# thisutils 0.2.2

* **func**:
  * Enhance `parallelize_fun()` function: Added support for named vectors and vectors in progress display, showing current processing item names or values in the progress bar. Improved progress bar formatting with status information and enhanced parallel processing progress updates.
  * Improve visual formatting in `add_pkg_file()` and `thisutils-package.R`: Enhanced separator line display with grey color styling using `cli::col_grey()` for better visual consistency.

# thisutils 0.2.1

* **func**:
  * Enhance `add_pkg_file()` function: Added automatic dependency checking to ensure `cli` package is included in DESCRIPTION file, and automatic pkgdown configuration checking to ensure proper package overview section setup.

# thisutils 0.2.0

* **func**:
  * Add package logo.
  * Add `get_verbose()` function: New exported function to get verbose option from global options or local arguments, providing better control over message display.
  * Enhance `.onAttach()` function: Now respects verbose settings and only displays startup messages when verbose mode is enabled, improving user experience.
  * Update `add_pkg_file()` function: Generated `.onAttach()` function now includes verbose check using `get_verbose()`, ensuring consistency with package behavior.

* **docs**:
  * Corrected brace escaping in `parse_inline_expressions()`.

# thisutils 0.1.9

* **func**:
  * Remove `list_figlet_fonts()` and `rescale()` functions.

# thisutils 0.1.7

* **func**:
  * Refactor `add_pkg_file()` function: Simplified function parameters by removing manual parameter inputs and automatically extracting all package information from DESCRIPTION file. Enhanced error handling and improved logging messages.
  * Enhance `parallelize_fun()` function with robust error handling: Added `clean_result` parameter to control automatic removal of failed results, and `throw_error` parameter to control detailed error message display. Functions now gracefully handle partial failures without stopping execution.
  * Add `parse_inline_expressions()` function: High-performance inline expression parser that evaluates `{}` expressions while preserving CLI formatting markers like `{.val ...}`.
  * Enhance `log_message()` function: Add new `"running"` message type with orange circle dotted symbol (`◌`) to indicate ongoing processes.

# thisutils 0.1.6

* **func**:
  * Add statistical *P-value* combination functions: `wilkinsonp()`, `maximump()`, `minimump()`, `meanp()`, `votep()`, and `sump()` for meta-analysis and multiple study result integration.
  * Add `max_depth()` function to calculate the maximum depth of nested lists.

# thisutils 0.1.5

* **func**:
  * Update `add_pkg_file()` function. Now, `add_pkg_file()` can automatically extract information of *R* package from `DESCRIPTION` file and save it in the `./R/` folder.

# thisutils 0.1.4

* **func**:
  * Delete `str_wrap()` function.

# thisutils 0.1.3

* **func**:
  * Replace `purrr::map2()` with `mapply()`, and delete `purrr` package in `DESCRIPTION`.
  * Replace `methods::is()` with `inherits()`, and delete `methods` package in `DESCRIPTION`.

* **docs**:
  * Update documentation for some functions.

# thisutils 0.1.1

* **func**:
  * Add `matrix_process()` function.

# thisutils 0.0.9

* **func**:
  * Add `matrix_to_table()` and `table_to_matrix()` functions for data conversion between matrix and table formats.
  * Modify parameter `sparse` to `return_sparse` for `as_matrix()`

* **docs**:
  * Update documentation for some functions.

# thisutils 0.0.8

* **docs**:
  * Update documentation for some functions.
