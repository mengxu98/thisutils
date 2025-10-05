# thisutils

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
