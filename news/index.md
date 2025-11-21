# Changelog

## thisutils 0.3.2

- **func**:
  - Change default value of `timestamp_style` parameter in
    [`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
    function from `TRUE` to `FALSE` for cleaner default output.

## thisutils 0.3.1

CRAN release: 2025-11-17

- **docs**:
  - Update
    [`figlet()`](https://mengxu98.github.io/thisutils/reference/figlet.md)
    references to use HTTPS, addressing CRAN incoming NOTE.

## thisutils 0.3.0

- **func**:
  - Enhance
    [`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
    function: Added new `plain_text` parameter to suppress level,
    symbol, timestamp, and message type formatting while preserving
    color and multiline settings, allowing for cleaner text-only output.
  - Improve
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function: Enhanced message types and formatting, using more
    appropriate message types (`warning`, `running`) and improved file
    references in log messages.

## thisutils 0.2.5

- **func**:
  - Optimize
    [`check_sparsity()`](https://mengxu98.github.io/thisutils/reference/check_sparsity.md)
    function: Improved calculation logic by computing total counts
    upfront, avoiding redundant calculations for sparse and non-sparse
    matrices.
  - Optimize
    [`normalization()`](https://mengxu98.github.io/thisutils/reference/normalization.md)
    function: Enhanced `max_min` and `maximum` methods by caching
    min/max values to avoid repeated calculations.
  - Improve code quality in
    [`parallelize_fun()`](https://mengxu98.github.io/thisutils/reference/parallelize_fun.md):
    Replaced [`sapply()`](https://rdrr.io/r/base/lapply.html) with
    [`vapply()`](https://rdrr.io/r/base/lapply.html) for type-safe
    return values and optimized error handling logic.
  - Optimize P-value combination functions
    ([`meanp()`](https://mengxu98.github.io/thisutils/reference/meanp.md),
    [`sump()`](https://mengxu98.github.io/thisutils/reference/sump.md),
    [`votep()`](https://mengxu98.github.io/thisutils/reference/votep.md)):
    Extracted `validp` variable to avoid repeated indexing and improved
    code efficiency.
  - Enhance
    [`sump()`](https://mengxu98.github.io/thisutils/reference/sump.md)
    function: Replaced loop-based calculation with vectorized operations
    for better performance.
  - Improve
    [`capitalize()`](https://mengxu98.github.io/thisutils/reference/capitalize.md)
    and
    [`unnest_fun()`](https://mengxu98.github.io/thisutils/reference/unnest_fun.md)
    functions: Replaced [`sapply()`](https://rdrr.io/r/base/lapply.html)
    with [`vapply()`](https://rdrr.io/r/base/lapply.html) for type
    safety.
  - Enhance
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function: Now automatically checks for the existence of `src`
    directory and only adds `@useDynLib` directive when `src` folder is
    present in the package environment.

## thisutils 0.2.4

- **func**:
  - Enhance
    [`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
    function: Now automatically handles non-character objects (e.g.,
    `data.table`, `data.frame`) by formatting them with
    `capture.output(print(...))`, allowing direct object input without
    manual string conversion.

## thisutils 0.2.3

- **func**:
  - Rewrite
    [`matrix_to_table()`](https://mengxu98.github.io/thisutils/reference/matrix_to_table.md)
    C++ implementation to iterate `dgCMatrix` slots directly, avoiding
    dense conversion and reducing memory/time cost on large sparse
    matrices.
  - Align `keep_zero`/`threshold` semantics across C++/R: retain entries
    if `abs(value) >= threshold`; zeros are retained only when
    `keep_zero = TRUE` and `threshold == 0`.

## thisutils 0.2.2

- **func**:
  - Enhance
    [`parallelize_fun()`](https://mengxu98.github.io/thisutils/reference/parallelize_fun.md)
    function: Added support for named vectors and vectors in progress
    display, showing current processing item names or values in the
    progress bar. Improved progress bar formatting with status
    information and enhanced parallel processing progress updates.
  - Improve visual formatting in
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    and `thisutils-package.R`: Enhanced separator line display with grey
    color styling using
    [`cli::col_grey()`](https://cli.r-lib.org/reference/ansi-styles.html)
    for better visual consistency.

## thisutils 0.2.1

- **func**:
  - Enhance
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function: Added automatic dependency checking to ensure `cli`
    package is included in DESCRIPTION file, and automatic pkgdown
    configuration checking to ensure proper package overview section
    setup.

## thisutils 0.2.0

CRAN release: 2025-10-06

- **func**:
  - Add package logo.
  - Add
    [`get_verbose()`](https://mengxu98.github.io/thisutils/reference/get_verbose.md)
    function: New exported function to get verbose option from global
    options or local arguments, providing better control over message
    display.
  - Enhance `.onAttach()` function: Now respects verbose settings and
    only displays startup messages when verbose mode is enabled,
    improving user experience.
  - Update
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function: Generated `.onAttach()` function now includes verbose
    check using
    [`get_verbose()`](https://mengxu98.github.io/thisutils/reference/get_verbose.md),
    ensuring consistency with package behavior.
- **docs**:
  - Corrected brace escaping in
    [`parse_inline_expressions()`](https://mengxu98.github.io/thisutils/reference/parse_inline_expressions.md).

## thisutils 0.1.9

- **func**:
  - Remove `list_figlet_fonts()` and `rescale()` functions.

## thisutils 0.1.7

- **func**:
  - Refactor
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function: Simplified function parameters by removing manual
    parameter inputs and automatically extracting all package
    information from DESCRIPTION file. Enhanced error handling and
    improved logging messages.
  - Enhance
    [`parallelize_fun()`](https://mengxu98.github.io/thisutils/reference/parallelize_fun.md)
    function with robust error handling: Added `clean_result` parameter
    to control automatic removal of failed results, and `throw_error`
    parameter to control detailed error message display. Functions now
    gracefully handle partial failures without stopping execution.
  - Add
    [`parse_inline_expressions()`](https://mengxu98.github.io/thisutils/reference/parse_inline_expressions.md)
    function: High-performance inline expression parser that evaluates
    [`{}`](https://rdrr.io/r/base/Paren.html) expressions while
    preserving CLI formatting markers like `{.val ...}`.
  - Enhance
    [`log_message()`](https://mengxu98.github.io/thisutils/reference/log_message.md)
    function: Add new `"running"` message type with orange circle dotted
    symbol (`◌`) to indicate ongoing processes.

## thisutils 0.1.6

- **func**:
  - Add statistical *P-value* combination functions:
    [`wilkinsonp()`](https://mengxu98.github.io/thisutils/reference/wilkinsonp.md),
    [`maximump()`](https://mengxu98.github.io/thisutils/reference/maximump.md),
    [`minimump()`](https://mengxu98.github.io/thisutils/reference/minimump.md),
    [`meanp()`](https://mengxu98.github.io/thisutils/reference/meanp.md),
    [`votep()`](https://mengxu98.github.io/thisutils/reference/votep.md),
    and
    [`sump()`](https://mengxu98.github.io/thisutils/reference/sump.md)
    for meta-analysis and multiple study result integration.
  - Add
    [`max_depth()`](https://mengxu98.github.io/thisutils/reference/max_depth.md)
    function to calculate the maximum depth of nested lists.

## thisutils 0.1.5

CRAN release: 2025-09-11

- **func**:
  - Update
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    function. Now,
    [`add_pkg_file()`](https://mengxu98.github.io/thisutils/reference/add_pkg_file.md)
    can automatically extract information of *R* package from
    `DESCRIPTION` file and save it in the `./R/` folder.

## thisutils 0.1.4

- **func**:
  - Delete `str_wrap()` function.

## thisutils 0.1.3

- **func**:
  - Replace
    [`purrr::map2()`](https://purrr.tidyverse.org/reference/map2.html)
    with [`mapply()`](https://rdrr.io/r/base/mapply.html), and delete
    `purrr` package in `DESCRIPTION`.
  - Replace [`methods::is()`](https://rdrr.io/r/methods/is.html) with
    [`inherits()`](https://rdrr.io/r/base/class.html), and delete
    `methods` package in `DESCRIPTION`.
- **docs**:
  - Update documentation for some functions.

## thisutils 0.1.1

- **func**:
  - Add
    [`matrix_process()`](https://mengxu98.github.io/thisutils/reference/matrix_process.md)
    function.

## thisutils 0.0.9

- **func**:
  - Add
    [`matrix_to_table()`](https://mengxu98.github.io/thisutils/reference/matrix_to_table.md)
    and
    [`table_to_matrix()`](https://mengxu98.github.io/thisutils/reference/table_to_matrix.md)
    functions for data conversion between matrix and table formats.
  - Modify parameter `sparse` to `return_sparse` for
    [`as_matrix()`](https://mengxu98.github.io/thisutils/reference/as_matrix.md)
- **docs**:
  - Update documentation for some functions.

## thisutils 0.0.8

- **docs**:
  - Update documentation for some functions.
