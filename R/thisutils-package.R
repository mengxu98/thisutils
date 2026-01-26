# -*- coding: utf-8 -*-

#' @title Collection of Utility Functions for Data Analysis and Computing
#'
#' @useDynLib thisutils
#'
#' @description
#' Provides utility functions for data analysis and scientific computing. Includes functions for logging, parallel processing, and other computational tasks to streamline workflows.
#'
#' @author Meng Xu (Maintainer), \email{mengxu98@qq.com}
#'
#' @source \url{https://mengxu98.github.io/thisutils/}
#'
#' @md
#' @docType package
#' @name thisutils-package
"_PACKAGE"

#' @title The logo of thisutils
#'
#' @description
#' The thisutils logo, using ASCII or Unicode characters
#' Use [cli::ansi_strip] to get rid of the colors.
#'
#' @md
#' @param unicode Unicode symbols on UTF-8 platforms.
#' Default is [cli::is_utf8_output].
#'
#' @return
#' A character vector with class `thisutils_logo`.
#'
#' @references
#' \url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}
#'
#' @export
#' @examples
#' thisutils_logo()
thisutils_logo <- function(unicode = cli::is_utf8_output()) {
  logo <- c(
    "          0          1        2             3     4
            __  __    _              __  _  __
           / /_/ /_  (_)_____ __  __/ /_(_)/ /_____
          / __/ __ ./ // ___// / / / __/ // // ___/
         / /_/ / / / /(__  )/ /_/ / /_/ // /(__  )
         .__/_/ /_/_//____/ .__,_/.__/_//_//____/
      5               6      7        8          9"
  )

  hexa <- c("*", ".", "o", "*", ".", "o", "*", ".", "o", "*")
  if (unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <- c(
    "red", "yellow", "green", "magenta", "cyan",
    "yellow", "green", "white", "magenta", "cyan"
  )

  col_hexa <- mapply(
    function(x, y) cli::make_ansi_style(y)(x),
    hexa, cols,
    SIMPLIFY = FALSE
  )

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(
    cli::col_blue(logo),
    class = "thisutils_logo"
  )
}

#' @title Print logo
#'
#' @param x Input information.
#' @param ... Other parameters.
#'
#' @return Print the ASCII logo
#'
#' @method print thisutils_logo
#'
#' @export
print.thisutils_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

.onAttach <- function(libname, pkgname) {
  verbose <- thisutils::get_verbose()
  if (isTRUE(verbose)) {
    version <- utils::packageVersion(pkgname)
    date <- utils::packageDate(pkgname)
    url <- utils::packageDescription(
      pkgname, fields = "URL"
    )

    msg <- paste0(
      cli::col_grey(strrep("-", 60)),
      "\n",
      cli::col_blue("Version: ", version, " (", date, " update)"),
      "\n",
      cli::col_blue("Website: ", cli::style_italic(url)),
      "\n\n",
      cli::col_grey("This message can be suppressed by:"),
      "\n",
      cli::col_grey("  suppressPackageStartupMessages(library(thisutils))"),
      "\n",
      cli::col_grey("  or options(log_message.verbose = FALSE)"),
      "\n",
      cli::col_grey(strrep("-", 60))
    )

    packageStartupMessage(thisutils_logo())
    packageStartupMessage(msg)
  }
}
