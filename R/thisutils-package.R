# -*- coding: utf-8 -*-

#' @title thisutils: An R package for utility functions.
#'
#' @useDynLib thisutils
#'
#' @description
#' An R package for utility functions.
#'
#' @author Meng Xu (Maintainer), \email{mengxu98@qq.com}
#'
#' @source \url{https://mengxu98.github.io/thisutils/}
#'
#' @md
#' @docType package
#' @name thisutils-package
"_PACKAGE"

#' @title thisutils logo
#'
#' @description
#' The thisutils logo, using ASCII or Unicode characters
#' Use [cli::ansi_strip] to get rid of the colors.
#'
#' @md
#' @param unicode Unicode symbols. Default is `TRUE` on UTF-8 platforms.
#'
#' @return A logo with ASCII formatted text
#'
#' @references
#'  \url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}
#'
#' @export
#' @examples
#' thisutils_logo()
thisutils_logo <- function(
    unicode = cli::is_utf8_output()) {
  logo <- c(
    "       0        1      2           3    4
   __  __    _              __  _  __
  / /_/ /_  (_)_____ __  __/ /_(_)/ /_____
 / __/ __ ./ // ___// / / / __/ // // ___/
/ /_/ / / / /(__  )/ /_/ / /_/ // /(__  )
.__/_/ /_/_//____/ .__,_/.__/_//_//____/
    5             6      7      8       9   "
  )

  hexa <- c("*", ".", "o", "*", ".", "o", "*", ".", "o", "*")
  if (unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <- c(
    "red", "yellow", "green", "magenta", "cyan", "yellow", "green", "white", "magenta", "cyan"
  )

  col_hexa <- purrr::map2(
    hexa, cols, ~ cli::make_ansi_style(.y)(.x)
  )

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  structure(cli::col_blue(logo), class = "logo")
}

#' @title print logo
#'
#' @param x Input information.
#' @param ... Other parameters.
#'
#' @return Print the ASCII logo
#'
#' @method print logo
#'
#' @export
print.logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}

.onAttach <- function(libname, pkgname) {
  version <- utils::packageDescription(pkgname, fields = "Version")

  msg <- paste0(
    "-------------------------------------------------------
",
    cli::col_blue(" ", pkgname, " version ", version),
    "
   This message can be suppressed by:
     suppressPackageStartupMessages(library(thisutils))
-------------------------------------------------------"
  )

  packageStartupMessage(thisutils_logo())
  packageStartupMessage(msg)
}
