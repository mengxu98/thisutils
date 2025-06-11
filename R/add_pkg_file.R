#' @title Add package logo functionality
#'
#' @description
#' Automatically generate package logo functions and related code for R package development.
#' This function creates a complete package logo setup including the logo function,
#' print method, and \link{.onAttach} function in the package's R directory.
#'
#' @md
#' @param pkg_name Character string, the name of the package. If NULL (default),
#' will be read from DESCRIPTION file.
#' @param pkg_description Character string, short description of the package.
#' If NULL (default), will be read from DESCRIPTION file.
#' @param author_name Character string, name of the package author.
#' If NULL (default), will be read from DESCRIPTION file.
#' @param author_email Character string, email of the package author.
#' If NULL (default), will be read from DESCRIPTION file.
#' @param github_url Character string, GitHub URL of the package.
#' If NULL (default), will be read from DESCRIPTION file or constructed based on package name.
#' @param output_dir Character string, directory where to save the package file.
#' Default is NULL, which will use tempdir() for temporary storage.
#' @param use_figlet Logical, whether to use figlet for ASCII art generation.
#' Default is TRUE.
#' @param figlet_font Character string, figlet font to use. Default is "Slant".
#' @param colors Character vector, colors to use for the logo elements.
#' @param unicode Logical, whether to use Unicode symbols. Default is TRUE.
#' @param verbose Logical, whether to print progress messages. Default is TRUE.
#' @param desc_file Character string, path to the DESCRIPTION file.
#' If NULL (default), package information must be provided manually via other parameters.
#'
#' @return creates a file in specified output directory
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create logo with manual parameters (recommended for portability)
#' add_pkg_file(
#'   pkg_name = "mypackage",
#'   pkg_description = "My awesome R package for data analysis",
#'   author_name = "Your Name",
#'   author_email = "your.email@example.com",
#'   github_url = "https://github.com/username/mypackage",
#'   output_dir = tempdir()
#' )
#'
#' # Create logo reading from DESCRIPTION file (if available)
#' if (file.exists("DESCRIPTION")) {
#'   add_pkg_file(
#'     desc_file = "DESCRIPTION",
#'     output_dir = tempdir()
#'   )
#' }
#'
#' # Create simple logo without figlet
#' add_pkg_file(
#'   pkg_name = "mypackage",
#'   pkg_description = "Simple package",
#'   author_name = "Author",
#'   author_email = "author@example.com",
#'   use_figlet = FALSE,
#'   output_dir = tempdir()
#' )
#'
#' # Customize colors and fonts
#' add_pkg_file(
#'   pkg_name = "mypackage",
#'   pkg_description = "Colorful package",
#'   author_name = "Author",
#'   author_email = "author@example.com",
#'   figlet_font = "Big",
#'   colors = c(
#'     "blue", "cyan", "green", "yellow", "red",
#'     "magenta", "white", "blue", "cyan", "green"
#'   ),
#'   output_dir = tempdir()
#' )
#' }
add_pkg_file <- function(
    pkg_name = NULL,
    desc_file = NULL,
    pkg_description = NULL,
    author_name = NULL,
    author_email = NULL,
    github_url = NULL,
    output_dir = NULL,
    use_figlet = TRUE,
    figlet_font = "Slant",
    colors = c(
      "red", "yellow", "green", "magenta", "cyan",
      "yellow", "green", "white", "magenta", "cyan"
    ),
    unicode = TRUE,
    verbose = TRUE) {
  if (is.null(output_dir)) {
    output_dir <- tempdir()
  }

  desc_info <- if (!is.null(desc_file)) {
    .read_description(desc_file, verbose)
  } else {
    list(
      Package = NULL,
      Description = NULL,
      Author = NULL,
      Email = NULL,
      GitHub_URL = NULL
    )
  }

  if (is.null(pkg_name)) {
    pkg_name <- desc_info$Package
    if (is.null(pkg_name)) {
      log_message(
        "Package name not provided and not found in DESCRIPTION file",
        message_type = "error"
      )
    }
  }
  if (is.null(pkg_description)) {
    pkg_description <- desc_info$Description
  }
  if (is.null(author_name)) {
    author_name <- desc_info$Author
  }
  if (is.null(author_email)) {
    author_email <- desc_info$Email
  }
  if (is.null(github_url)) {
    github_url <- desc_info$GitHub_URL
  }

  if (verbose) {
    log_message("Creating package logo for: ", pkg_name)
  }

  ascii_lines <- NULL
  if (use_figlet) {
    tryCatch(
      {
        ascii_art <- figlet(pkg_name, font = figlet_font)
        ascii_lines <- as.character(ascii_art)
        if (verbose) {
          log_message("Generated figlet ASCII art successfully")
        }
      },
      error = function(e) {
        if (verbose) {
          log_message("Figlet generation failed, using simple ASCII art",
            message_type = "warning"
          )
        }
      }
    )
  }

  if (is.null(ascii_lines)) {
    ascii_lines <- c(
      paste0("  ", paste(rep("*", nchar(pkg_name) + 4), collapse = "")),
      paste0("  * ", pkg_name, " *"),
      paste0("  ", paste(rep("*", nchar(pkg_name) + 4), collapse = ""))
    )
  }

  file_content <- .generate_content(
    pkg_name = pkg_name,
    pkg_description = pkg_description,
    author_name = author_name,
    author_email = author_email,
    github_url = github_url,
    ascii_lines = ascii_lines,
    colors = colors,
    unicode = unicode
  )

  output_file <- file.path(
    output_dir,
    paste0(pkg_name, "-package.R")
  )

  writeLines(file_content, output_file)
  if (verbose) {
    log_message("Package file created successfully: ", output_file,
      message_type = "success"
    )
  }

  invisible(output_file)
}

.generate_content <- function(
    pkg_name,
    pkg_description,
    author_name,
    author_email,
    github_url,
    ascii_lines,
    colors,
    unicode) {
  ascii_with_numbers <- .add_color_numbers_to_ascii(
    ascii_lines,
    length(colors)
  )

  content <- c(
    "# -*- coding: utf-8 -*-",
    "",
    paste0("#' @title ", pkg_name, ": ", pkg_description),
    "#'",
    paste0("#' @useDynLib ", pkg_name),
    "#'",
    "#' @description",
    paste0("#' ", pkg_description),
    "#'",
    paste0("#' @author ", author_name, " (Maintainer), \\email{", author_email, "}"),
    "#'",
    paste0("#' @source \\url{", github_url, "}"),
    "#'",
    "#' @md",
    "#' @docType package",
    paste0("#' @name ", pkg_name, "-package"),
    "\"_PACKAGE\"",
    "",
    paste0("#' @title ", pkg_name, " logo"),
    "#'",
    "#' @description",
    paste0("#' The ", pkg_name, " logo, using ASCII or Unicode characters"),
    "#' Use [cli::ansi_strip()] to get rid of the colors.",
    "#' @param unicode Unicode symbols. Default is `TRUE` on UTF-8 platforms.",
    "#'",
    "#' @references",
    "#'  \\url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}",
    "#'",
    "#' @md",
    "#' @export",
    "#' @examples",
    paste0("#' ", pkg_name, "_logo()"),
    paste0(pkg_name, "_logo <- function("),
    "    unicode = cli::is_utf8_output()) {",
    "  logo <- c(",
    paste0("    \"", ascii_with_numbers, "\""),
    "  )",
    "",
    .generate_hexa(length(colors), colors, unicode),
    "",
    "  col_hexa <- purrr::map2(",
    "    hexa, cols, ~ cli::make_ansi_style(.y)(.x)",
    "  )",
    "",
    paste0("  for (i in 0:", length(colors) - 1, ") {"),
    "    pat <- paste0(\"\\\\b\", i, \"\\\\b\")",
    "    logo <- sub(pat, col_hexa[[i + 1]], logo)",
    "  }",
    "",
    "  structure(cli::col_blue(logo), class = \"logo\")",
    "}",
    "",
    "#' @title print logo",
    "#'",
    "#' @param x Input information.",
    "#' @param ... Other parameters.",
    "#' @method print logo",
    "#'",
    "#' @export",
    "print.logo <- function(x, ...) {",
    "  cat(x, ..., sep = \"\\n\")",
    "  invisible(x)",
    "}",
    "",
    ".onAttach <- function(libname, pkgname) {",
    "  version <- utils::packageDescription(pkgname, fields = \"Version\")",
    "",
    "  msg <- paste0(",
    "    \"-------------------------------------------------------",
    "\",",
    "    cli::col_blue(\" \", pkgname, \" version \", version),",
    "    \"",
    "   This message can be suppressed by:",
    paste0("     suppressPackageStartupMessages(library(", pkg_name, "))"),
    "-------------------------------------------------------\"",
    "  )",
    "",
    paste0("  packageStartupMessage(", pkg_name, "_logo())"),
    "  packageStartupMessage(msg)",
    "}"
  )

  return(content)
}

.add_color_numbers_to_ascii <- function(
    ascii_lines,
    num_colors) {
  if (length(ascii_lines) == 0) {
    return("")
  }

  top_numbers <- "       0        1      2           3    4"
  bottom_numbers <- "    5             6      7      8       9   "

  all_lines <- c(top_numbers, ascii_lines, bottom_numbers)
  result <- paste(all_lines, collapse = "\n")

  return(result)
}

.generate_hexa <- function(
    num_colors,
    colors,
    unicode) {
  symbols <- rep(c("*", ".", "o"), length.out = num_colors)

  code <- c(
    paste0("  hexa <- c(", paste0("\"", symbols, "\"", collapse = ", "), ")"),
    "  if (unicode) {",
    "    hexa <- c(\"*\" = \"\\u2b22\", \"o\" = \"\\u2b21\", \".\" = \".\")[hexa]",
    "  }",
    "",
    "  cols <- c(",
    paste0("    ", paste0("\"", colors, "\"", collapse = ", ")),
    "  )"
  )

  return(code)
}

.read_description <- function(
    desc_file,
    verbose = TRUE) {
  if (!file.exists(desc_file)) {
    log_message(
      "DESCRIPTION file not found",
      message_type = "error"
    )
  }

  if (verbose) {
    log_message(
      "Reading package information from file: ",
      desc_file
    )
  }

  desc_lines <- readLines(desc_file, warn = FALSE)
  desc_content <- paste(desc_lines, collapse = "\n")

  package_match <- regexpr(
    "Package:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )
  package_name <- if (package_match != -1) {
    trimws(
      regmatches(
        desc_content, package_match
      )
    )
    trimws(
      sub(
        "Package:\\s*", "",
        regmatches(desc_content, package_match)
      )
    )
  } else {
    log_message(
      "Package name not found in DESCRIPTION file",
      message_type = "warning"
    )
    return(NULL)
  }

  title_match <- regexpr(
    "Title:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )
  title <- if (title_match != -1) {
    trimws(
      sub(
        "Title:\\s*", "", regmatches(desc_content, title_match)
      )
    )
  } else {
    log_message(
      "Title not found in DESCRIPTION file",
      message_type = "warning"
    )
    return(
      NULL
    )
  }

  desc_match <- regexpr(
    "Description:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )
  description <- if (desc_match != -1) {
    trimws(
      sub(
        "Description:\\s*", "",
        regmatches(desc_content, desc_match)
      )
    )
  } else {
    title
  }

  authors_pattern <- "Authors@R:\\s*\\n?\\s*person\\([^)]*name\\s*=\\s*[\"']([^\"']+)[\"'][^)]*email\\s*=\\s*[\"']([^\"']+)[\"']"
  authors_match <- regexpr(
    authors_pattern,
    desc_content,
    perl = TRUE
  )

  if (authors_match != -1) {
    authors_text <- regmatches(
      desc_content,
      authors_match
    )

    name_pattern <- "name\\s*=\\s*[\"']([^\"']+)[\"']"
    name_match <- regexpr(
      name_pattern,
      authors_text,
      perl = TRUE
    )
    author_name <- if (name_match != -1) {
      sub(".*name\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", authors_text)
    } else {
      "Author"
    }

    email_match <- regexpr(
      "email\\s*=\\s*[\"']([^\"']+)[\"']",
      authors_text,
      perl = TRUE
    )
    author_email <- if (email_match != -1) {
      sub(".*email\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", authors_text)
    } else {
      "author@example.com"
    }
  } else {
    maintainer_match <- regexpr(
      "Maintainer:\\s*([^<]+)<([^>]+)>",
      desc_content,
      perl = TRUE
    )

    if (maintainer_match != -1) {
      maintainer_text <- regmatches(
        desc_content, maintainer_match
      )
      author_name <- trimws(
        sub(
          "Maintainer:\\s*([^<]+)<.*", "\\1",
          maintainer_text
        )
      )
      author_email <- sub(
        ".*<([^>]+)>.*", "\\1",
        maintainer_text
      )
    } else {
      author_name <- "Author"
      author_email <- "author@example.com"
    }
  }

  url_match <- regexpr(
    "URL:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )
  bug_reports_match <- regexpr(
    "BugReports:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )

  github_url <- NULL
  if (url_match != -1) {
    url <- trimws(sub("URL:\\s*", "", regmatches(desc_content, url_match)))
    github_url <- url
  } else if (bug_reports_match != -1) {
    bug_url <- trimws(
      sub("BugReports:\\s*", "", regmatches(desc_content, bug_reports_match))
    )
    github_url <- sub("/issues$", "", bug_url)
  }

  if (is.null(github_url) && !is.null(package_name)) {
    github_url <- paste0("https://github.com/username/", package_name)
  }

  if (verbose) {
    log_message(
      "Information extracted for: '", package_name,
      "', Author: ", author_name,
      message_type = "success"
    )
  }

  list(
    Package = package_name,
    Description = description,
    Author = author_name,
    Email = author_email,
    GitHub_URL = github_url
  )
}
