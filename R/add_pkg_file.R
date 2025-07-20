#' @title Add package file
#'
#' @description
#' Automatically generate a file containing functions and related code for R package development.
#'
#' @md
#' @param desc_file The DESCRIPTION file.
#' Must be provided, it will be used to extract package information.
#' Using `add_pkg_file("DESCRIPTION", output_dir = "R")`,
#' will be created <pkg_name>-package.R based on the DESCRIPTION file in `R` dir.
#' If you want to use some specific information,
#' such as `author_name` or `author_email`, you can provide them manually.
#' @param pkg_name Character string, the name of the package.
#' Default is NULL, which will be read from DESCRIPTION file.
#' @param title Character string, title of the package.
#' Default is NULL, which will be read from DESCRIPTION file.
#' @param pkg_description Character string, short description of the package.
#' Default is NULL, which will be read from DESCRIPTION file.
#' @param author_name Character string, name of the package author.
#' Default is NULL, which will be read from DESCRIPTION file.
#' @param author_email Character string, email of the package author.
#' Default is NULL, which will be read from DESCRIPTION file.
#' @param github_url Character string, GitHub URL of the package.
#' Default is NULL, which will be read from DESCRIPTION file or constructed based on package name.
#' @param output_dir Character string, directory where to save the package file.
#' Default is NULL, you should specify it, such as 'R'.
#' @param use_figlet Logical, whether to use figlet for ASCII art generation.
#' Default is TRUE. Details see [figlet].
#' @param figlet_font Character string, figlet font to use.
#' Default is "Slant".
#' @param colors Character vector, colors to use for the logo elements.
#' @param unicode Logical, whether to use Unicode symbols.
#' Default is TRUE.
#' @param verbose Logical, whether to print progress messages.
#' Default is TRUE.
#'
#' @return Creates a file in specified output directory
#'
#' @export
add_pkg_file <- function(
    desc_file,
    pkg_name = NULL,
    title = NULL,
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
        "Package name not provided and not found in 'DESCRIPTION' file",
        message_type = "error"
      )
    }
  }
  if (is.null(title)) {
    title <- desc_info$Title
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

  log_message(
    "Creating package logo for: {.pkg {pkg_name}}",
    verbose = verbose
  )

  ascii_lines <- NULL
  if (use_figlet) {
    tryCatch(
      {
        ascii_art <- figlet(pkg_name, font = figlet_font)
        ascii_lines <- as.character(ascii_art)
        log_message(
          "Generated figlet ASCII art successfully",
          message_type = "success",
          verbose = verbose
        )
      },
      error = function(e) {
        log_message(
          "Figlet generation failed, using simple ASCII art",
          message_type = "warning",
          verbose = verbose
        )
      }
    )
  }

  if (is.null(ascii_lines)) {
    ascii_lines <- c(
      paste0(
        "  ", paste(rep("*", nchar(pkg_name) + 4), collapse = "")
      ),
      paste0(
        "  * ", pkg_name, " *"
      ),
      paste0(
        "  ", paste(rep("*", nchar(pkg_name) + 4), collapse = "")
      )
    )
  }

  file_content <- .generate_content(
    pkg_name = pkg_name,
    title = title,
    pkg_description = pkg_description,
    author_name = author_name,
    author_email = author_email,
    github_url = github_url,
    ascii_lines = ascii_lines,
    colors = colors,
    unicode = unicode
  )

  if (!is.null(output_dir)) {
    output_file <- file.path(
      output_dir,
      paste0(pkg_name, "-package.R")
    )
    writeLines(file_content, output_file)
    log_message(
      "Package file written to: {.file {output_file}}",
      message_type = "success",
      verbose = verbose
    )
    invisible(file_content)
  } else {
    log_message(
      "Not provided {.arg output_dir}, please specify it, such as {.val R}",
      message_type = "warning",
      verbose = verbose
    )
    invisible(file_content)
  }
}

.generate_content <- function(
    pkg_name,
    title,
    pkg_description,
    author_name,
    author_email,
    github_url,
    ascii_lines,
    colors,
    unicode) {
  ascii_with_numbers <- .add_color_numbers(
    ascii_lines,
    length(colors)
  )

  content <- c(
    "# -*- coding: utf-8 -*-",
    "",
    paste0("#' @title ", title),
    "#'",
    paste0("#' @useDynLib ", pkg_name),
    "#'",
    "#' @description",
    paste0("#' ", pkg_description),
    "#'",
    paste0(
      "#' @author ", author_name, " (Maintainer), \\email{", author_email, "}"
    ),
    "#'",
    paste0("#' @source \\url{", github_url, "}"),
    "#'",
    "#' @md",
    "#' @docType package",
    paste0("#' @name ", pkg_name, "-package"),
    "\"_PACKAGE\"",
    "",
    paste0("#' @title The logo of ", pkg_name),
    "#'",
    "#' @description",
    paste0(
      "#' The ", pkg_name, " logo, using ASCII or Unicode characters"
    ),
    "#' Use [cli::ansi_strip] to get rid of the colors.",
    "#'",
    "#' @md",
    "#' @param unicode Unicode symbols on UTF-8 platforms.",
    "#' Default is [cli::is_utf8_output].",
    "#'",
    "#' @references",
    "#' \\url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}",
    "#'",
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
    "  structure(",
    "    cli::col_blue(logo),",
    paste0("    class = \"", pkg_name, "_logo\""),
    "  )",
    "}",
    "",
    "#' @title Print logo",
    "#'",
    "#' @param x Input information.",
    "#' @param ... Other parameters.",
    "#'",
    "#' @return Print the ASCII logo",
    "#'",
    paste0("#' @method print ", pkg_name, "_logo"),
    "#'",
    "#' @export",
    "#'",
    paste0("print.", pkg_name, "_logo <- function(x, ...) {"),
    "  cat(x, ..., sep = \"\\n\")",
    "  invisible(x)",
    "}",
    "",
    ".onAttach <- function(libname, pkgname) {",
    "  version <- utils::packageDescription(pkgname, fields = \"Version\")",
    "",
    "  msg <- paste0(",
    "    strrep(\"-\", 60),",
    "    \"\\n\",",
    "    cli::col_blue(pkgname, \" version \", version),",
    "    \"\\n\",",
    "    cli::col_grey(\"This message can be suppressed by:\"),",
    "    \"\\n\",",
    paste0(
      "    cli::col_grey(\"  suppressPackageStartupMessages(library(", pkg_name, "))\"),"
    ),
    "    \"\\n\",",
    "    strrep(\"-\", 60)",
    "  )",
    "",
    paste0("  packageStartupMessage(", pkg_name, "_logo())"),
    "  packageStartupMessage(msg)",
    "}"
  )

  content
}

.add_color_numbers <- function(
    ascii_lines,
    num_colors) {
  if (length(ascii_lines) == 0) {
    return("")
  }

  top_numbers <- "    0        1      2           3    4"
  bottom_numbers <- "  5             6      7      8       9"

  all_lines <- c(top_numbers, ascii_lines, bottom_numbers)
  result <- paste(all_lines, collapse = "\n")

  result
}

.generate_hexa <- function(
    num_colors,
    colors,
    unicode) {
  symbols <- rep(c("*", ".", "o"), length.out = num_colors)

  code <- c(
    paste0(
      "  hexa <- c(", paste0("\"", symbols, "\"", collapse = ", "), ")"
    ),
    "  if (unicode) {",
    "    hexa <- c(\"*\" = \"\\u2b22\", \"o\" = \"\\u2b21\", \".\" = \".\")[hexa]",
    "  }",
    "",
    "  cols <- c(",
    paste0(
      "    ",
      paste0(
        "\"", colors[1:(num_colors / 2)], "\"",
        collapse = ", "
      ), ","
    ),
    paste0(
      "    ",
      paste0(
        "\"", colors[(num_colors / 2 + 1):num_colors], "\"",
        collapse = ", "
      )
    ),
    "  )"
  )

  code
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
    url <- trimws(
      sub("URL:\\s*", "", regmatches(desc_content, url_match))
    )
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

  log_message(
    "Information extracted for: {.pkg {package_name}}, ",
    "Author: {.pkg {author_name}}, {.email {author_email}}",
    message_type = "success",
    verbose = verbose
  )

  list(
    Package = package_name,
    Title = title,
    Description = description,
    Author = author_name,
    Email = author_email,
    GitHub_URL = github_url
  )
}
