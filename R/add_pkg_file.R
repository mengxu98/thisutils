#' @title Add package file
#'
#' @description
#' Automatically generate a file containing functions and related code for R package development.
#'
#' @md
#' @param use_figlet Whether to use figlet for ASCII art generation.
#' Default is `TRUE`. Details see [figlet].
#' @param figlet_font Character string, figlet font to use.
#' Default is `"Slant"`.
#' @param colors Character vector, colors to use for the logo elements.
#' @param unicode Whether to use Unicode symbols.
#' Default is `TRUE`.
#' @param verbose Whether to print progress messages.
#' Default is `TRUE`.
#'
#' @return
#' Creates a file in specified output directory.
#'
#' @export
add_pkg_file <- function(
    use_figlet = TRUE,
    figlet_font = "Slant",
    colors = c(
      "red", "yellow", "green", "magenta", "cyan",
      "yellow", "green", "white", "magenta", "cyan"
    ),
    unicode = TRUE,
    verbose = TRUE) {
  desc_file <- "DESCRIPTION"
  pkgdown_file <- "_pkgdown.yml"

  desc_info <- .read_description(desc_file, verbose)

  pkg_name <- desc_info$Package
  title <- desc_info$Title
  pkg_description <- desc_info$Description
  author_name <- desc_info$Author
  author_email <- desc_info$Email
  github_url <- desc_info$GitHub_URL

  if (is.null(pkg_name)) {
    log_message(
      "{.cls package_name} not found in {.file {desc_file}}.
      Please add 'Package: {.cls package_name}'",
      message_type = "error"
    )
  }
  if (is.null(title)) {
    log_message(
      "{.cls package_title} not found in {.file {desc_file}}.
      Please add 'Title: {.cls package_title}'",
      message_type = "error"
    )
  }
  if (is.null(author_name)) {
    log_message(
      "{.cls author_name} not found in {.file {desc_file}}.
      Please add 'Authors@R:' or 'Maintainer:' field",
      message_type = "error"
    )
  }
  if (is.null(author_email)) {
    log_message(
      "{.cls author_email} not found in {.file {desc_file}}.
      Please add email to 'Authors@R:' or 'Maintainer:' field",
      message_type = "error"
    )
  }

  log_message(
    "Creating logo for: {.pkg {pkg_name}}",
    verbose = verbose
  )

  ascii_lines <- NULL
  if (use_figlet) {
    tryCatch(
      {
        ascii_art <- figlet(
          pkg_name,
          font = figlet_font,
          width = 60,
          justify = "centre",
          absolute = TRUE
        )
        ascii_lines <- as.character(ascii_art)
        log_message(
          "Generated ASCII art logo successfully",
          message_type = "success",
          verbose = verbose
        )
        if (verbose) {
          print(ascii_art)
        }
      },
      error = function(e) {
        log_message(
          "ASCII art logo generation failed, using simple logo",
          message_type = "warning",
          verbose = verbose
        )
      }
    )
  }

  if (is.null(ascii_lines)) {
    ascii_lines <- c(
      paste0(
        "  ", paste0(rep("*", nchar(pkg_name) + 4))
      ),
      paste0(
        "  * ", pkg_name, " *"
      ),
      paste0(
        "  ", paste0(rep("*", nchar(pkg_name) + 4))
      )
    )
  }

  has_src_dir <- dir.exists("src")

  file_content <- generate_content(
    pkg_name = pkg_name,
    title = title,
    pkg_description = pkg_description,
    author_name = author_name,
    author_email = author_email,
    github_url = github_url,
    ascii_lines = ascii_lines,
    colors = colors,
    unicode = unicode,
    has_src_dir = has_src_dir
  )

  output_file <- file.path(
    paste0("R/", pkg_name, "-package.R")
  )
  writeLines(file_content, output_file)
  log_message(
    "Written to {.file {output_file}}",
    message_type = "success",
    verbose = verbose
  )
  .check_dependency(desc_file, verbose)
  .check_pkgdown(pkgdown_file, pkg_name, verbose)

  invisible(file_content)
}

generate_content <- function(
    pkg_name,
    title,
    pkg_description,
    author_name,
    author_email,
    github_url,
    ascii_lines,
    colors,
    unicode,
    has_src_dir = FALSE) {
  ascii_with_numbers <- .add_color_numbers(
    ascii_lines,
    length(colors)
  )

  use_dynlib_line <- if (has_src_dir) {
    c(
      paste0("#' @useDynLib ", pkg_name),
      "#'"
    )
  } else {
    NULL
  }

  content <- c(
    "# -*- coding: utf-8 -*-",
    "",
    paste0("#' @title ", title),
    "#'",
    use_dynlib_line,
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
    "#' @return",
    paste0(
      "#' A character vector with class `", tolower(pkg_name), "_logo`."
    ),
    "#'",
    "#' @references",
    "#' \\url{https://github.com/tidyverse/tidyverse/blob/main/R/logo.R}",
    "#'",
    "#' @export",
    "#' @examples",
    paste0("#' ", tolower(pkg_name), "_logo()"),
    paste0(
      tolower(pkg_name),
      "_logo <- function(unicode = cli::is_utf8_output()) {"
    ),
    "  logo <- c(",
    paste0(
      "    \"", ascii_with_numbers, "\""
    ),
    "  )",
    "",
    .generate_hexa(length(colors), colors, unicode),
    "",
    "  col_hexa <- mapply(",
    "    function(x, y) cli::make_ansi_style(y)(x),",
    "    hexa, cols,",
    "    SIMPLIFY = FALSE",
    "  )",
    "",
    paste0(
      "  for (i in 0:", length(colors) - 1, ") {"
    ),
    "    pat <- paste0(\"\\\\b\", i, \"\\\\b\")",
    "    logo <- sub(pat, col_hexa[[i + 1]], logo)",
    "  }",
    "",
    "  structure(",
    "    cli::col_blue(logo),",
    paste0(
      "    class = \"", tolower(pkg_name), "_logo\""
    ),
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
    paste0(
      "#' @method print ", tolower(pkg_name), "_logo"
    ),
    "#'",
    "#' @export",
    "#'",
    paste0(
      "print.", tolower(pkg_name), "_logo <- function(x, ...) {"
    ),
    "  cat(x, ..., sep = \"\\n\")",
    "  invisible(x)",
    "}",
    "",
    ".onAttach <- function(libname, pkgname) {",
    "  verbose <- thisutils::get_verbose()",
    "  if (isTRUE(verbose)) {",
    "    version <- utils::packageDescription(",
    "      pkgname,",
    "      fields = \"Version\"",
    "    )",
    "",
    "    msg <- paste0(",
    "      cli::col_grey(strrep(\"-\", 60)),",
    "      \"\\n\",",
    "      cli::col_blue(pkgname, \" version \", version),",
    "      \"\\n\",",
    "      cli::col_grey(\"This message can be suppressed by:\"),",
    "      \"\\n\",",
    paste0(
      "      cli::col_grey(\"  suppressPackageStartupMessages(library(",
      pkg_name, "))\"),"
    ),
    "      \"\\n\",",
    paste0(
      "      cli::col_grey(\"  or options(log_message.verbose = FALSE)\"),"
    ),
    "      \"\\n\",",
    "      cli::col_grey(strrep(\"-\", 60))",
    "    )",
    "",
    paste0(
      "    packageStartupMessage(",
      tolower(pkg_name), "_logo())"
    ),
    "    packageStartupMessage(msg)",
    "  }",
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

  top_numbers <- "          0          1        2             3     4"
  bottom_numbers <- "      5               6      7        8          9"

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

  log_message(
    "Reading package information from {.file {desc_file}}",
    verbose = verbose
  )

  desc_lines <- readLines(desc_file, warn = FALSE)
  desc_content <- paste(desc_lines, collapse = "\n")

  package_match <- regexpr(
    "Package:\\s*([^\\n]+)",
    desc_content,
    perl = TRUE
  )
  package_name <- if (package_match != -1) {
    trimws(
      sub(
        "Package:\\s*", "",
        regmatches(desc_content, package_match)
      )
    )
  } else {
    NULL
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
    NULL
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
    "Package: {.pkg {package_name}}\n",
    "Title: {.pkg {title}}\n",
    "Description: {.pkg {description}}\n",
    "Author: {.pkg {author_name}}, {.email {author_email}}\n",
    "GitHub URL: {.url {github_url}}\n",
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

.check_dependency <- function(desc_file, verbose = TRUE) {
  if (!file.exists(desc_file)) {
    log_message(
      "{.file {desc_file}} not found",
      message_type = "error"
    )
    return()
  }

  desc_content <- readLines(desc_file, warn = FALSE)

  imports_start <- which(grepl("^Imports:", desc_content))
  if (length(imports_start) == 0) {
    log_message(
      "No {.cls Imports} section found in {.file {desc_file}}",
      message_type = "warning",
      verbose = verbose
    )
    return()
  }

  imports_end <- imports_start
  for (i in (imports_start + 1):length(desc_content)) {
    if (grepl("^[A-Za-z]", desc_content[i]) && !grepl("^\\s", desc_content[i])) {
      imports_end <- i - 1
      break
    }
    if (i == length(desc_content)) {
      imports_end <- length(desc_content)
    }
  }

  imports_section <- desc_content[imports_start:imports_end]
  has_cli <- any(grepl("\\bcli\\b", imports_section))

  if (!has_cli) {
    log_message(
      "Adding cli dependency to {.file {desc_file}}",
      verbose = verbose
    )

    if (imports_end == imports_start) {
      desc_content[imports_start] <- paste0(
        desc_content[imports_start], "\n    cli"
      )
    } else {
      desc_content[imports_end] <- paste0(
        desc_content[imports_end], "\n    cli"
      )
    }

    writeLines(desc_content, desc_file)
    log_message(
      "Successfully added {.pkg cli} to {.file {desc_file}}",
      message_type = "success",
      verbose = verbose
    )
  } else {
    log_message(
      "{.pkg cli} already present in {.file {desc_file}}",
      verbose = verbose
    )
  }
}

.check_pkgdown <- function(pkgdown_file, pkg_name, verbose = TRUE) {
  if (!file.exists(pkgdown_file)) {
    log_message(
      "{.file {pkgdown_file}} not found",
      message_type = "warning",
      verbose = verbose
    )
    return()
  }

  pkgdown_content <- readLines(pkgdown_file, warn = FALSE)

  reference_start <- which(grepl("^reference:", pkgdown_content))
  if (length(reference_start) == 0) {
    log_message(
      "No {.cls reference} section found in {.file {pkgdown_file}}",
      message_type = "warning",
      verbose = verbose
    )
    return()
  }

  has_subtitle <- any(
    grepl('subtitle: "Package overview"', pkgdown_content)
  )
  has_package_entry <- any(
    grepl(paste0("- ", pkg_name, "-package"), pkgdown_content)
  )
  has_logo_entry <- any(
    grepl(paste0("- ", tolower(pkg_name), "_logo"), pkgdown_content)
  )

  if (!has_subtitle || !has_package_entry || !has_logo_entry) {
    log_message(
      "Updating {.file {pkgdown_file}} with package overview section",
      message_type = "running",
      verbose = verbose
    )

    insert_pos <- reference_start + 1

    new_content <- c()
    if (!has_subtitle) {
      new_content <- c(new_content, "", "- subtitle: \"Package overview\"")
    }
    if (!has_package_entry || !has_logo_entry) {
      new_content <- c(new_content, "- contents:")
      if (!has_package_entry) {
        new_content <- c(
          new_content, paste0("  - ", pkg_name, "-package")
        )
      }
      if (!has_logo_entry) {
        new_content <- c(
          new_content, paste0("  - ", tolower(pkg_name), "_logo")
        )
      }
    }

    if (length(new_content) > 0) {
      updated_content <- c(
        pkgdown_content[1:insert_pos],
        new_content,
        pkgdown_content[(insert_pos + 1):length(pkgdown_content)]
      )

      writeLines(updated_content, pkgdown_file)
      log_message(
        "Successfully updated {.file {pkgdown_file}}",
        message_type = "success",
        verbose = verbose
      )
    }
  } else {
    log_message(
      "Package overview section already present in {.file {pkgdown_file}}",
      verbose = verbose
    )
  }
}
