#' @title Check and install R packages
#'
#' @md
#' @inheritParams log_message
#' @param packages Package to be installed.
#' Package source can be *CRAN*, *Bioconductor* or *Github*.
#' By default, the package name is extracted according to the `packages` parameter.
#' @param lib The location of the library directories where to install the packages.
#' @param dependencies Which dependencies to install.
#' Passed to [pak::pkg_install].
#' Default is `NA`, auto install hard dependencies: *Depends*, *Imports*,
#' and *LinkingTo*, excluding *Suggests*.
#' @param force Whether to force the installation of packages.
#' Default is `FALSE`.
#' @param install Whether missing or outdated packages may be installed.
#' Set to `FALSE` for read-only diagnostics. Default is `TRUE` for backward
#' compatibility.
#' @param timeout Maximum installation time in seconds. A finite timeout runs
#' installation in a supervised R subprocess and terminates only that process
#' tree on timeout. Default is `Inf`.
#' @param load Whether to load packages after successful installation.
#' Uses [do.call] dispatch to avoid CRAN static checks on [base::library].
#' Default is `FALSE`.
#'
#' @return Package installation status.
#'
#' @export
check_r <- function(
  packages,
  lib = .libPaths()[1],
  dependencies = NA,
  force = FALSE,
  install = TRUE,
  timeout = Inf,
  load = FALSE,
  verbose = TRUE
) {
  if (!is.logical(install) || length(install) != 1L || is.na(install)) {
    stop("`install` must be TRUE or FALSE", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) || timeout <= 0) {
    stop("`timeout` must be one positive number or Inf", call. = FALSE)
  }
  status_list <- list()
  error_details <- list()
  for (pkg in packages) {
    version <- NULL
    if (grepl("/", pkg)) {
      pkg_name <- strsplit(pkg, split = "/|@|==", perl = TRUE)[[1]][[2]]
    } else {
      pkg_info <- strsplit(pkg, split = "@|==", perl = TRUE)[[1]]
      pkg_name <- pkg_info[[1]]
      if (length(pkg_info) > 1) {
        version <- pkg_info[[2]]
      }
    }
    check_pkg <- check_pkg_status(
      pkg_name,
      version = version,
      lib = lib
    )

    force_update <- FALSE
    if (check_pkg && !is.null(version)) {
      current_version <- utils::packageVersion(pkg_name)
      force_update <- current_version < package_version(version)
    }
    force_update <- force_update || isTRUE(force)

    if (!check_pkg || force_update) {
      if (!isTRUE(install)) {
        status_list[[pkg_name]] <- FALSE
        error_details[[pkg_name]] <- if (force_update && check_pkg) {
          "the installed version does not satisfy the request and installation is disabled"
        } else {
          "the package is unavailable and installation is disabled"
        }
        next
      }
      log_message(
        "Installing: {.pkg {pkg_name}}...",
        message_type = "running",
        verbose = verbose
      )
      status_list[[pkg_name]] <- FALSE
      tryCatch(
        expr = {
          if (!dir.exists(lib)) {
            dir.create(lib, recursive = TRUE, showWarnings = FALSE)
          }
          check_r_run_install(
            pkg = pkg,
            lib = lib,
            dependencies = dependencies,
            timeout = timeout,
            verbose = verbose
          )
        },
        error = function(e) {
          status_list[[pkg_name]] <- FALSE
          err_msg <- tryCatch(
            rlang::cnd_message(e, inherit = TRUE),
            error = function(...) conditionMessage(e)
          )
          error_details[[pkg_name]] <<- cli::ansi_strip(err_msg)
        }
      )
      status_list[[pkg_name]] <- check_pkg_status(
        pkg_name,
        version = version,
        lib = lib
      )
    } else {
      status_list[[pkg_name]] <- TRUE
    }
  }

  success <- sapply(status_list, isTRUE)
  failed <- names(status_list)[!success]

  if (length(failed) > 0) {
    for (pkg_name in failed) {
      err <- error_details[[pkg_name]]
      if (!is.null(err)) {
        log_message(
          "Failed to install: {.pkg {pkg_name}}. Error: {.val {err}}",
          message_type = "warning",
          verbose = verbose
        )
      } else {
        log_message(
          "Failed to install: {.pkg {pkg_name}}. Please install manually",
          message_type = "warning",
          verbose = verbose
        )
      }
    }
  } else {
    installed_names <- names(status_list)[success]
    log_message(
      "{.pkg {installed_names}} installed successfully",
      message_type = "success",
      verbose = verbose
    )
  }

  if (isTRUE(load)) {
    load_packages(
      names(status_list)[success],
      lib = lib,
      verbose = verbose
    )
  }

  return(invisible(status_list))
}

check_r_run_install <- function(
  pkg,
  lib,
  dependencies = NA,
  timeout = Inf,
  verbose = TRUE
) {
  process <- callr::r_bg(
    func = function(pkg, lib, dependencies, verbose) {
      install_call <- function() {
        pak::pkg_install(
          pkg,
          lib = lib,
          ask = FALSE,
          dependencies = dependencies
        )
      }
      if (isTRUE(verbose)) {
        install_call()
      } else {
        invisible(suppressMessages(install_call()))
      }
      invisible(TRUE)
    },
    args = list(
      pkg = pkg,
      lib = lib,
      dependencies = dependencies,
      verbose = verbose
    ),
    libpath = unique(c(lib, .libPaths())),
    stdout = "|",
    stderr = "|",
    supervise = TRUE,
    package = FALSE
  )
  on.exit({
    if (isTRUE(process$is_alive())) {
      process$kill_tree()
    }
  }, add = TRUE)

  timeout_ms <- if (is.finite(timeout)) as.integer(min(timeout * 1000, .Machine$integer.max)) else -1L
  process$wait(timeout = timeout_ms)
  if (isTRUE(process$is_alive())) {
    process$kill_tree()
    stop(
      sprintf("Package installation timed out after %s seconds: %s", format(timeout), pkg),
      call. = FALSE
    )
  }
  if (isTRUE(verbose)) {
    output <- process$read_all_output_lines()
    errors <- process$read_all_error_lines()
    if (length(output) > 0L) writeLines(output)
    if (length(errors) > 0L) writeLines(errors, con = stderr())
  }
  process$get_result()
  invisible(TRUE)
}

load_packages <- function(pkgs, lib = .libPaths(), verbose = TRUE) {
  for (pkg in pkgs) {
    result <- tryCatch(
      expr = {
        do.call(
          "library",
          list(
            pkg,
            lib.loc = lib,
            character.only = TRUE,
            quietly = !verbose
          )
        )
        TRUE
      },
      error = function(e) FALSE
    )
    if (!isTRUE(result)) {
      log_message(
        "Failed to load: {.pkg {pkg}}",
        message_type = "warning",
        verbose = verbose
      )
    } else {
      log_message(
        "Loaded: {.pkg {pkg}}",
        message_type = "success",
        verbose = verbose
      )
    }
  }
  invisible(NULL)
}

#' @title Check and remove R packages
#'
#' @md
#' @inheritParams log_message
#' @param packages Package to be removed.
#' @param lib The location of the library directories where to remove the packages.
#'
#' @export
remove_r <- function(
  packages,
  lib = .libPaths()[1],
  verbose = TRUE
) {
  status_list <- list()
  for (pkg in packages) {
    pkg_installed <- check_pkg_status(pkg, lib = lib)

    if (pkg_installed) {
      log_message(
        "Removing: {.pkg {pkg}}...",
        verbose = verbose
      )
      status_list[[pkg]] <- FALSE
      tryCatch(
        expr = {
          pak::pkg_remove(pkg, lib = lib)
        },
        error = function(e) {
          log_message(
            "Warning during removal: {.pkg {pkg}}. Error: {.val {e$message}}",
            message_type = "warning",
            verbose = verbose
          )
        }
      )
      status_list[[pkg]] <- !check_pkg_status(pkg, lib = lib)
    } else {
      log_message(
        "{.pkg {pkg}} is not installed, skipping removal",
        message_type = "warning",
        verbose = verbose
      )
      status_list[[pkg]] <- TRUE
    }
  }

  success <- sapply(status_list, isTRUE)
  failed <- names(status_list)[!success]

  if (length(failed) > 0) {
    log_message(
      "Failed to remove: {.pkg {failed}}. Please remove manually",
      message_type = "warning",
      verbose = verbose
    )
  } else {
    log_message(
      "{.pkg {packages}} removed successfully",
      message_type = "success",
      verbose = verbose
    )
  }

  return(invisible(status_list))
}

#' @title Check if a package is installed with the specified version
#'
#' @md
#' @inheritParams check_r
#' @param pkg Package name.
#' @param version Package version to check. If `NULL`, only checks if the package is installed.
#'
#' @return `TRUE` if the package is installed with the specified version, `FALSE` otherwise.
#'
#' @export
check_pkg_status <- function(
  pkg,
  version = NULL,
  lib = .libPaths()[1]
) {
  installed_pkgs_info <- utils::installed.packages(lib.loc = lib)
  installed_pkgs <- installed_pkgs_info[, "Package"]
  installed_pkgs_version <- installed_pkgs_info[, "Version"]
  pkg_exists <- pkg %in% installed_pkgs
  if (is.null(version)) {
    version_match <- TRUE
  } else {
    version_match <- installed_pkgs_version[installed_pkgs == pkg] == version
  }

  if (isFALSE(pkg_exists) || isFALSE(version_match)) {
    return(FALSE)
  }

  TRUE
}
