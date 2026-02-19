#' @title Check and install R packages
#'
#' @md
#' @inheritParams log_message
#' @param packages Package to be installed.
#' Package source can be *CRAN*, *Bioconductor* or *Github*.
#' By default, the package name is extracted according to the `packages` parameter.
#' @param lib The location of the library directories where to install the packages.
#' @param dependencies Whether to install dependencies of the packages.
#' Default is `TRUE`.
#' @param force Whether to force the installation of packages.
#' Default is `FALSE`.
#'
#' @return Package installation status.
#'
#' @export
check_r <- function(
    packages,
    lib = .libPaths()[1],
    dependencies = TRUE,
    force = FALSE,
    verbose = TRUE) {
  status_list <- list()
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
      log_message(
        "Installing: {.pkg {pkg_name}}...",
        message_type = "running",
        verbose = verbose
      )
      status_list[[pkg]] <- FALSE
      tryCatch(
        expr = {
          old_lib_paths <- .libPaths()
          .libPaths(lib)
          if (isTRUE(verbose)) {
            pak::pak(
              pkg,
              lib = lib,
              dependencies = dependencies
            )
          } else {
            invisible(
              suppressMessages(
                pak::pak(
                  pkg,
                  lib = lib,
                  dependencies = dependencies
                )
              )
            )
          }
          .libPaths(old_lib_paths)
        },
        error = function(e) {
          status_list[[pkg]] <- FALSE
          log_message(
            "Failed to install: {.pkg {pkg_name}}. Error: {.val {e$message}}",
            message_type = "warning",
            verbose = verbose
          )
        }
      )
      status_list[[pkg]] <- check_pkg_status(
        pkg_name,
        version = version,
        lib = lib
      )
    } else {
      status_list[[pkg]] <- TRUE
    }
  }

  success <- sapply(status_list, isTRUE)
  failed <- names(status_list)[!success]

  if (length(failed) > 0) {
    log_message(
      "Failed to install: {.pkg {failed}}. Please install manually",
      message_type = "warning",
      verbose = verbose
    )
  } else {
    log_message(
      "{.pkg {packages}} installed successfully",
      message_type = "success",
      verbose = verbose
    )
  }

  return(invisible(status_list))
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
    verbose = TRUE) {
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
          old_lib_paths <- .libPaths()
          .libPaths(lib)
          pak::pkg_remove(pkg)
          .libPaths(old_lib_paths)
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

check_pkg_status <- function(pkg, version = NULL, lib = .libPaths()[1]) {
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
