test_that("check_r loads each successful package from the requested library", {
  loaded <- NULL
  loaded_lib <- NULL

  testthat::local_mocked_bindings(
    check_pkg_status = function(pkg, version = NULL, lib = NULL) {
      TRUE
    },
    load_packages = function(pkgs, lib = .libPaths(), verbose = TRUE) {
      loaded <<- pkgs
      loaded_lib <<- lib
      invisible(NULL)
    },
    .package = "thisutils"
  )

  lib <- tempfile("thisutils-library-")
  status <- check_r(
    c("foo", "foo", "bar"),
    lib = lib,
    load = TRUE,
    verbose = FALSE
  )

  expect_identical(status, list(foo = TRUE, bar = TRUE))
  expect_identical(loaded, c("foo", "bar"))
  expect_identical(loaded_lib, lib)
})
