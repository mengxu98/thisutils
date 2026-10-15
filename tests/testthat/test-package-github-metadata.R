test_that("pak GitHub source metadata is recognized without reinstalling", {
  description <- list(
    RemotePkgRef = "github::cole-trapnell-lab/monocle3@536f103",
    RemoteUsername = "cole-trapnell-lab", RemoteRepo = "monocle3",
    RemoteRef = "HEAD", RemoteSha = "536f1033d6de7c957f26a1f403f81efbd825e0db"
  )
  original_description <- utils::packageDescription
  testthat::local_mocked_bindings(packageDescription = function(pkg, ...) {
    if (identical(pkg, "monocle3")) description else original_description(pkg, ...)
  }, .package = "utils")
  testthat::local_mocked_bindings(check_pkg_status = function(...) TRUE, .package = "thisutils")
  installed <- FALSE
  testthat::local_mocked_bindings(pkg_install = function(...) {
    installed <<- TRUE
    invisible(NULL)
  }, .package = "pak")
  expect_identical(check_r("cole-trapnell-lab/monocle3", verbose = FALSE), list(monocle3 = TRUE))
  expect_false(installed)
  expect_identical(check_r("cole-trapnell-lab/monocle3", install = FALSE, verbose = FALSE), list(monocle3 = TRUE))

  remote_status <- getFromNamespace("check_r_remote_status", "thisutils")
  info <- list(name = "monocle3", remote = "cole-trapnell-lab/monocle3", ref = "536f103")
  expect_true(remote_status(info))
  info$ref <- "different-ref"
  expect_false(remote_status(info))
  info$ref <- NULL
  info$remote <- "other-owner/monocle3"
  expect_false(remote_status(info))
})

test_that("plain and pak GitHub repository identities retain the same matching rules", {
  remote_status <- getFromNamespace("check_r_remote_status", "thisutils")
  ref <- "owner/example"
  testthat::local_mocked_bindings(packageDescription = function(...) list(RemotePkgRef = ref), .package = "utils")
  info <- list(name = "example", remote = "owner/example", ref = NULL)
  for (value in c("owner/example", "github::owner/example", "GITHUB::Owner/Example.git@main")) {
    ref <- value
    expect_true(remote_status(info))
  }
  ref <- "gitlab::owner/example"
  expect_false(remote_status(info))
})
