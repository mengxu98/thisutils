test_that("non-interactive attach does not emit startup messages", {
  testthat::local_mocked_bindings(
    interactive = function() FALSE,
    .package = "base"
  )

  expect_silent(thisutils:::.onAttach(NULL, "thisutils"))
})

test_that("interactive attach honors verbose = FALSE", {
  old <- options(log_message.verbose = FALSE)
  on.exit(options(old), add = TRUE)

  testthat::local_mocked_bindings(
    interactive = function() TRUE,
    .package = "base"
  )

  expect_silent(thisutils:::.onAttach(NULL, "thisutils"))
})
