test_that("figlet produces figlet_text output", {
  result <- figlet("test")
  expect_true(inherits(result, "figlet_text"))
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("figlet_font loads default Slant font", {
  font <- figlet_font("Slant")
  expect_true(inherits(font, "figlet_font"))
  expect_equal(font$name, "Slant")
})

test_that("figlet_font returns same object if already figlet_font", {
  font <- figlet_font("Slant")
  font2 <- figlet_font(font)
  expect_identical(font, font2)
})

test_that("figlet_font errors on nonexistent font", {
  expect_error(figlet_font("NonExistentFont12345"))
})

test_that("print.figlet_text works", {
  result <- figlet("hi")
  expect_output(print(result))
})

test_that("as.character.figlet_text works", {
  result <- figlet("hi")
  s <- as.character(result)
  expect_true(is.character(s))
  expect_equal(length(s), 1)
})

test_that("figlet handles multiline input", {
  result <- figlet("a\nb")
  expect_true(inherits(result, "figlet_text"))
})
