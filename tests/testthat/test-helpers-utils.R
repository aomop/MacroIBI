test_that("reactive_handler handles plain numerics and non-numerics", {
  expect_equal(reactive_handler(5), 5)
  expect_equal(reactive_handler(c(1, 2)), c(1, 2))
  
  # Non-numeric / NULL → 0
  expect_equal(reactive_handler("not numeric"), 0)
  expect_equal(reactive_handler(NULL), 0)
})

test_that("safe_reactive_value returns value from a callable or default", {
  # Use simple functions as stand-ins for reactives
  good_fun <- function() 42
  null_fun <- function() NULL
  bad_fun  <- function() stop("boom")
  
  expect_equal(safe_reactive_value(good_fun, default = 0), 42)
  expect_equal(safe_reactive_value(null_fun, default = 99), 99)
  expect_equal(safe_reactive_value(bad_fun,  default = -1), -1)
})

test_that("add_ordinal_suffix works for Date input", {
  dates <- as.Date(c(
    "2024-01-01", "2024-01-02", "2024-01-03",
    "2024-01-04", "2024-01-11", "2024-01-13", "2024-01-21"
  ))
  out <- add_ordinal_suffix(dates)
  
  expect_equal(
    out,
    c(
      "January 1st, 2024",
      "January 2nd, 2024",
      "January 3rd, 2024",
      "January 4th, 2024",
      "January 11th, 2024",
      "January 13th, 2024",
      "January 21st, 2024"
    )
  )
})

test_that("add_ordinal_suffix parses character dates with style", {
  chars <- c("2024-01-01", "2024-01-02")
  out <- add_ordinal_suffix(chars, style = "%Y-%m-%d")
  expect_equal(out, c("January 1st, 2024", "January 2nd, 2024"))
})

test_that("add_ordinal_suffix errors on unsupported input and bad style", {
  # Non-Date, non-character
  expect_error(add_ordinal_suffix(123))
  
  # Character without style
  expect_error(add_ordinal_suffix("2024-01-01"))
  
  # Bad style that cannot parse the date
  expect_error(add_ordinal_suffix("2024/01/01", style = "%Y-%m-%d"))
})

test_that("escape_latex escapes common special characters", {
  x <- "50% & 1_2"
  y <- escape_latex(x)
  
  # Expect backslash-escaped percent, ampersand, underscore
  expect_equal(y, "50\\% \\& 1\\_2")
})

test_that("escape_latex replaces apostrophes with text macros", {
  x <- "Bob's folder \\ path"
  y <- escape_latex(x)
  
  # `'` should be gone, and textquotesingle macro present
  expect_false(grepl("'", y, fixed = TRUE))
  expect_true(grepl("textquotesingle", y, fixed = TRUE))
})
