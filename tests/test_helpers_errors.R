library(testthat)
source(testthat::test_path("..", "R", "helpers_errors.R"))


test_that("format_safe_error_message handles empty titles and list details", {
  msg <- format_safe_error_message("", list(" Line1 ", "Line2"))
  expect_equal(msg, "Error:\nLine1\nLine2")

  condition_msg <- format_safe_error_message("Alert", simpleError("failed"))
  expect_equal(condition_msg, "Alert:\nfailed")
})


test_that("validate_numeric_columns ignores missing inputs and fails on non-numeric", {
  expect_invisible(validate_numeric_columns(NULL, NULL))

  df <- data.frame(a = 1:3, b = letters[1:3])
  expect_error(
    validate_numeric_columns(df, c("a", "b"), context_label = "responses"),
    "must be numeric"
  )
})
