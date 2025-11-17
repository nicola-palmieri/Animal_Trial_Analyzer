library(testthat)
library(openxlsx)
source(testthat::test_path("..", "R", "module_upload_helpers.R"))

make_workbook <- function(matrix_rows) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(matrix_rows, path, colNames = FALSE)
  path
}

test_that("convert_wide_to_long handles blank separator columns", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Pad", "", "Measurement", "Measurement"),
    c("", "", "", "", "R1", "R2"),
    c("1", "A", "x", "", "5", "6"),
    c("2", "B", "y", "", "7", "8")
  ))

  result <- convert_wide_to_long(path)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Sample", "Group", "Replicate", "Measurement") %in% names(result)))
  expect_equal(nrow(result), 4)
  expect_equal(result$Measurement[result$Sample == "1" & result$Replicate == "R2"], 6)
})


test_that("convert_wide_to_long flags duplicate measurements per replicate", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Measurement", ""),
    c("", "", "R1", "R1"),
    c("1", "A", "3", "4")
  ))

  expect_error(convert_wide_to_long(path), "Duplicate measurements detected")
})


test_that("convert_wide_to_long tolerates extra header rows and empty trailing columns", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Measurement", "Measurement", ""),
    c("", "", "R1", "R2", ""),
    c("Notes", "Ignore", "", "", ""),
    c("3", "C", "7.5", NA, "")
  ))

  result <- convert_wide_to_long(path)

  expect_equal(nrow(result), 2)
  expect_true(any(is.na(result$Measurement)))
  expect_true(all(result$Replicate %in% c("R1", "R2")))
})
