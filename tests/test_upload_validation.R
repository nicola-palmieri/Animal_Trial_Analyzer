library(testthat)
library(openxlsx)
source("R/module_upload_helpers.R")

make_workbook <- function(matrix_rows) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(matrix_rows, path, colNames = FALSE)
  path
}

test_that("convert_wide_to_long reshapes wide data and preserves NA", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Measurement", ""),
    c("", "", "R1", "R2"),
    c("1", "A", "1.5", "2.5"),
    c("2", "B", "3.0", NA)
  ))

  result <- convert_wide_to_long(path)

  expect_s3_class(result, "tbl_df")
  expect_equal(sort(names(result)), sort(c("Sample", "Group", "Replicate", "Measurement")))
  expect_equal(nrow(result), 4)
  expect_true(any(is.na(result$Measurement)))
  expect_equal(result$Measurement[result$Sample == "1" & result$Replicate == "R1"], 1.5)
})

test_that("convert_wide_to_long detects duplicate measurements per replicate", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Measurement", ""),
    c("", "", "R1", "R2"),
    c("1", "A", "3", "4"),
    c("1", "A", "5", "6")
  ))

  expect_error(convert_wide_to_long(path), "Duplicate measurements detected")
})

test_that("convert_wide_to_long tolerates extra header rows and empty cells", {
  path <- make_workbook(rbind(
    c("Sample", "Group", "Measurement", ""),
    c("", "", "R1", "R2"),
    c("Notes", "Ignore", "", ""),
    c("3", "C", "7.5", "8.5")
  ))

  result <- convert_wide_to_long(path)

  expect_equal(nrow(result), 4)
  expect_true(any(is.na(result$Measurement)))
  expect_true(all(result$Replicate %in% c("R1", "R2")))
})


test_that("preprocess_uploaded_table orders factors numerically and keeps NAs", {
  df <- data.frame(
    sample_id = 1:4,
    treatment = c("T2", "T10", "T1", NA),
    batch = rep("Only", 4),
    stringsAsFactors = FALSE
  )

  processed <- preprocess_uploaded_table(df)

  expect_s3_class(processed$treatment, "factor")
  expect_equal(levels(processed$treatment), c("T1", "T2", "T10"))
  expect_true(any(is.na(processed$treatment)))
  expect_equal(levels(processed$batch), "Only")
})
