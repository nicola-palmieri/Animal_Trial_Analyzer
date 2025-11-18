library(testthat)
library(openxlsx)
library(dplyr)
library(readxl)
library(tidyr)
library(zoo)
library(purrr)

source("../R/module_upload_helpers.R")

test_that("convert_wide_to_long behaves correctly across 20 Excel scenarios", {
  
  dir.create("test_excels", showWarnings = FALSE)
  
  write_test_excel <- function(data, name, index) {
    filename <- sprintf("%02d_%s.xlsx", index, name)
    path <- file.path("test_excels", filename)
    openxlsx::write.xlsx(data, path, colNames = FALSE)
    path
  }
  
  tests <- list(
    list("simple_clean", rbind(
      c("ID", "Group", "A", "B"),
      c("", "", "Rep1", "Rep2"),
      c("1", "Ctrl", "5", "6"),
      c("2", "Treat", "7", "8")
    )),
    list("no_second_header", rbind(
      c("ID", "Group", "A_Rep1", "A_Rep2"),
      c("", "", "", ""),
      c("1", "Ctrl", "10", "12")
    )),
    list("irregular_merged", rbind(
      c("ID", "Group", "A", "", "B", ""),
      c("", "", "Rep1", "Rep2", "Rep1", "Rep2"),
      c("1", "Ctrl", "1.2", "1.3", "5.1", "5.2")
    )),
    list("blank_first_row", rbind(
      c("", "", "", ""),
      c("ID", "Group", "A_Rep1", "A_Rep2"),
      c("1", "Ctrl", "5", "7")
    )),
    list("duplicate_subheaders", rbind(
      c("ID", "Group", "A", "A"),
      c("", "", "Rep1", "Rep1"),
      c("1", "Ctrl", "1", "2")
    )),
    list("trailing_empty", rbind(
      c("ID", "Group", "A", "B", ""),
      c("", "", "Rep1", "Rep2", ""),
      c("1", "Ctrl", "1", "2", "")
    )),
    list("numeric_headers", rbind(
      c("ID", "Group", "1", "2", "3"),
      c("", "", "Rep1", "Rep2", "Rep3"),
      c("1", "Ctrl", "4", "5", "6")
    )),
    list("mixed_headers", rbind(
      c("ID", "Diet", "Day1", "Day2", "3"),
      c("", "", "R1", "R2", "R3"),
      c("1", "Low", "1.1", "1.2", "1.3")
    )),
    list("spaces_specialchars", rbind(
      c(" ID ", "Group ", "A (mg)", "B (mg)"),
      c("", "", "Rep 1", "Rep 2"),
      c("1", "Ctrl", "7.1", "7.2")
    )),
    list("empty_rows_na", rbind(
      c("ID", "Group", "A", "B"),
      c("", "", "Rep1", "Rep2"),
      c("1", "Ctrl", "10", NA),
      c("", "", "", "")
    )),
    list("merged_across_three", rbind(
      c("ID", "Treatment", "A", "", "", "B", "", ""),
      c("", "", "R1", "R2", "R3", "R1", "R2", "R3"),
      c("1", "Low", "1", "2", "3", "4", "5", "6")
    )),
    list("all_numeric_header", rbind(
      c("1", "2", "3", "4"),
      c("Rep1", "Rep2", "Rep3", "Rep4"),
      c("5", "6", "7", "8")
    )),
    list("underscores_mixed", rbind(
      c("ID", "Batch", "A_Rep1", "A.Rep2", "A-Rep3"),
      c("", "", "", "", ""),
      c("1", "B1", "1", "2", "3")
    )),
    list("text_numeric_combo", rbind(
      c("ID", "Stage", "Day 1", "Day 2", "Day 10"),
      c("", "", "RepA", "RepB", "RepC"),
      c("1", "Init", "0.1", "0.2", "0.9")
    )),
    list("multi_blank_zones", rbind(
      c("ID", "", "", "B", "", ""),
      c("", "", "", "R1", "R2", ""),
      c("1", "Ctrl", "5", "6", "7", "8")
    )),
    list("missing_measure_values", rbind(
      c("ID", "Group", "A", "B"),
      c("", "", "Rep1", "Rep2"),
      c("1", "Ctrl", NA, "5")
    )),
    list("special_symbols", rbind(
      c("Sample#", "Type$", "A%", "B%"),
      c("", "", "R1", "R2"),
      c("1", "Ctrl", "2.2", "2.5")
    )),
    list("wide_gap_headers", rbind(
      c("ID", "Group", rep("", 10)),
      c("", "", paste0("V", 1:10)),
      c("1", "Ctrl", as.character(1:10))
    )),
    list("duplicate_block_headers", rbind(
      c("ID", "Group", "A", "A", "B", "B"),
      c("", "", "R1", "R2", "R1", "R2"),
      c("1", "Ctrl", "1", "2", "3", "4")
    )),
    list("weird_chars_and_spaces", rbind(
      c(" ID ", " T reat ", "A! ", "B? "),
      c("", "", "R-1", "R-2"),
      c("1", "Low", "10", "12")
    ))
  )
  
  paths <- purrr::map2_chr(
    seq_along(tests),
    tests,
    function(i, x) write_test_excel(x[[2]], x[[1]], i)
  )
  
  safe_convert <- purrr::safely(convert_wide_to_long)
  
  expect_true(all(file.exists(paths)))
  
  results <- map(paths, safe_convert)
  
  out1 <- results[[1]]
  expect_null(out1$error)
  expect_s3_class(out1$result, "tbl_df")
  expect_equal(nrow(out1$result), 2)
  expect_true(all(c("ID", "Group") %in% names(out1$result)))
  expect_true(any(grepl("^A_", names(out1$result))))
  expect_true(any(grepl("^B_", names(out1$result))))
  
  
  out2 <- results[[2]]
  expect_null(out2$error)
  
  out3 <- results[[3]]
  expect_null(out3$error)
  
  out4 <- results[[4]]
  expect_null(out4$error)
  
  out5 <- results[[5]]
  expect_null(out5$error)
  expect_s3_class(out5$result, "tbl_df")
  
  out7 <- results[[7]]
  expect_null(out7$error)
  expect_true(ncol(out7$result) >= 3)
  
  out19 <- results[[19]]
  expect_null(out19$error)
  expect_s3_class(out19$result, "tbl_df")
})
