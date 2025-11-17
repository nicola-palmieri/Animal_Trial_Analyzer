library(testthat)
library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(withr)
library(purrr)
library(zoo)

source(test_path("..", "..", "R", "module_upload_helpers.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

write_excel_matrix <- function(mat, path, sheet = "Sheet1") {
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, mat, colNames = FALSE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  path
}

expect_cell <- function(data, replicate_value, variable, expected, id_col = "Sample") {
  row <- data |> filter(.data[[names(data)[1]]] == "S1", .data[[grep("Rep", names(data), value = TRUE)[1]]] == replicate_value)
  expect_equal(row[[variable]], expected)
}

test_that("convert_wide_to_long handles diverse wide layouts", {
  tmp_dir <- withr::local_tempdir()

  scenarios <- list(
    basic_layout = list(
      matrix = rbind(
        c("Sample", "Group", "Info", "", "A", "A", "B", "B"),
        c("", "", "", "", "R1", "R2", "R1", "R2"),
        c("S1", "Ctrl", "alpha", "", 1, 2, 3, 4),
        c("S2", "Trt", "beta", "", 5, 6, 7, 8)
      ),
      check = function(df) {
        expect_s3_class(df, "tbl_df")
        expect_equal(sort(names(df)), sort(c("Sample", "Group", "Replicate", "A", "B", "Info")))
        expect_true(all(c("R1", "R2") %in% unique(df$Replicate)))
        first_row <- df |>
          arrange(Sample, Replicate) |>
          filter(Sample == "S1")
        expect_equal(first_row$A, c(1, 2))
        expect_equal(first_row$B, c(3, 4))
      }
    ),
    no_second_header_row = list(
      matrix = rbind(
        c("Sample", "Group", "A_R1", "A_R2", "B_R1"),
        c("", "", "", "", ""),
        c("S1", "Ctrl", 10, 12, 13)
      ),
      check = function(df) {
        expect_named(df, c("Sample", "Group", "Replicate", "A", "B"))
        expect_equal(df$Replicate, c("R1", "R2", "R1"))
        expect_equal(df$A[df$Replicate == "R2"], 12)
        expect_equal(df$B[df$Replicate == "R1"], 13)
      }
    ),
    header_fill_propagation = list(
      matrix = rbind(
        c("ID", "", "", "Measure", "Measure"),
        c("", "", "", "R1", "R2"),
        c("S1", "", "", 1, 2)
      ),
      check = function(df) {
        expect_true("ID" %in% names(df))
        expect_equal(df$Replicate, c("R1", "R2"))
        expect_equal(df$Measure[df$Replicate == "R2"], 2)
      }
    ),
    metadata_boundary = list(
      matrix = rbind(
        c("ID", "Batch", "Note", "", "Mass", "Mass", "Temp", "Temp"),
        c("", "", "", "", "R1", "R2", "R1", "R2"),
        c("S1", "B1", "cold", "", 1.1, 1.2, 10, 11)
      ),
      check = function(df) {
        expect_true(all(c("ID", "Batch", "Note") %in% names(df)))
        expect_equal(df$Mass, c(1.1, 1.2))
        expect_equal(df$Temp[df$Replicate == "R1"], 10)
      }
    ),
    numeric_headers = list(
      matrix = rbind(
        c("Sample", "Group", "", "1", "1", "2", "2"),
        c("", "", "", "R1", "R2", "R1", "R2"),
        c("S1", "Ctrl", "", 4, 5, 6, 7)
      ),
      check = function(df) {
        expect_true(all(c("1", "2") %in% names(df)))
        expect_equal(df$`1`[df$Replicate == "R2"], 5)
      }
    ),
    spaces_and_symbols = list(
      matrix = rbind(
        c(" Sample ", " Group ", "", "A (mg)", "A (mg)", "B%", "B%"),
        c("", "", "", "R 1", "R 2", "R 1", "R 2"),
        c("S1", "Ctrl", "", 7.1, 7.2, 8.1, 8.2)
      ),
      check = function(df) {
        expect_true(all(c(" Sample ", " Group ") %in% names(df)))
        expect_equal(df$`A (mg)`[df$Replicate == "R 2"], 7.2)
        expect_equal(df$`B%`[df$Replicate == "R 1"], 8.1)
      }
    ),
    missing_values = list(
      matrix = rbind(
        c("Sample", "Group", "", "A", "A", "B", "B"),
        c("", "", "", "R1", "R2", "R1", "R2"),
        c("S1", "Ctrl", "", NA, 2, 3, NA)
      ),
      check = function(df) {
        expect_true(any(is.na(df$A)))
        expect_true(any(is.na(df$B)))
        expect_equal(df$B[df$Replicate == "R1"], 3)
      }
    ),
    multiple_samples = list(
      matrix = rbind(
        c("Sample", "Group", "", "A", "A"),
        c("", "", "", "R1", "R2"),
        c("S1", "Ctrl", "", 1, 2),
        c("S2", "Trt", "", 3, 4),
        c("S3", "Trt", "", 5, 6)
      ),
      check = function(df) {
        expect_equal(length(unique(df$Sample)), 3)
        expect_equal(df |> filter(Sample == "S3", Replicate == "R2") |> pull(A), 6)
      }
    ),
    long_replicate_labels = list(
      matrix = rbind(
        c("Sample", "", "A", "A"),
        c("", "", "Baseline", "FollowUp"),
        c("S1", "", 0.1, 0.2)
      ),
      check = function(df) {
        expect_true(all(c("Baseline", "FollowUp") %in% df$Replicate))
        expect_equal(df$A[df$Replicate == "FollowUp"], 0.2)
      }
    ),
    custom_replicate_column = list(
      matrix = rbind(
        c("Sample", "", "A", "A"),
        c("", "", "Day1", "Day2"),
        c("S1", "", 9, 10)
      ),
      replicate_col = "Visit",
      check = function(df) {
        expect_true("Visit" %in% names(df))
        expect_equal(df$A[df$Visit == "Day2"], 10)
      }
    ),
    duplicate_measure_detection = list(
      matrix = rbind(
        c("Sample", "", "A", "A"),
        c("", "", "R1", "R1"),
        c("S1", "", 1, 2)
      ),
      error = "Duplicate measurements detected"
    ),
    wide_gap_headers = list(
      matrix = rbind(
        c("Sample", rep("", 5)),
        c("", paste0("V", 1:5)),
        c("S1", as.character(1:5))
      ),
      check = function(df) {
        expect_equal(ncol(df), 7)
        expect_equal(df |> filter(Replicate == "V3") |> pull(V3), 3)
      }
    ),
    underscore_variable_names = list(
      matrix = rbind(
        c("Sample", "", "Heart_Rate", "Heart_Rate"),
        c("", "", "R1", "R2"),
        c("S1", "", 60, 62)
      ),
      check = function(df) {
        expect_true("Heart_Rate" %in% names(df))
        expect_equal(df$Heart_Rate[df$Replicate == "R2"], 62)
      }
    ),
    alternate_sheet = list(
      matrix = rbind(
        c("Sample", "", "A", "A"),
        c("", "", "R1", "R2"),
        c("S1", "", 1, 2)
      ),
      sheet = "Data",
      check = function(df) {
        expect_equal(unique(df$Replicate), c("R1", "R2"))
      }
    ),
    character_ids = list(
      matrix = rbind(
        c("ID", "", "A", "A"),
        c("", "", "R1", "R2"),
        c("Alpha", "", 5, 6)
      ),
      check = function(df) {
        expect_true(is.character(df$ID) || is.factor(df$ID))
        expect_equal(df$A[df$Replicate == "R1"], 5)
      }
    ),
    trailing_empty_column = list(
      matrix = rbind(
        c("Sample", "", "A", "A", ""),
        c("", "", "R1", "R2", ""),
        c("S1", "", 1, 2, "")
      ),
      check = function(df) {
        expect_true("A" %in% names(df))
        expect_equal(nrow(df), 2)
      }
    ),
    single_variable_many_reps = list(
      matrix = rbind(
        c("Sample", "", rep("Value", 4)),
        c("", "", paste0("R", 1:4)),
        c("S1", "", 1:4)
      ),
      check = function(df) {
        expect_equal(sort(df$Replicate), paste0("R", 1:4))
        expect_equal(df$Value[df$Replicate == "R3"], 3)
      }
    ),
    no_blank_boundary = list(
      matrix = rbind(
        c("Sample", "A", "A"),
        c("", "R1", "R2"),
        c("S1", 1, 2)
      ),
      check = function(df) {
        expect_equal(df$Replicate, c("R1", "R2"))
        expect_equal(df$A[df$Replicate == "R1"], 1)
      }
    ),
    make_unique_headers = list(
      matrix = rbind(
        c("Sample", "", "A", "A", "A"),
        c("", "", "R1", "R2", "R2"),
        c("S1", "", 1, 2, 3)
      ),
      check = function(df) {
        expect_equal(df$Replicate, c("R1", "R2", "R2_1"))
        expect_equal(df$A[df$Replicate == "R2_1"], 3)
      }
    ),
    numeric_replicate_names = list(
      matrix = rbind(
        c("Sample", "", "A", "A"),
        c("", "", "1", "2"),
        c("S1", "", 5, 6)
      ),
      check = function(df) {
        expect_true(all(c("1", "2") %in% df$Replicate))
        expect_equal(df$A[df$Replicate == "1"], 5)
      }
    )
  )

  walk2(scenarios, seq_along(scenarios), function(cfg, idx) {
    sheet <- cfg$sheet %||% "Sheet1"
    file_path <- file.path(tmp_dir, sprintf("%02d_%s.xlsx", idx, names(scenarios)[[idx]]))
    write_excel_matrix(cfg$matrix, file_path, sheet = sheet)

    if (!is.null(cfg$error)) {
      expect_error(
        convert_wide_to_long(file_path, sheet = sheet, replicate_col = cfg$replicate_col %||% "Replicate"),
        cfg$error
      )
    } else {
      result <- convert_wide_to_long(file_path, sheet = sheet, replicate_col = cfg$replicate_col %||% "Replicate")
      cfg$check(result)
    }
  })
})
