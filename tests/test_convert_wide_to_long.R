library(openxlsx)
library(dplyr)
library(readxl)
library(tidyr)
library(zoo)
library(purrr)

# ==========================================================
# Paste your convert_wide_to_long() and helpers here
# ==========================================================

convert_wide_to_long <- function(path, sheet = 1, replicate_col = "Replicate") {
  # ---- Read first two rows to capture merged header structure ----
  headers <- readxl::read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE)
  header1 <- as.character(unlist(headers[1, , drop = TRUE]))
  header2 <- as.character(unlist(headers[2, , drop = TRUE]))
  
  # ---- Fill blanks forward in first header ----
  header1[header1 == ""] <- NA
  header1 <- zoo::na.locf(header1, na.rm = FALSE)
  header2[is.na(header2) | header2 == ""] <- ""
  
  # ---- Combine headers safely ----
  clean_names <- ifelse(header2 == "", header1, paste0(header1, "_", header2))
  clean_names <- make.unique(clean_names, sep = "_")
  
  # ---- Detect number of fixed columns ----
  first_empty <- which(is.na(headers[1, ]) | headers[1, ] == "")[1]
  if (is.na(first_empty)) {
    n_fixed <- 0
  } else {
    n_fixed <- max(0, first_empty - 2)
  }
  fixed_cols <- clean_names[seq_len(n_fixed)]
  measure_cols <- setdiff(clean_names, fixed_cols)
  
  # ---- Read data with computed names ----
  data <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = clean_names)
  
  # ---- Reshape from wide to long then back to tidy ----
  data_long <- data |>
    pivot_longer(
      cols = tidyselect::all_of(measure_cols),
      names_to = c("Variable", replicate_col),
      names_pattern = "^(.*)_([^_]*)$",
      values_to = "Value"
    ) |>
    pivot_wider(names_from = "Variable", values_from = "Value")
  
  as_tibble(data_long)
}


# ==========================================================
# Helper functions
# ==========================================================

dir.create("test_excels", showWarnings = FALSE)

write_test_excel <- function(data, name, index) {
  filename <- sprintf("%02d_%s.xlsx", index, name)
  path <- file.path("test_excels", filename)
  openxlsx::write.xlsx(data, path, colNames = FALSE)
  path
}


# ==========================================================
# Generate 20 Excel scenarios
# ==========================================================

tests <- list(
  list("simple_clean", list(
    c("ID", "Group", "A", "B"),
    c("", "", "Rep1", "Rep2"),
    list(1, "Ctrl", 5, 6),
    list(2, "Treat", 7, 8)
  )),
  
  list("no_second_header", list(
    c("ID", "Group", "A_Rep1", "A_Rep2"),
    c("", "", "", ""),
    list(1, "Ctrl", 10, 12)
  )),
  
  list("irregular_merged", list(
    c("ID", "Group", "A", "", "B", ""),
    c("", "", "Rep1", "Rep2", "Rep1", "Rep2"),
    list(1, "Ctrl", 1.2, 1.3, 5.1, 5.2)
  )),
  
  list("blank_first_row", list(
    c("", "", "", ""),
    c("ID", "Group", "A_Rep1", "A_Rep2"),
    list(1, "Ctrl", 5, 7)
  )),
  
  list("duplicate_subheaders", list(
    c("ID", "Group", "A", "A"),
    c("", "", "Rep1", "Rep1"),
    list(1, "Ctrl", 1, 2)
  )),
  
  list("trailing_empty", list(
    c("ID", "Group", "A", "B", ""),
    c("", "", "Rep1", "Rep2", ""),
    list(1, "Ctrl", 1, 2, NA)
  )),
  
  list("numeric_headers", list(
    c("ID", "Group", "1", "2", "3"),
    c("", "", "Rep1", "Rep2", "Rep3"),
    list(1, "Ctrl", 4, 5, 6)
  )),
  
  list("mixed_headers", list(
    c("ID", "Diet", "Day1", "Day2", "3"),
    c("", "", "R1", "R2", "R3"),
    list(1, "Low", 1.1, 1.2, 1.3)
  )),
  
  list("spaces_specialchars", list(
    c(" ID ", "Group ", "A (mg)", "B (mg)"),
    c("", "", "Rep 1", "Rep 2"),
    list(1, "Ctrl", 7.1, 7.2)
  )),
  
  list("empty_rows_na", list(
    c("ID", "Group", "A", "B"),
    c("", "", "Rep1", "Rep2"),
    list(1, "Ctrl", 10, NA),
    c("", "", "", "")
  )),
  
  list("merged_across_three", list(
    c("ID", "Treatment", "A", "", "", "B", "", ""),
    c("", "", "R1", "R2", "R3", "R1", "R2", "R3"),
    list(1, "Low", 1, 2, 3, 4, 5, 6)
  )),
  
  list("all_numeric_header", list(
    c("1", "2", "3", "4"),
    c("Rep1", "Rep2", "Rep3", "Rep4"),
    list(5, 6, 7, 8)
  )),
  
  list("underscores_mixed", list(
    c("ID", "Batch", "A_Rep1", "A.Rep2", "A-Rep3"),
    c("", "", "", "", ""),
    list(1, "B1", 1, 2, 3)
  )),
  
  list("text_numeric_combo", list(
    c("ID", "Stage", "Day 1", "Day 2", "Day 10"),
    c("", "", "RepA", "RepB", "RepC"),
    list(1, "Init", 0.1, 0.2, 0.9)
  )),
  
  list("multi_blank_zones", list(
    c("ID", "", "", "B", "", ""),
    c("", "", "", "R1", "R2", ""),
    list(1, "Ctrl", 5, 6, 7, 8)
  )),
  
  list("missing_measure_values", list(
    c("ID", "Group", "A", "B"),
    c("", "", "Rep1", "Rep2"),
    list(1, "Ctrl", NA, 5)
  )),
  
  list("special_symbols", list(
    c("Sample#", "Type$", "A%", "B%"),
    c("", "", "R1", "R2"),
    list(1, "Ctrl", 2.2, 2.5)
  )),
  
  list("wide_gap_headers", list(
    c("ID", "Group", rep("", 10)),
    c("", "", paste0("V", 1:10)),
    c(1, "Ctrl", as.numeric(1:10))
  )),
  
  list("duplicate_block_headers", list(
    c("ID", "Group", "A", "A", "B", "B"),
    c("", "", "R1", "R2", "R1", "R2"),
    list(1, "Ctrl", 1, 2, 3, 4)
  )),
  
  list("weird_chars_and_spaces", list(
    c(" ID ", " T reat ", "A! ", "B? "),
    c("", "", "R-1", "R-2"),
    list(1, "Low", 10, 12)
  ))
)


paths <- purrr::map2(
  seq_along(tests),
  tests,
  function(i, x) write_test_excel(x[[2]], x[[1]], i)
)

safe_convert <- safely(function(path) {
  convert_wide_to_long(path)
})

# ==========================================================
# Run safely on all files
# ==========================================================

results <- map(paths, function(p) {
  message("Testing: ", basename(p))
  res <- safe_convert(p)
  tibble(
    file = basename(p),
    success = !is.null(res$result),
    error = if (!is.null(res$error)) conditionMessage(res$error) else NA_character_,
    rows = if (!is.null(res$result)) nrow(res$result) else NA_integer_,
    cols = if (!is.null(res$result)) ncol(res$result) else NA_integer_
  )
})

summary_df <- bind_rows(results)
print(summary_df)

# Save diagnostics
write.csv(summary_df, "conversion_summary.csv", row.names = FALSE)

cat("\nSummary written to conversion_summary.csv\n")
