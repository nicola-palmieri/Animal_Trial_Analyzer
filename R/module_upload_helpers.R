# Clean names + convert characters to ordered factors
preprocess_uploaded_table <- function(df) {
  df <- janitor::clean_names(df)
  df <- df |> mutate(across(where(is.character), auto_factor_order))
  df
}

# Convert character to factor with numeric-aware order
auto_factor_order <- function(x) {
  if (!is.character(x)) return(x)
  nums <- suppressWarnings(as.numeric(gsub("\\D", "", x)))
  if (all(is.na(nums))) {
    factor(x, levels = sort(unique(x)))
  } else {
    x <- factor(x, levels = unique(x[order(nums, na.last = TRUE)]))
    x
  }
}


convert_wide_to_long <- function(path, sheet = 1, replicate_col = "Replicate") {
  
  # ---- Read first two rows together to preserve blanks ----
  headers <- read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE)
  
  header1 <- as.character(unlist(headers[1, , drop = TRUE]))
  header2 <- as.character(unlist(headers[2, , drop = TRUE]))
  
  # ---- Fill blanks forward in header1 ----
  header1[header1 == ""] <- NA
  header1 <- zoo::na.locf(header1, na.rm = FALSE)
  header2[is.na(header2) | header2 == ""] <- ""
  
  # ---- Combine safely ----
  clean_names <- ifelse(header2 == "", header1, paste0(header1, "_", header2))
  clean_names <- make.unique(clean_names, sep = "_")
  
  # ---- Read data using combined names ----
  data <- read_excel(path, sheet = sheet, skip = 2, col_names = clean_names)
  
  # ---- Identify ID vs measurement columns ----
  fixed_cols <- clean_names[1:3]
  measure_cols <- setdiff(clean_names, fixed_cols)
  
  # ---- Reshape ----
  data_long <- data |>
    pivot_longer(
      cols = all_of(measure_cols),
      names_to = c("Variable", replicate_col),
      names_pattern = "^(.*)_([^_]*)$",
      values_to = "Value"
    ) |>
    pivot_wider(names_from = "Variable", values_from = "Value")
  
  as_tibble(data_long)
}
