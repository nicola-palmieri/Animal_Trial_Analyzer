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
  
  # ---- Load first two rows ----
  headers <- readxl::read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE)
  header1 <- as.character(unlist(headers[1, , drop = TRUE]))
  header2 <- as.character(unlist(headers[2, , drop = TRUE]))
  
  # ---- Handle blank first header row ----
  if (all(header1 == "" | is.na(header1))) {
    header1 <- header2
    header2 <- rep("", length(header1))
  }
  
  # ---- Fill blanks in upper header ----
  header1[header1 == ""] <- NA
  header1 <- zoo::na.locf(header1, na.rm = FALSE)
  
  header2[is.na(header2) | header2 == ""] <- ""
  
  # ---- If no second header exists, extract replicate label from name ----
  if (all(header2 == "")) {
    header2 <- ifelse(grepl("_", header1), sub(".*_", "", header1), "")
  }
  
  # ---- Build clean names ----
  clean_names <- ifelse(header2 == "", header1, paste0(header1, "_", header2))
  clean_names <- make.unique(clean_names, sep = "_")
  
  fixed_cols <- clean_names[header2 == ""]
  measure_cols <- clean_names[header2 != ""]
  
  # ---- Read full data ----
  data <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = clean_names)
  
  # ---- Long form ----
  data_long <- data |>
    pivot_longer(
      cols = tidyselect::all_of(measure_cols),
      names_to = c("Variable", replicate_col),
      names_pattern = "^(.*)_([^_]*)$",
      values_to = "Value"
    )
  
  # ---- Duplicate detection ----
  id_cols <- c(fixed_cols, replicate_col, "Variable")
  
  duplicates <- data_long |>
    group_by(across(all_of(id_cols))) |>
    summarise(n = n(), .groups = "drop") |>
    filter(n > 1)
  
  if (nrow(duplicates) > 0) {
    ex <- duplicates[1, ]
    
    stop(sprintf(
      "Duplicate measurements detected for variable '%s' and replicate '%s'. Ensure header labels are unique before uploading.",
      as.character(ex$Variable %||% "<unknown>"),
      as.character(ex[[replicate_col]] %||% "<blank>")
    ), call. = FALSE)
  }
  
  # ---- Build final column name manually ----
  data_long <- data_long |>
    mutate(
      .final_name = paste0(Variable, "_", .data[[replicate_col]])
    )
  
  # ---- Pivot wider safely ----
  data_wide <- data_long |>
    select(all_of(fixed_cols), .final_name, Value) |>
    pivot_wider(
      names_from = .final_name,
      values_from = Value
    ) |>
    as_tibble()
  
  data_wide
}

safe_convert_wide_to_long <- purrr::safely(convert_wide_to_long)

safe_preprocess_uploaded_table <- purrr::safely(preprocess_uploaded_table)
