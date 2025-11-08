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
    )

  # ---- Detect duplicate measurements before widening ----
  id_cols <- c(fixed_cols, replicate_col, "Variable")
  duplicates <- data_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.n > 1)

  if (nrow(duplicates) > 0) {
    example <- duplicates[1, , drop = FALSE]
    var_label <- example$Variable
    if (is.factor(var_label)) {
      var_label <- as.character(var_label)
    }
    if (length(var_label) == 0 || is.na(var_label) || identical(var_label, "")) {
      var_label <- "<unknown>"
    }

    replicate_label <- example[[replicate_col]]
    if (is.factor(replicate_label)) {
      replicate_label <- as.character(replicate_label)
    }
    if (length(replicate_label) == 0 || is.na(replicate_label) || identical(replicate_label, "")) {
      replicate_label <- "<blank>"
    }

    stop(
      sprintf(
        "Duplicate measurements detected for variable '%s' and replicate '%s'. Ensure header labels are unique before uploading.",
        var_label,
        replicate_label
      ),
      call. = FALSE
    )
  }

  data_long |>
    pivot_wider(names_from = "Variable", values_from = "Value") |>
    as_tibble()
}

safe_convert_wide_to_long <- purrr::safely(convert_wide_to_long)

safe_preprocess_uploaded_table <- purrr::safely(preprocess_uploaded_table)
