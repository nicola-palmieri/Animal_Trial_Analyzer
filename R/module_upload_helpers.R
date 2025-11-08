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
  if (nrow(headers) < 2) {
    stop("The uploaded sheet must contain at least two header rows for wide-to-long conversion.", call. = FALSE)
  }

  trim_to_chr <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- NA_character_
    out <- trimws(x)
    out[out == ""] <- NA_character_
    out
  }

  header1_raw <- trim_to_chr(unlist(headers[1, , drop = TRUE]))
  header2_raw <- trim_to_chr(unlist(headers[2, , drop = TRUE]))

  header1_filled <- header1_raw
  header1_filled <- zoo::na.locf(header1_filled, na.rm = FALSE)
  header2_clean <- header2_raw
  header2_clean[is.na(header2_clean)] <- ""

  col_ids <- paste0("col_", seq_along(header1_raw))
  header_info <- tibble::tibble(
    col_id = col_ids,
    header1_raw = header1_raw,
    header2_raw = header2_raw,
    header1 = header1_filled,
    header2 = header2_clean
  ) |>
    dplyr::mutate(
      is_blank = (is.na(header1_raw) | header1_raw == "") & header2 == ""
    )

  header_counts <- table(header_info$header1[!is.na(header_info$header1)])
  lookup_count <- function(x) {
    if (is.na(x)) return(0L)
    if (!x %in% names(header_counts)) return(0L)
    as.integer(header_counts[[x]])
  }

  header_info <- header_info |>
    dplyr::mutate(
      header1_count = vapply(header1, lookup_count, integer(1)),
      is_measure = dplyr::case_when(
        is_blank ~ FALSE,
        header2 != "" ~ TRUE,
        header1_count > 1 ~ TRUE,
        TRUE ~ FALSE
      )
    )

  non_blank_ids <- header_info$col_id[!header_info$is_blank]
  header_info <- header_info |>
    dplyr::filter(!is_blank)

  measure_info <- header_info |>
    dplyr::filter(is_measure)
  fixed_info <- header_info |>
    dplyr::filter(!is_measure)

  if (nrow(measure_info) == 0) {
    stop("No measurement columns detected. Please verify the workbook header structure.", call. = FALSE)
  }

  if (any(is.na(measure_info$header1) | measure_info$header1 == "")) {
    stop("Unable to determine variable names from the header. Ensure each measurement block has a label in the first header row.", call. = FALSE)
  }

  measure_info <- measure_info |>
    dplyr::mutate(
      Variable = header1,
      Replicate = header2
    ) |>
    dplyr::group_by(Variable) |>
    dplyr::mutate(
      Replicate = {
        reps <- Replicate
        reps[is.na(reps) | reps == ""] <- paste0("Rep", seq_along(reps))
        make.unique(reps, sep = "_")
      }
    ) |>
    dplyr::ungroup()

  if (nrow(fixed_info) > 0) {
    fixed_info <- fixed_info |>
      dplyr::mutate(
        fixed_name = header1_raw,
        fixed_name = ifelse(is.na(fixed_name) | fixed_name == "", header2, fixed_name),
        fixed_name = ifelse(is.na(fixed_name) | fixed_name == "", paste0("Column_", dplyr::row_number()), fixed_name)
      )
    fixed_info$fixed_name <- make.unique(fixed_info$fixed_name, sep = "_")
  }

  data <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = col_ids)
  if (length(non_blank_ids) > 0) {
    data <- dplyr::select(data, tidyselect::all_of(non_blank_ids))
  }

  if (nrow(fixed_info) > 0) {
    rename_map <- rlang::set_names(fixed_info$col_id, fixed_info$fixed_name)
    data <- dplyr::rename(data, !!!rename_map)
  }

  measure_lookup <- measure_info |>
    dplyr::select(col_id, Variable, Replicate)

  data_long <- data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(measure_lookup$col_id),
      names_to = "col_id",
      values_to = "Value"
    ) |>
    dplyr::left_join(measure_lookup, by = "col_id") |>
    dplyr::select(-col_id)

  variable_levels <- unique(measure_lookup$Variable)
  replicate_sym <- rlang::sym(replicate_col)

  data_long <- data_long |>
    dplyr::mutate(
      Variable = factor(Variable, levels = variable_levels)
    ) |>
    dplyr::rename(!!replicate_sym := Replicate)

  id_cols <- c(if (nrow(fixed_info) > 0) fixed_info$fixed_name else character(), replicate_col, "Variable")
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
    tidyr::pivot_wider(names_from = "Variable", values_from = "Value") |>
    tibble::as_tibble()
}

safe_convert_wide_to_long <- purrr::safely(convert_wide_to_long)

safe_preprocess_uploaded_table <- purrr::safely(preprocess_uploaded_table)
