convert_wide_to_long <- function(path, sheet = 1, replicate_col = "Replicate") {
  header_rows <- 2
  
  headers <- readxl::read_excel(path, sheet = sheet, n_max = header_rows, col_names = FALSE)
  data <- readxl::read_excel(path, sheet = sheet, skip = header_rows, col_names = FALSE)
  
  first_row <- as.character(unlist(headers[1, ]))
  first_row[first_row == ""] <- NA
  
  # find first repeated header (start of measured block)
  duplicated_start <- which(duplicated(first_row) & !is.na(first_row))
  if (length(duplicated_start) > 0) {
    n_fixed <- duplicated_start[1] - 1
  } else {
    n_fixed <- 3
  }
  
  fixed_names <- as.character(unlist(headers[1, 1:n_fixed]))
  fixed_names[is.na(fixed_names) | fixed_names == ""] <- paste0("V", seq_len(sum(is.na(fixed_names) | fixed_names == "")))
  colnames(data)[1:n_fixed] <- fixed_names
  
  measure_names <- as.character(unlist(headers[1, (n_fixed + 1):ncol(headers)]))
  measure_names <- zoo::na.locf(measure_names)
  
  replicate_ids <- as.character(unlist(headers[2, (n_fixed + 1):ncol(headers)]))
  replicate_ids[is.na(replicate_ids) | replicate_ids == ""] <- "1"
  
  colnames(data)[(n_fixed + 1):ncol(data)] <- paste0(measure_names, "_", replicate_ids)
  
  measure_cols <- colnames(data)[(n_fixed + 1):ncol(data)]
  
  data_fixed <- data[, 1:n_fixed]
  data_long <- data |>
    select(all_of(measure_cols)) |>
    mutate(row_id = seq_len(n())) |>
    pivot_longer(
      cols = all_of(measure_cols),
      names_to = c("Variable", replicate_col),
      names_sep = "_",
      values_to = "Value"
    ) |>
    pivot_wider(names_from = "Variable", values_from = "Value")
  
  long_df <- bind_cols(data_fixed[rep(seq_len(nrow(data_fixed)), each = length(unique(data_long[[replicate_col]]))), ],
                       data_long |> select(-row_id))
  
  long_df
}
