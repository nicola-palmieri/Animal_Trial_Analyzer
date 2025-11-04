# ===============================================================
# 🧱 Basic grid layout helpers
# ===============================================================

compute_default_grid <- function(n) {
  n <- max(1L, as.integer(n))
  rows <- ceiling(sqrt(n))
  cols <- ceiling(n / rows)
  list(rows = rows, cols = cols)
}

validate_grid <- function(n_items, rows, cols) {
  n_items <- max(1L, as.integer(n_items))
  rows <- max(1L, as.integer(rows))
  cols <- max(1L, as.integer(cols))

  too_small <- rows * cols < n_items
  empty_row <- n_items <= (rows - 1L) * cols
  empty_col <- n_items <= rows * (cols - 1L)
  too_large <- (!too_small) && (empty_row || empty_col)

  if (too_small) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)
    ))
  }

  if (too_large) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)
    ))
  }

  list(valid = TRUE, message = NULL)
}

basic_grid_value <- function(value,
                             default = 1L,
                             min_value = 1L,
                             max_value = 10L) {
  if (is.null(value) || length(value) == 0) {
    return(as.integer(default))
  }

  raw <- suppressWarnings(as.integer(value[1]))
  if (is.na(raw)) {
    return(as.integer(default))
  }

  adjusted <- max(as.integer(min_value), raw)
  if (!is.null(max_value)) {
    adjusted <- min(as.integer(max_value), adjusted)
  }

  as.integer(adjusted)
}

basic_grid_layout <- function(rows = NULL,
                              cols = NULL,
                              default_rows = 1L,
                              default_cols = 1L,
                              min_rows = 1L,
                              min_cols = 1L,
                              max_rows = 10L,
                              max_cols = 10L) {
  list(
    nrow = basic_grid_value(
      value = rows,
      default = default_rows,
      min_value = min_rows,
      max_value = max_rows
    ),
    ncol = basic_grid_value(
      value = cols,
      default = default_cols,
      min_value = min_cols,
      max_value = max_cols
    )
  )
}

needs_numeric_update <- function(current_value, target_value) {
  if (is.null(target_value) || length(target_value) == 0) {
    return(FALSE)
  }

  target_int <- suppressWarnings(as.integer(target_value[1]))
  if (is.na(target_int)) {
    return(FALSE)
  }

  if (is.null(current_value) || length(current_value) == 0) {
    return(TRUE)
  }

  current_int <- suppressWarnings(as.integer(current_value[1]))
  if (is.na(current_int)) {
    return(TRUE)
  }

  !identical(current_int, target_int)
}

sync_numeric_input <- function(session, input_id, current_value, target_value) {
  if (!needs_numeric_update(current_value, target_value)) {
    return(invisible(FALSE))
  }

  target_int <- as.integer(target_value[1])

  session$onFlushed(function() {
    updateNumericInput(session, input_id, value = target_int)
  }, once = TRUE)

  invisible(TRUE)
}
