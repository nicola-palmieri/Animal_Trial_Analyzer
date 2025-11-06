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

numeric_sync_state <- function(session) {
  state <- session$userData$ta_numeric_sync_state
  if (is.null(state) || !is.environment(state)) {
    state <- new.env(parent = emptyenv())
    session$userData$ta_numeric_sync_state <- state
  }
  state
}

sync_numeric_input <- function(session, input_id, current_value, target_value) {
  if (is.null(target_value) || length(target_value) == 0) {
    return(invisible(FALSE))
  }

  target_int <- suppressWarnings(as.integer(target_value[1]))
  if (is.na(target_int)) {
    return(invisible(FALSE))
  }

  state <- numeric_sync_state(session)
  key <- paste0("input_", input_id)

  current_int <- suppressWarnings(as.integer(current_value[1]))
  missing_current <- length(current_value) == 0 || is.na(current_int)

  if (!exists(key, envir = state, inherits = FALSE)) {
    assign(key, list(value = NULL, auto = TRUE), envir = state)
  }

  entry <- get(key, envir = state, inherits = FALSE)

  if (missing_current) {
    assign(key, list(value = target_int, auto = TRUE), envir = state)
    session$onFlushed(function() {
      updateNumericInput(session, input_id, value = target_int)
    }, once = TRUE)
    return(invisible(TRUE))
  }

  if (!identical(current_int, entry$value)) {
    entry <- list(value = current_int, auto = FALSE)
    assign(key, entry, envir = state)
  }

  if (isTRUE(entry$auto) && !identical(current_int, target_int)) {
    assign(key, list(value = target_int, auto = TRUE), envir = state)
    session$onFlushed(function() {
      updateNumericInput(session, input_id, value = target_int)
    }, once = TRUE)
    return(invisible(TRUE))
  }

  invisible(FALSE)
}
