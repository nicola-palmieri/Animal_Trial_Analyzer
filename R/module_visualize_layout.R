# ===============================================================
# 🧱 Basic grid layout helpers
# ===============================================================

compute_default_grid <- function(n) {
  n <- coerce_grid_value(n, default = 1L)
  rows <- ceiling(sqrt(n))
  list(rows = rows, cols = ceiling(n / rows))
}

coerce_grid_value <- function(value, default = 1L, min_value = 1L, max_value = NULL) {
  raw <- suppressWarnings(as.integer(value[1]))
  if (length(value) == 0 || is.na(raw)) raw <- default
  raw <- max(as.integer(min_value), raw)
  if (!is.null(max_value)) raw <- min(as.integer(max_value), raw)
  as.integer(raw)
}

validate_grid <- function(n_items, rows, cols) {
  n_items <- coerce_grid_value(n_items, default = 1L)
  rows <- coerce_grid_value(rows, default = 1L)
  cols <- coerce_grid_value(cols, default = 1L)

  too_small <- rows * cols < n_items
  empty_row <- n_items <= (rows - 1L) * cols
  empty_col <- n_items <= rows * (cols - 1L)

  if (too_small) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)
    ))
  }

  if (!too_small && (empty_row || empty_col)) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)
    ))
  }

  list(valid = TRUE, message = NULL)
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
    nrow = coerce_grid_value(rows, default_rows, min_rows, max_rows),
    ncol = coerce_grid_value(cols, default_cols, min_cols, max_cols)
  )
}

adjust_grid_layout <- function(n_items, layout) {
  if (is.null(layout) || length(layout) == 0) {
    return(list(nrow = 1L, ncol = 1L))
  }

  n_items <- coerce_grid_value(n_items, default = 1L)
  rows <- coerce_grid_value(layout$nrow, default = 1L)
  cols <- coerce_grid_value(layout$ncol, default = 1L)

  if (rows * cols <= n_items) {
    return(list(nrow = rows, ncol = cols))
  }

  repeat {
    adjusted <- FALSE
    if (rows > 1L && (rows - 1L) * cols >= n_items) {
      rows <- rows - 1L
      adjusted <- TRUE
    }
    if (cols > 1L && rows * (cols - 1L) >= n_items) {
      cols <- cols - 1L
      adjusted <- TRUE
    }
    if (!adjusted) break
  }

  list(nrow = rows, ncol = cols)
}

numeric_sync_state <- function(session) {
  state <- session$userData$ta_numeric_sync_state
  if (!is.environment(state)) {
    state <- new.env(parent = emptyenv())
    session$userData$ta_numeric_sync_state <- state
  }
  state
}

pending_key <- function(input_id) paste0("pending_", input_id)
input_key <- function(input_id) paste0("input_", input_id)

mark_pending_numeric_update <- function(session, input_id) {
  assign(pending_key(input_id), TRUE, envir = numeric_sync_state(session))
}

consume_pending_numeric_update <- function(session, input_id) {
  state <- numeric_sync_state(session)
  key <- pending_key(input_id)
  pending <- get0(key, envir = state, inherits = FALSE, ifnotfound = FALSE)
  if (isTRUE(pending)) {
    assign(key, FALSE, envir = state)
    return(TRUE)
  }
  FALSE
}

schedule_numeric_update <- function(session, input_id, value) {
  mark_pending_numeric_update(session, input_id)
  session$onFlushed(function() {
    updateNumericInput(session, input_id, value = value)
  }, once = TRUE)
}

sync_numeric_input <- function(session, input_id, current_value, target_value) {
  target_int <- suppressWarnings(as.integer(target_value[1]))
  if (length(target_value) == 0 || is.na(target_int)) {
    return(invisible(FALSE))
  }

  state <- numeric_sync_state(session)
  key <- input_key(input_id)
  entry <- get0(key, envir = state, inherits = FALSE, ifnotfound = list(value = NULL, auto = TRUE))

  current_int <- suppressWarnings(as.integer(current_value[1]))
  if (length(current_int) == 0) current_int <- NA_integer_
  missing_current <- is.na(current_int)

  if (!missing_current && !identical(current_int, entry$value)) {
    entry <- list(value = current_int, auto = FALSE)
    assign(key, entry, envir = state)
  }

  needs_update <- missing_current || (isTRUE(entry$auto) && !identical(current_int, target_int))
  if (!needs_update) {
    return(invisible(FALSE))
  }

  assign(key, list(value = target_int, auto = TRUE), envir = state)
  schedule_numeric_update(session, input_id, target_int)
  invisible(TRUE)
}
