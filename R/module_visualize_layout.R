# ===============================================================
# 🧱 Visualization Layout Management
# ===============================================================

initialize_layout_state <- function(input, session) {
  layout_overrides <- reactiveValues(
    strata_rows = 0,
    strata_cols = 0,
    resp_rows = 0,
    resp_cols = 0
  )

  layout_manual <- reactiveValues(
    strata_rows = FALSE,
    strata_cols = FALSE,
    resp_rows = FALSE,
    resp_cols = FALSE
  )

  suppress_updates <- reactiveValues(
    strata_rows = TRUE,
    strata_cols = TRUE,
    resp_rows = TRUE,
    resp_cols = TRUE
  )

  observe_numeric_input <- function(name) {
    observeEvent(input[[name]], {
      if (isTRUE(suppress_updates[[name]])) {
        suppress_updates[[name]] <- FALSE
        return()
      }

      val <- suppressWarnings(as.numeric(input[[name]]))
      if (is.na(val) || val < 1) {
        layout_overrides[[name]] <- 0L
        layout_manual[[name]] <- FALSE
      } else {
        clamped <- as.integer(max(1, min(10, val)))
        layout_overrides[[name]] <- clamped
        layout_manual[[name]] <- TRUE
      }
    })
  }
  lapply(c("strata_rows", "strata_cols", "resp_rows", "resp_cols"), observe_numeric_input)

  effective_input <- function(name) {
    if (isTRUE(layout_manual[[name]])) layout_overrides[[name]] else 0
  }

  default_ui_value <- function(cur_val) {
    val <- if (is.null(cur_val)) 1 else cur_val
    ifelse(is.na(val) || val <= 0, 1, min(10, val))
  }

  list(
    overrides = layout_overrides,
    manual = layout_manual,
    suppress = suppress_updates,
    effective_input = effective_input,
    default_ui_value = default_ui_value
  )
}

observe_layout_synchronization <- function(plot_info_reactive, layout_state, session) {
  observeEvent(plot_info_reactive(), {
    plot_info_reactive()
    layout_state
    session
    invisible(NULL)
  })
  invisible(NULL)
}

resolve_grid_layout <- function(n_items, rows_input = NULL, cols_input = NULL) {
  n_items <- suppressWarnings(as.integer(n_items[1]))
  if (is.na(n_items) || n_items <= 0) {
    n_items <- 1L
  }
  
  rows_raw <- resolve_grid_value(rows_input)
  cols_raw <- resolve_grid_value(cols_input)
  
  rows <- rows_raw
  cols <- cols_raw
  
  if (is.na(rows) && is.na(cols)) {
    rows <- ceiling(sqrt(n_items))
    cols <- ceiling(n_items / rows)
  } else if (is.na(rows)) {
    cols <- cols_raw
    if (is.na(cols) || cols <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      rows <- ceiling(n_items / cols)
    }
  } else if (is.na(cols)) {
    rows <- rows_raw
    if (is.na(rows) || rows <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      cols <- ceiling(n_items / rows)
    }
  }
  
  if ((is.na(rows_raw) || is.na(cols_raw)) && !is.na(rows) && !is.na(cols)) {
    while (rows * cols < n_items) {
      if (cols <= rows) {
        cols <- cols + 1L
      } else {
        rows <- rows + 1L
      }
    }
  }
  
  list(nrow = rows, ncol = cols)
}

resolve_grid_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_integer_)
  val <- suppressWarnings(as.integer(value[1]))
  if (is.na(val) || val < 1) return(NA_integer_)
  val
}


