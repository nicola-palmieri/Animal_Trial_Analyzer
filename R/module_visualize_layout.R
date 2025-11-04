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

sync_grid_controls <- function(layout_state,
                               input,
                               session,
                               rows_name,
                               cols_name,
                               layout,
                               max_value = 10L) {
  if (is.null(layout)) {
    return(invisible(NULL))
  }

  update_control <- function(name, target) {
    if (is.null(name) || is.null(target) || !is.finite(target)) return()

    target <- as.integer(max(1L, min(max_value, round(target))))

    if (isTRUE(shiny::isolate(layout_state$manual[[name]]))) {
      return()
    }

    current <- shiny::isolate(input[[name]])
    if (isTRUE(!identical(as.integer(current), target))) {
      layout_state$suppress[[name]] <- TRUE
      updateNumericInput(session, name, value = target, min = 1, max = max_value)
    }
  }

  update_control(rows_name, layout$nrow)
  update_control(cols_name, layout$ncol)

  invisible(NULL)
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
  # --- Validate number of items ---
  n_items <- suppressWarnings(as.integer(n_items[1]))
  if (is.na(n_items) || n_items <= 0) n_items <- 1L

  # --- Extract numeric inputs ---
  rows_raw <- resolve_grid_value(rows_input)
  cols_raw <- resolve_grid_value(cols_input)

  rows <- rows_raw
  cols <- cols_raw

  # --- Compute sensible defaults ---
  if (is.na(rows) && is.na(cols)) {
    # automatic roughly-square layout
    rows <- ceiling(sqrt(n_items))
    cols <- ceiling(n_items / rows)
  } else if (is.na(rows)) {
    # rows missing, infer from columns
    if (is.na(cols) || cols <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      rows <- ceiling(n_items / cols)
    }
  } else if (is.na(cols)) {
    # cols missing, infer from rows
    if (is.na(rows) || rows <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      cols <- ceiling(n_items / rows)
    }
  }

  # --- Clamp minimum values ---
  rows <- max(1L, rows)
  cols <- max(1L, cols)

  capacity <- rows * cols

  if (capacity < n_items) {
    return(list(
      nrow = rows,
      ncol = cols,
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)
    ))
  }

  if (capacity > n_items) {
    return(list(
      nrow = rows,
      ncol = cols,
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)
    ))
  }

  list(nrow = rows, ncol = cols, valid = TRUE, message = NULL)
}


resolve_grid_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_integer_)
  val <- suppressWarnings(as.integer(value[1]))
  if (is.na(val) || val < 1) return(NA_integer_)
  val
}


