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

  paired_name <- function(name) {
    switch(
      name,
      strata_rows = "strata_cols",
      strata_cols = "strata_rows",
      resp_rows   = "resp_cols",
      resp_cols   = "resp_rows",
      NULL
    )
  }

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
    if (isTRUE(layout_manual[[name]])) {
      layout_overrides[[name]]
    } else {
      counterpart <- paired_name(name)
      if (!is.null(counterpart) && isTRUE(layout_manual[[counterpart]])) {
        val <- default_ui_value(input[[name]])
        as.integer(val)
      } else {
        0L
      }
    }
  }

  default_ui_value <- function(cur_val) {
    val <- if (is.null(cur_val)) 1 else cur_val
    ifelse(is.na(val) || val <= 0, 1, min(10, val))
  }

  list(
    overrides = layout_overrides,
    manual = layout_manual,
    suppress = suppress_updates,
    paired_name = paired_name,
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

  paired_name <- layout_state$paired_name

  update_control <- function(name, target) {
    if (is.null(name) || is.null(target) || !is.finite(target)) return()

    counterpart <- paired_name(name)
    if (!is.null(counterpart) && isTRUE(shiny::isolate(layout_state$manual[[counterpart]]))) {
      return()
    }

    if (isTRUE(shiny::isolate(layout_state$manual[[name]]))) {
      return()
    }

    target <- as.integer(max(1L, min(max_value, round(target))))

    current <- shiny::isolate(input[[name]])
    if (!identical(as.integer(current), target)) {
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
  
  # --- Compute sensible defaults when missing ---
  if (is.na(rows) && is.na(cols)) {
    rows <- ceiling(sqrt(n_items))
    cols <- ceiling(n_items / rows)
  } else if (is.na(rows)) {
    if (is.na(cols) || cols <= 0) {
      rows <- ceiling(sqrt(n_items)); cols <- ceiling(n_items / rows)
    } else {
      rows <- ceiling(n_items / cols)
    }
  } else if (is.na(cols)) {
    if (is.na(rows) || rows <= 0) {
      rows <- ceiling(sqrt(n_items)); cols <- ceiling(n_items / rows)
    } else {
      cols <- ceiling(n_items / rows)
    }
  }
  
  # --- Clamp minimum values ---
  rows <- max(1L, rows)
  cols <- max(1L, cols)
  
  # --- Evaluate grid validity ---
  capacity  <- rows * cols
  too_small <- capacity < n_items
  # "Too large" means at least one entire row OR one entire column is empty
  empty_row <- n_items <= (rows - 1L) * cols
  empty_col <- n_items <= rows * (cols - 1L)
  too_large <- (!too_small) && (empty_row || empty_col)
  
  if (too_small) {
    return(list(
      nrow = rows,
      ncol = cols,
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)
    ))
  }
  
  if (too_large) {
    return(list(
      nrow = rows,
      ncol = cols,
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)
    ))
  }
  
  # --- Valid grid ---
  list(
    nrow = rows,
    ncol = cols,
    valid = TRUE,
    message = NULL
  )
}


resolve_grid_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_integer_)
  val <- suppressWarnings(as.integer(value[1]))
  if (is.na(val) || val < 1) return(NA_integer_)
  val
}


