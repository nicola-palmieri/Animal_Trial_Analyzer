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
      if (is.na(val) || val <= 0) {
        layout_overrides[[name]] <- 0L
        layout_manual[[name]] <- FALSE
      } else {
        layout_overrides[[name]] <- as.integer(val)
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
    ifelse(is.na(val) || val <= 0, 1, val)
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
    info <- plot_info_reactive()
    if (is.null(info)) return()

    sync_input <- function(id, value, manual_key) {
      val <- ifelse(is.null(value) || value <= 0, 1, value)
      if (!isTRUE(layout_state$manual[[manual_key]])) {
        layout_state$suppress[[id]] <- TRUE
        updateNumericInput(session, id, value = val)
      }
    }

    if (isTRUE(info$has_strata)) {
      sync_input("strata_rows", info$layout$strata$rows, "strata_rows")
      sync_input("strata_cols", info$layout$strata$cols, "strata_cols")
    } else {
      sync_input("strata_rows", 1, "strata_rows")
      sync_input("strata_cols", 1, "strata_cols")
    }

    resp_rows_val <- if (info$n_responses <= 1) 1 else info$layout$responses$nrow
    resp_cols_val <- if (info$n_responses <= 1) 1 else info$layout$responses$ncol

    sync_input("resp_rows", resp_rows_val, "resp_rows")
    sync_input("resp_cols", resp_cols_val, "resp_cols")
  })
}
