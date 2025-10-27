# ===============================================================
# 🧱 Visualization Layout Management
# ===============================================================

initialize_layout_state <- function(input, session) {
  default_key <- "default"
  state_map <- reactiveVal(list())
  active_key <- reactiveVal(default_key)

  base_state <- function() {
    list(
      overrides = list(
        strata_rows = 0L,
        strata_cols = 0L,
        resp_rows = 0L,
        resp_cols = 0L
      ),
      manual = list(
        strata_rows = FALSE,
        strata_cols = FALSE,
        resp_rows = FALSE,
        resp_cols = FALSE
      ),
      suppress = list(
        strata_rows = TRUE,
        strata_cols = TRUE,
        resp_rows = TRUE,
        resp_cols = TRUE
      ),
      last_synced = list(
        strata_rows = NA_integer_,
        strata_cols = NA_integer_,
        resp_rows = NA_integer_,
        resp_cols = NA_integer_
      )
    )
  }

  ensure_state <- function(key) {
    if (is.null(key) || key == "") key <- default_key
    map <- state_map()
    if (is.null(map[[key]])) {
      map[[key]] <- base_state()
      state_map(map)
    }
    invisible(key)
  }

  get_state <- function(key = active_key()) {
    key <- ensure_state(key)
    state_map()[[key]]
  }

  update_state <- function(key, mutate_fn) {
    key <- ensure_state(key)
    map <- state_map()
    entry <- map[[key]]
    entry <- mutate_fn(entry)
    map[[key]] <- entry
    state_map(map)
    invisible(entry)
  }

  get_state_field <- function(key, field, name) {
    entry <- get_state(key)
    entry[[field]][[name]]
  }

  set_state_field <- function(key, field, name, value) {
    update_state(key, function(entry) {
      entry[[field]][[name]] <- value
      entry
    })
  }

  sanitize_positive_int <- function(value, default = 1L) {
    val_numeric <- suppressWarnings(as.numeric(value))
    if (length(val_numeric) != 1 || is.na(val_numeric) || val_numeric <= 0) {
      as.integer(default)
    } else {
      as.integer(round(val_numeric))
    }
  }

  observe_numeric_input <- function(name) {
    observeEvent(input[[name]], {
      key <- active_key()
      if (is.null(key) || key == "") key <- default_key

      if (isTRUE(get_state_field(key, "suppress", name))) {
        set_state_field(key, "suppress", name, FALSE)
        return()
      }

      val <- suppressWarnings(as.numeric(input[[name]]))
      if (is.na(val) || val <= 0) {
        set_state_field(key, "overrides", name, 0L)
        set_state_field(key, "manual", name, FALSE)
        set_state_field(key, "last_synced", name, NA_integer_)
      } else {
        sanitized <- as.integer(round(val))
        set_state_field(key, "overrides", name, sanitized)
        set_state_field(key, "manual", name, TRUE)
        set_state_field(key, "last_synced", name, NA_integer_)
      }
    })
  }

  lapply(c("strata_rows", "strata_cols", "resp_rows", "resp_cols"), observe_numeric_input)

  effective_input <- function(name, key = active_key()) {
    key <- ensure_state(key)
    if (isTRUE(get_state_field(key, "manual", name))) {
      get_state_field(key, "overrides", name)
    } else {
      0L
    }
  }

  ui_value <- function(name, key = active_key()) {
    key <- ensure_state(key)
    entry <- get_state(key)
    candidate <- if (isTRUE(entry$manual[[name]])) {
      entry$overrides[[name]]
    } else {
      entry$last_synced[[name]]
    }
    if (is.null(candidate) || is.na(candidate) || candidate <= 0) 1L else as.integer(candidate)
  }

  is_manual <- function(name, key = active_key()) {
    key <- ensure_state(key)
    isTRUE(get_state_field(key, "manual", name))
  }

  get_last_synced <- function(name, key = active_key()) {
    key <- ensure_state(key)
    get_state_field(key, "last_synced", name)
  }

  set_last_synced <- function(name, value, key = active_key()) {
    key <- ensure_state(key)
    set_state_field(key, "last_synced", name, as.integer(value))
  }

  get_suppress <- function(name, key = active_key()) {
    key <- ensure_state(key)
    isTRUE(get_state_field(key, "suppress", name))
  }

  set_suppress <- function(name, value, key = active_key()) {
    key <- ensure_state(key)
    set_state_field(key, "suppress", name, isTRUE(value))
  }

  sync_ui_inputs <- function(key = active_key()) {
    key <- ensure_state(key)
    entry <- get_state(key)
    for (name in names(entry$overrides)) {
      target <- ui_value(name, key)
      current <- suppressWarnings(as.numeric(input[[name]]))
      current_int <- if (length(current) == 0 || is.na(current)) NA_integer_ else as.integer(round(current))
      if (!identical(current_int, target)) {
        set_suppress(name, TRUE, key)
        updateNumericInput(session, name, value = target)
      }
    }
  }

  set_active_key <- function(key, sync_ui = FALSE) {
    key <- ensure_state(key)
    active_key(key)
    if (isTRUE(sync_ui)) {
      sync_ui_inputs(key)
    }
    invisible(key)
  }

  list(
    ensure_key = ensure_state,
    set_active_key = set_active_key,
    active_key = function() active_key(),
    effective_input = effective_input,
    ui_value = ui_value,
    is_manual = is_manual,
    get_last_synced = get_last_synced,
    set_last_synced = set_last_synced,
    get_suppress = get_suppress,
    set_suppress = set_suppress,
    sync_ui_inputs = sync_ui_inputs,
    sanitize_value = sanitize_positive_int
  )
}

observe_layout_synchronization <- function(input, plot_info_reactive, layout_state, session) {
  observeEvent(plot_info_reactive(), {
    info <- plot_info_reactive()
    if (is.null(info)) return()

    target_key <- info$key
    if (is.null(target_key) || target_key == "") {
      target_key <- layout_state$active_key()
    }
    layout_state$ensure_key(target_key)

    sync_input <- function(id, value, manual_key) {
      if (layout_state$is_manual(manual_key, target_key)) {
        return()
      }

      val <- layout_state$sanitize_value(value)

      pending_val <- layout_state$get_last_synced(id, target_key)
      if (layout_state$get_suppress(id, target_key) && !is.na(pending_val) && identical(pending_val, val)) {
        return()
      }

      current <- isolate({
        cur <- suppressWarnings(as.numeric(input[[id]]))
        if (length(cur) == 0) NA_real_ else cur
      })

      if (!is.na(current) && identical(as.integer(round(current)), val)) {
        layout_state$set_last_synced(id, val, target_key)
        return()
      }

      layout_state$set_suppress(id, TRUE, target_key)
      layout_state$set_last_synced(id, val, target_key)
      updateNumericInput(session, id, value = val)
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

    if (identical(layout_state$active_key(), target_key)) {
      layout_state$sync_ui_inputs(target_key)
    }
  })
}
