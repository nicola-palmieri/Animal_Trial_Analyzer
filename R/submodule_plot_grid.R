# ===============================================================
# 🧩 Plot grid module
# ===============================================================

compute_default_grid <- function(n) {
  n <- coerce_grid_value(n, default = 1L)
  rows <- ceiling(sqrt(n))
  list(rows = rows, cols = ceiling(n / rows))
}

coerce_grid_value <- function(value,
                              default = 1L,
                              min_value = 1L,
                              max_value = NULL) {
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

plot_grid_ui <- function(id,
                         rows_label = "Grid rows",
                         cols_label = "Grid columns",
                         rows_help = NULL,
                         cols_help = NULL,
                         rows_min = 1L,
                         rows_max = 10L,
                         cols_min = 1L,
                         cols_max = 10L,
                         rows_value = NA,
                         cols_value = NA) {
  ns <- NS(id)

  rows_input <- numericInput(
    ns("rows"),
    rows_label,
    value = rows_value,
    min = rows_min,
    max = rows_max,
    step = 1
  )

  cols_input <- numericInput(
    ns("cols"),
    cols_label,
    value = cols_value,
    min = cols_min,
    max = cols_max,
    step = 1
  )

  if (!is.null(rows_help)) {
    rows_input <- with_help_tooltip(rows_input, rows_help)
  }

  if (!is.null(cols_help)) {
    cols_input <- with_help_tooltip(cols_input, cols_help)
  }

  tagList(
    fluidRow(
      column(6, rows_input),
      column(6, cols_input)
    )
  )
}

plot_grid_server <- function(id,
                             rows_min = 1L,
                             rows_max = 10L,
                             cols_min = 1L,
                             cols_max = 10L) {
  moduleServer(id, function(input, output, session) {
    sanitize_value <- function(value, min_value, max_value) {
      if (length(value) == 0) {
        return(NA_integer_)
      }
      numeric_value <- suppressWarnings(as.integer(value[1]))
      if (is.na(numeric_value)) {
        return(NA_integer_)
      }
      numeric_value <- max(as.integer(min_value), numeric_value)
      if (!is.null(max_value)) {
        numeric_value <- min(as.integer(max_value), numeric_value)
      }
      as.integer(numeric_value)
    }

    rows <- reactive(sanitize_value(input$rows, rows_min, rows_max))
    cols <- reactive(sanitize_value(input$cols, cols_min, cols_max))

    list(
      rows = rows,
      cols = cols,
      values = reactive(list(rows = rows(), cols = cols()))
    )
  })
}
