# ===============================================================
# 🧩 Visualization Layout Helpers
# ===============================================================

# ---- 1. ANOVA layout controls ----
build_anova_layout_controls <- function(ns, input, info, default_ui_value) {
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  n_responses <- if (!is.null(info$responses)) length(info$responses) else 0
  
  strata_inputs <- if (has_strata) {
    tagList(
      h5("Across strata:"),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("strata_rows"),
            "Grid rows",
            value = isolate(default_ui_value(input$strata_rows)),
            min = 0,
            step = 1
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("strata_cols"),
            "Grid columns",
            value = isolate(default_ui_value(input$strata_cols)),
            min = 0,
            step = 1
          )
        )
      )
    )
  } else {
    NULL
  }
  
  response_inputs <- if (!is.null(n_responses) && n_responses > 1) {
    tagList(
      h5("Across responses:"),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("resp_rows"),
            "Grid rows",
            value = isolate(default_ui_value(input$resp_rows)),
            min = 0,
            step = 1
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("resp_cols"),
            "Grid columns",
            value = isolate(default_ui_value(input$resp_cols)),
            min = 0,
            step = 1
          )
        )
      )
    )
  } else {
    NULL
  }
  
  tagList(
    h4("Layout Controls"),
    strata_inputs,
    response_inputs
  )
}

# ---- 2. ggpairs layout controls ----
build_ggpairs_layout_controls <- function() {
  tagList(
    h4("Layout Controls"),
    helpText("Pairwise correlation plots do not support grid layouts. Adjust the plot size inputs below to resize the matrix.")
  )
}

# ---- 3. PCA layout controls ----
build_pca_layout_controls <- function(ns, data) {
  if (is.null(data)) return(NULL)
  
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  
  tagList(
    h4("PCA Plot Controls"),
    if (length(cat_vars) > 0) {
      tagList(
        selectInput(ns("pca_color"), "Color by:", choices = c("None", cat_vars), selected = "None"),
        selectInput(ns("pca_shape"), "Shape by:", choices = c("None", cat_vars), selected = "None")
      )
    } else {
      helpText("No categorical variables available for coloring or shaping.")
    }
  )
}


compute_layout <- function(n_items, rows_input, cols_input) {
  # Safely handle nulls
  if (is.null(n_items) || length(n_items) == 0 || is.na(n_items) || n_items <= 0) {
    return(list(nrow = 1, ncol = 1))
  }
  
  # Replace NULL or NA inputs with 0
  if (is.null(rows_input) || is.na(rows_input)) rows_input <- 0
  if (is.null(cols_input) || is.na(cols_input)) cols_input <- 0
  
  n_row_input <- suppressWarnings(as.numeric(rows_input))
  n_col_input <- suppressWarnings(as.numeric(cols_input))
  
  # Handle invalid inputs
  if (is.na(n_row_input)) n_row_input <- 0
  if (is.na(n_col_input)) n_col_input <- 0
  
  if (n_row_input > 0) {
    n_row_final <- n_row_input
    if (n_col_input > 0) {
      n_col_final <- max(n_col_input, ceiling(n_items / max(1, n_row_final)))
    } else {
      n_col_final <- ceiling(n_items / max(1, n_row_final))
    }
  } else if (n_col_input > 0) {
    n_col_final <- n_col_input
    n_row_final <- ceiling(n_items / max(1, n_col_final))
  } else {
    # Default heuristic: single row if <=5 items, otherwise two
    n_row_final <- ifelse(n_items <= 5, 1, 2)
    n_col_final <- ceiling(n_items / n_row_final)
  }
  
  list(
    nrow = max(1, as.integer(n_row_final)),
    ncol = max(1, as.integer(n_col_final))
  )
}

build_layout_controls_for_type <- function(ns, input, info, default_ui_value, data_for_pca = NULL) {
  current_type <- if (!is.null(info$type)) info$type else "anova"

  if (identical(current_type, "anova") || identical(current_type, "two_way_anova")) {
    return(build_anova_layout_controls(ns, input, info, default_ui_value))
  }

  if (identical(current_type, "ggpairs")) {
    return(build_ggpairs_layout_controls())
  }

  if (identical(current_type, "pca")) {
    return(build_pca_layout_controls(ns, data_for_pca))
  }

  NULL
}

