# ===============================================================
# 🧩 Visualization Layout Helpers
# ===============================================================

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

build_ggpairs_layout_controls <- function() {
  tagList(
    h4("Layout Controls"),
    helpText("Pairwise correlation plots do not support grid layouts. Adjust the plot size inputs below to resize the matrix.")
  )
}


build_pca_layout_controls <- function(ns, data) {
  if (is.null(data)) return(NULL)
  
  cat_vars <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
  all_vars <- names(data)
  
  tagList(
    h4("PCA Plot Controls"),
    if (length(cat_vars) > 0) {
      tagList(
        selectInput(ns("pca_color"), "Color by:", choices = c("None", cat_vars), selected = "None"),
        selectInput(ns("pca_shape"), "Shape by:", choices = c("None", cat_vars), selected = "None")
      )
    } else {
      helpText("No categorical variables available for coloring or shaping.")
    },
    selectInput(ns("pca_label"), "Label points with:", choices = c("None", all_vars), selected = "None"),
    sliderInput(ns("pca_label_size"), "Label size", min = 1, max = 5, step = 0.2, value = 2)
  )
}


