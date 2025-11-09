# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

MAX_STRATIFICATION_LEVELS <- 10

get_categorical_columns <- function(df) {
  names(df)[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]
}

stratification_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("stratify_var_ui")),
    uiOutput(ns("strata_order_ui"))
  )
}

stratification_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactive({
      data_obj <- if (is.function(data)) data() else data
      req(is.data.frame(data_obj))
      validate(need(nrow(data_obj) > 0, "No data available for stratification."))
      data_obj
    })

    output$stratify_var_ui <- renderUI({
      data <- req(df())
      cat_cols <- get_categorical_columns(data)

      if (length(cat_cols) == 0) {
        return(helpText("No categorical columns available for stratification."))
      }

      with_help_tooltip(
        selectInput(
          ns("stratify_var"),
          "Stratify by:",
          choices = c("None", cat_cols),
          selected = "None"
        ),
        "Tip: Choose a categorical variable to split analyses by group."
      )
    })

    output$strata_order_ui <- renderUI({
      req(input$stratify_var)
      if (input$stratify_var == "None") return(NULL)

      data <- req(df())
      selected_values <- data[[input$stratify_var]]
      selected_values <- selected_values[!is.na(selected_values)]

      available_levels <- if (is.factor(selected_values)) {
        levels(selected_values)
      } else {
        unique(as.character(selected_values))
      }
      available_levels <- available_levels[nzchar(available_levels)]

      n_levels <- length(available_levels)
      validate(need(
        n_levels <= MAX_STRATIFICATION_LEVELS,
        sprintf("'%s' has too many levels (%d > %d).",
                input$stratify_var, n_levels, MAX_STRATIFICATION_LEVELS)
      ))

      with_help_tooltip(
        selectInput(
          ns("strata_order"),
          "Order of levels:",
          choices = available_levels,
          selected = available_levels,
          multiple = TRUE
        ),
        "Tip: Adjust which levels to include and in what order they appear."
      )
    })

    reactive({
      list(
        var = if (identical(input$stratify_var, "None")) NULL else input$stratify_var,
        levels = input$strata_order
      )
    })
  })
}

  

