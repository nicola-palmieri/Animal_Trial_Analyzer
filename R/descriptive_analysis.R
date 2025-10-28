# ===============================================================
# 🧾 Animal Trial Analyzer — Descriptive Statistics Module (Aligned Layout)
# ===============================================================

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("advanced_options")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Show summary", width = "100%")),
        column(6, downloadButton(ns("download_summary"), "Download summary", width = "100%"))
      ),
      hr()
    ),
    results = tagList(
      verbatimTextOutput(ns("summary_text"))
    )
  )
}

descriptive_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- filtered_data
    
    # ------------------------------------------------------------
    # Dynamic inputs
    # ------------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      
      tagList(
        h5("Categorical variables:"),
        selectInput(ns("cat_vars"), label = NULL, choices = cat_cols, selected = cat_cols, multiple = TRUE),
        br(),
        h5("Numeric variables:"),
        selectInput(ns("num_vars"), label = NULL, choices = num_cols, selected = num_cols, multiple = TRUE)
      )
    })
    
    output$advanced_options <- renderUI({
      tagList(
        render_stratification_controls(ns, df, input),
        uiOutput(ns("strata_order_ui"))
      )
    })
    
    output$strata_order_ui <- renderUI({
      render_strata_order_input(ns, df, input$stratify_var)
    })
    
    # ------------------------------------------------------------
    # Summary computation
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      req(df())
      raw_data <- df()
      local_data <- raw_data  # create a copy to avoid modifying shared reactive
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      validate(need(length(selected_vars) > 0, "Please select at least one variable."))

      group_var <- if (is.null(input$stratify_var) || input$stratify_var == "None") NULL else input$stratify_var
      if (!guard_stratification_levels(raw_data, group_var)) {
        return(NULL)
      }

      data_columns <- selected_vars

      if (!is.null(group_var)) {
        # keep ONLY selected levels, in the exact order; drop NA and unused levels
        sel <- input$strata_order
        if (!is.null(sel) && length(sel) > 0) {
          local_data <- dplyr::filter(local_data, .data[[group_var]] %in% sel)
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]), levels = sel)
        } else {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]))
        }
        local_data <- droplevels(local_data)

        data_columns <- unique(c(data_columns, group_var))
      }

      data_columns <- data_columns[!is.na(data_columns) & nzchar(data_columns)]
      data_columns <- intersect(data_columns, names(local_data))
      local_data <- local_data[, data_columns, drop = FALSE]

      if (!is.null(group_var) && !is.null(input$strata_order)) {
        if (group_var %in% names(local_data)) {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]),
                                            levels = input$strata_order)
        }
      }

      strata_levels <- if (!is.null(group_var) && group_var %in% names(local_data)) {
        levels(local_data[[group_var]])
      } else {
        NULL
      }

      list(
        summary = compute_descriptive_summary(local_data, group_var),
        selected_vars = selected_vars,
        group_var = group_var,
        processed_data = local_data,
        strata_levels = strata_levels
      )
    })
    
    
    
    # ------------------------------------------------------------
    # Print summary
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      print_summary_sections(summary_data()$summary)
    })
    
    # ------------------------------------------------------------
    # Download
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        results <- summary_data()
        req(results)
        sink(file)
        on.exit(sink(), add = TRUE)
        print_summary_sections(results$summary)
      }
    )
    
    # ------------------------------------------------------------
    # Return full model info
    # ------------------------------------------------------------
    return(reactive({
      details <- summary_data()
      if (is.null(details)) {
        return(NULL)
      }

      list(
        type = "descriptive",
        data = df,
        summary = reactive({ details <- summary_data(); req(details); details$summary }),
        selected_vars = reactive({ details <- summary_data(); req(details); details$selected_vars }),
        group_var = reactive({ details <- summary_data(); req(details); details$group_var }),
        processed_data = reactive({ details <- summary_data(); req(details); details$processed_data }),
        strata_levels = reactive({ details <- summary_data(); req(details); details$strata_levels })
      )
    }))
    
  })
}

