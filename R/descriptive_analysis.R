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
        column(6, actionButton(ns("run"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_summary"), "Download Summary", width = "100%"))
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
      render_stratification_controls(ns, df, input)
    })
    
    # ------------------------------------------------------------
    # Summary computation
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      data <- df()
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      validate(need(length(selected_vars) > 0, "Please select at least one variable."))
      
      group_var <- if (is.null(input$stratify_var) || input$stratify_var == "None") NULL else input$stratify_var
      if (!is.null(group_var) && !(group_var %in% selected_vars)) {
        selected_vars <- c(selected_vars, group_var)
      }
      
      data <- data[, selected_vars, drop = FALSE]
      list(
        summary = compute_descriptive_summary(data, group_var),
        selected_vars = selected_vars,
        group_var = group_var
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
        sink(file)
        print_summary_sections(summary_data()$summary)
        sink()
      }
    )
    
    # ------------------------------------------------------------
    # Return full model info
    # ------------------------------------------------------------
    return(reactive({
      list(
        type = "descriptive",
        data = df,
        summary = reactive(summary_data()$summary),
        selected_vars = reactive(summary_data()$selected_vars),
        group_var = reactive(summary_data()$group_var)
      )
    }))
    
  })
}

