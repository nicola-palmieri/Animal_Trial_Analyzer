# ===============================================================
# 🧪 Table Analyzer — Two-way ANOVA Module
# ===============================================================

two_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order_1")),
      uiOutput(ns("level_order_2")),
      uiOutput(ns("advanced_options")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Show results", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"))
      )
    ),
    results = tagList(
      uiOutput(ns("summary_ui"))
    )
  )
}

two_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------
    # Reactive data
    # -----------------------------------------------------------
    df <- filtered_data
    
    # -----------------------------------------------------------
    # Dynamic inputs
    # -----------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        checkboxInput(ns("multi_resp"), "Allow multiple response variables", value = FALSE),
        uiOutput(ns("response_selector")),
        selectInput(
          ns("factor1"),
          "Categorical predictor 1 (x-axis):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        ),
        selectInput(
          ns("factor2"),
          "Categorical predictor 2 (lines):",
          choices = cat_cols,
          selected = if (length(cat_cols) > 1) cat_cols[2] else NULL
        )
      )
    })
    
    output$response_selector <- renderUI({
      render_response_selector(ns, df, input)
    })
    
    output$stratification_controls <- stratification_ui(ns("strat"))
    output$advanced_options <- renderUI({
      tags$details(
        tags$summary(strong(STRAT_SECTION_TITLE)),
        uiOutput(ns("stratification_controls"))
      )
    })
    strat_info <- stratification_server("strat", df)
    
    # -----------------------------------------------------------
    # Level order selections
    # -----------------------------------------------------------
    output$level_order_1 <- renderUI({
      req(df(), input$factor1)
      levels1 <- unique(as.character(df()[[input$factor1]]))
      selectInput(
        ns("order1"),
        paste("Order of levels (first = reference):", input$factor1, "(x-axis)"),
        choices = levels1,
        selected = levels1,
        multiple = TRUE
      )
    })
    
    output$level_order_2 <- renderUI({
      req(df(), input$factor2)
      levels2 <- unique(as.character(df()[[input$factor2]]))
      selectInput(
        ns("order2"),
        paste("Order of levels (first = reference):", input$factor2, "(lines)"),
        choices = levels2,
        selected = levels2,
        multiple = TRUE
      )
    })
    
    # -----------------------------------------------------------
    # Model fitting (via shared helper)
    # -----------------------------------------------------------
    models <- eventReactive(input$run, {
      req(df(), input$response, input$factor1, input$order1, input$factor2, input$order2)
      responses <- get_selected_responses(input)
      prepare_stratified_anova(
        df = df(),
        responses = responses,
        model = "twoway_anova",
        factor1_var = input$factor1,
        factor1_order = input$order1,
        factor2_var = input$factor2,
        factor2_order = input$order2,
        stratification = strat_info()
      )
    })
    
    

    # -----------------------------------------------------------
    # Download all results as one combined DOCX
    # -----------------------------------------------------------
    output$download_all <- downloadHandler(
      filename = function() {
        model_info <- models()
        if (is.null(model_info)) return("anova_results.docx")
        
        n_resp <- length(model_info$responses)
        n_strata <- if (is.null(model_info$strata)) 0 else length(model_info$strata$levels)
        strata_label <- ifelse(n_strata == 0, "nostratum", paste0(n_strata, "strata"))
        timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
        sprintf("anova_results_%sresp_%s_%s.docx", n_resp, strata_label, timestamp)
      },
      content = function(file) {
        model_info <- models()
        if (is.null(model_info)) stop("Please run the ANOVA first.")
        download_all_anova_results(model_info, file)
      }
    )
    
    # -----------------------------------------------------------
    # Render results
    # -----------------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "Two-way ANOVA")
    })
    
    # -----------------------------------------------------------
    # Render model summaries + downloads (shared helper)
    # -----------------------------------------------------------
    bind_anova_outputs(ns, output, models)
    
    return(models)
  })
}
