# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA Module (Refactored)
# ===============================================================

source("R/module_analysis_anova_shared.R")
source("R/module_analysis_multiple_responses.R")


one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      uiOutput(ns("advanced_options")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
      uiOutput(ns("summary_ui"))
    )
  )
}

one_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------
    # Reactive data
    # -----------------------------------------------------------
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # -----------------------------------------------------------
    # Dynamic inputs
    # -----------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        uiOutput(ns("response_inputs")),
        selectInput(
          ns("group"),
          "Categorical predictor:",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    output$response_inputs <- renderUI({
      render_response_inputs(ns, df(), input)
    })
    
    output$advanced_options <- renderUI({
      render_stratification_controls(ns, df, input)
    })
    
    output$strata_order_ui <- renderUI({
      render_strata_order_input(ns, df, input$stratify_var)
    })
    
    # -----------------------------------------------------------
    # Level order selection
    # -----------------------------------------------------------
    output$level_order <- renderUI({
      req(df(), input$group)
      levels <- unique(as.character(df()[[input$group]]))
      selectInput(
        ns("order"),
        "Order of levels (first = reference):",
        choices = levels,
        selected = levels,
        multiple = TRUE
      )
    })
    
    # -----------------------------------------------------------
    # Model fitting (via shared helper)
    # -----------------------------------------------------------
    models <- eventReactive(input$run, {
      req(df(), input$response, input$group, input$order)
      responses <- get_selected_responses(input)
      req(length(responses) > 0)
      
      prepare_stratified_models(
        df = df(),
        responses = responses,
        strat_var = input$stratify_var,
        factor1 = input$group,
        factor2 = NULL,
        orders = list(order1 = input$order, order2 = NULL),
        formula_builder = function(resp, f1, f2) as.formula(paste(resp, "~", f1))
      )
    })
    
    # -----------------------------------------------------------
    # Download all results as one combined DOCX
    # -----------------------------------------------------------
    output$download_all <- downloadHandler(
      
      filename = function() {
        paste0("anova_all_results_", Sys.Date(), ".docx")
      },
      content = function(file) {
        model_info <- models()
        download_all_anova_results(model_info, file)
      }
    )
    
    # -----------------------------------------------------------
    # Render results (shared UI generator)
    # -----------------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "One-way ANOVA")
    })
    
    # -----------------------------------------------------------
    # Render model summaries + download buttons (shared helper)
    # -----------------------------------------------------------
    bind_anova_outputs(ns, output, models)
    
    return(models)
  })
}
