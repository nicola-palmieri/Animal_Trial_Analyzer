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
    df <- use_filtered_df(filtered_data)
    
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
    
    bind_anova_stratification_ui(ns, output, df, input)
    
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
        model_info <- models()
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
