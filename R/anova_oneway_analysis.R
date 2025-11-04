# ===============================================================
# 🧪 Table Analyzer — One-way ANOVA Module 
# ===============================================================

one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      tags$details(
        tags$summary(strong("Advanced options")),
        br(),
        stratification_ui("strat", ns)
      ),
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

one_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------
    # Reactive data
    # -----------------------------------------------------------
    df <- filtered_data
    
    # -----------------------------------------------------------
    # Dynamic inputs
    # -----------------------------------------------------------
    responses <- multi_response_server("response", df)

    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]

      tagList(
        multi_response_ui(ns("response")),
        selectInput(
          ns("group"),
          "Categorical predictor:",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    strat_info <- stratification_server("strat", df)
    
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
      req(df(), input$group, input$order)
      resp_vals <- responses()
      req(length(resp_vals) > 0)
      prepare_stratified_anova(
        df = df(),
        responses = resp_vals,
        model = "oneway_anova",
        factor1_var = input$group,
        factor1_order = input$order,
        stratification = strat_info()
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

    df_final <- reactive({
      mod <- models()
      if (is.null(mod)) return(NULL)
      mod$data_used
    })

    model_fit <- reactive({
      mod <- models()
      if (is.null(mod)) return(NULL)
      mod$models
    })

    compiled_results <- reactive({
      mod <- models()
      if (is.null(mod)) return(NULL)
      compile_anova_results(mod)
    })

    summary_table <- reactive({
      res <- compiled_results()
      if (is.null(res)) return(NULL)
      res$summary
    })

    posthoc_results <- reactive({
      res <- compiled_results()
      if (is.null(res)) return(NULL)
      res$posthoc
    })

    effect_table <- reactive({
      res <- compiled_results()
      if (is.null(res)) return(NULL)
      res$effects
    })

    error_table <- reactive({
      res <- compiled_results()
      if (is.null(res)) return(NULL)
      res$errors
    })

    reactive({
      mod <- models()
      if (is.null(mod)) return(NULL)

      data_used <- df_final()

      list(
        analysis_type = "ANOVA",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = posthoc_results(),
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        metadata = list(
          responses = mod$responses,
          strata = mod$strata,
          factors = mod$factors,
          orders = mod$orders,
          errors = error_table()
        ),
        type = "oneway_anova",
        models = model_fit(),
        responses = mod$responses,
        strata = mod$strata,
        factors = mod$factors,
        orders = mod$orders
      )
    })
  })
}
