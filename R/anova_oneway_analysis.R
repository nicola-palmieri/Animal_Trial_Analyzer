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
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show results", width = "100%"),
          "Help: Run the ANOVA using the selected response and group variable."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"),
          "Help: Export the ANOVA summaries, post-hoc tests, and diagnostics."
        ))
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
        with_help_tooltip(
          selectInput(
            ns("group"),
            "Categorical predictor:",
            choices = cat_cols,
            selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
          ),
          "Help: Choose the grouping variable that defines the comparison categories."
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
      with_help_tooltip(
        selectInput(
          ns("order"),
          "Order of levels (first = reference):",
          choices = levels,
          selected = levels,
          multiple = TRUE
        ),
        "Help: Arrange the group levels; the first level is used as the reference in outputs."
      )
    })
    
    # -----------------------------------------------------------
    # Model fitting (via shared helper)
    # -----------------------------------------------------------
    models <- eventReactive(input$run, {
      req(df(), input$group, input$order)
      resp_vals <- responses()
      validate(
        need(length(resp_vals) > 0, "Please select at least one response variable."),
        need(all(input$order %in% unique(df()[[input$group]])), "Invalid level order.")
      )
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
        req(model_info)
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
      req(mod)
      mod$data_used
    })

    model_fit <- reactive({
      mod <- models()
      req(mod)
      mod$models
    })

    compiled_results <- reactive({
      mod <- models()
      req(mod)
      compile_anova_results(mod)
    })

    summary_table <- reactive({
      res <- compiled_results()
      req(res)
      res$summary
    })

    posthoc_results <- reactive({
      res <- compiled_results()
      req(res)
      res$posthoc
    })

    effect_table <- reactive({
      res <- compiled_results()
      req(res)
      res$effects
    })

    error_table <- reactive({
      res <- compiled_results()
      req(res)
      res$errors
    })

    reactive({
      mod <- models()
      req(mod)

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
