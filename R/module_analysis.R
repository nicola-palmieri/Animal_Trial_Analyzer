# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Coordinator (fixed + cleaned)
# ===============================================================
source("R/module_analysis_descriptive.R")
source("R/module_analysis_anova_one-way.R")
source("R/module_analysis_anova_two-way.R")
source("R/module_analysis_regression.R")  
source("R/module_analysis_lm.R")
source("R/module_analysis_lmm.R")
source("R/module_analysis_pairwise_correlation.R")
source("R/module_analysis_pca.R")

analysis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 3 — Analyze Results"),
      p("Choose the statistical approach that fits your trial design, then inspect the summaries on the right."),
      hr(),
      tags$style(HTML(sprintf("
        /* Make only this input's dropdown tall enough to show all items */
        #%s + .selectize-control .selectize-dropdown,
        #%s + .selectize-control .selectize-dropdown .selectize-dropdown-content {
          max-height: none !important;   /* show everything, no internal scroll */
        }
      ", ns("analysis_type"), ns("analysis_type")))),
      selectInput(
        ns("analysis_type"),
        "Select analysis type:",
        choices = list(
          " " = "",
          "Descriptive" = c("Descriptive Statistics" = "Descriptive Statistics"),
          "Univariate" = c(
            "One-way ANOVA" = "One-way ANOVA",
            "Two-way ANOVA" = "Two-way ANOVA",
            "Linear Model (LM)" = "Linear Model (LM)",
            "Linear Mixed Model (LMM)" = "Linear Mixed Model (LMM)"
          ),
          "Multivariate" = c(
            "Pairwise Correlation" = "Pairwise Correlation",
            "Principal Component Analysis (PCA)" = "PCA"
          )
        ),
        selected = ""
      ),
      hr(),
      uiOutput(ns("config_panel"))
    ),
    mainPanel(
      width = 8,
      h4("Analysis Results"),
      uiOutput(ns("results_panel"))
    )
  )
}

analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    # --- Mapping between analysis type and submodules ---
    submodules <- list(
      "Descriptive Statistics" = list(
        id = "descriptive",
        ui = descriptive_ui,
        server = descriptive_server,
        type = "descriptive"
      ),
      "One-way ANOVA" = list(
        id = "anova_one",
        ui = one_way_anova_ui,
        server = one_way_anova_server,
        type = "anova"
      ),
      "Two-way ANOVA" = list(
        id = "anova_two",
        ui = two_way_anova_ui,
        server = two_way_anova_server,
        type = "anova"
      ),
      "Linear Model (LM)" = list(
        id = "lm",
        ui = lm_ui,
        server = lm_server,
        type = "lm"
      ),
      "Linear Mixed Model (LMM)" = list(
        id = "lmm",
        ui = lmm_ui,
        server = lmm_server,
        type = "lmm"
      ),
      "Pairwise Correlation" = list(
        id = "ggpairs",
        ui = ggpairs_ui,
        server = ggpairs_server,
        type = "ggpairs"
      ),
      "PCA" = list(
        id = "pca",
        ui = pca_ui,
        server = pca_server,
        type = "pca"
      )
    )
    
    # --- Render selected submodule UI dynamically ---
    current_module_ui <- reactive({
      req(input$analysis_type)
      mod <- submodules[[input$analysis_type]]
      req(mod)
      mod$ui(ns(mod$id))
    })
    
    output$config_panel <- renderUI({
      ui <- current_module_ui()
      req(ui$config)
      ui$config
    })
    
    output$results_panel <- renderUI({
      ui <- current_module_ui()
      req(ui$results)
      ui$results
    })
    
    # --- Dynamically call selected server ---
    model_fit <- reactiveVal(NULL)
    
    observeEvent(input$analysis_type, {
      mod <- submodules[[input$analysis_type]]
      if (is.null(mod)) {
        model_fit(NULL)
        return()
      }

      server_result <- mod$server(mod$id, df)

      if (is.null(server_result) || !is.function(server_result)) {
        model_fit(reactive(list(type = mod$type, models = NULL)))
        return()
      }

      model_fit(reactive({
        raw <- tryCatch(
          server_result(),
          error = function(e) {
            if (inherits(e, "shiny.silent.stop")) {
              return(NULL)
            }
            stop(e)
          }
        )
        
        # Case 1: module returned nothing
        if (is.null(raw)) {
          return(list(type = mod$type, model = NULL, models = NULL))
        }
        
        # Case 2: module returned a list-like structure (e.g. ANOVA)
        if (is.list(raw)) {
          if (is.null(raw$type)) raw$type <- mod$type
          if (!is.null(raw$model) && is.null(raw$models)) raw$models <- raw$model
          if (!is.null(raw$models) && is.null(raw$model)) raw$model <- raw$models
          return(raw)
        }
        
        # Case 3: module returned a bare model object (lm, lmerMod, etc.)
        list(
          type   = mod$type,
          model  = raw,
          models = raw  # alias for compatibility
        )
      }))
      
    }, ignoreNULL = FALSE)
    
    # --- Expose final fitted model ---
    reactive({
      model_reactive <- model_fit()
      req(model_reactive)
      model <- model_reactive()
      req(model)
      model
    })
  })
}
