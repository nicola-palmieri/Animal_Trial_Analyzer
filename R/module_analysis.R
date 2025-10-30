# ===============================================================
# 🧪 Animal Trial Analyzer — Analysis Coordinator (clean + stable)
# ===============================================================

analysis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 3 — Analyze results"),
      p("Select the statistical approach that fits your trial design, then inspect the summaries on the right."),
      hr(),
      
      # --- CSS: expand dropdown height for better visibility ---
      tags$style(HTML(sprintf("
        #%s + .selectize-control .selectize-dropdown,
        #%s + .selectize-control .selectize-dropdown .selectize-dropdown-content {
          max-height: none !important;
        }
      ", ns("analysis_type"), ns("analysis_type")))),
      
      # --- Analysis type selector ---
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
      h4("Analysis results"),
      uiOutput(ns("results_panel"))
    )
  )
}


analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    # --- Submodule mapping ---
    modules <- list(
      "Descriptive Statistics" = list(id = "desc", ui = descriptive_ui, server = descriptive_server, type = "desc"),
      "One-way ANOVA"          = list(id = "anova1", ui = one_way_anova_ui, server = one_way_anova_server, type = "anova1"),
      "Two-way ANOVA"          = list(id = "anova2", ui = two_way_anova_ui, server = two_way_anova_server, type = "anova2"),
      "Linear Model (LM)"      = list(id = "lm", ui = lm_ui, server = lm_server, type = "lm"),
      "Linear Mixed Model (LMM)" = list(id = "lmm", ui = lmm_ui, server = lmm_server, type = "lmm"),
      "Pairwise Correlation"   = list(id = "pairs", ui = ggpairs_ui, server = ggpairs_server, type = "pairs"),
      "PCA"                    = list(id = "pca", ui = pca_ui, server = pca_server, type = "pca")
    )
    
    # --- Get current module definition ---
    current_mod <- reactive({
      req(input$analysis_type)
      modules[[input$analysis_type]]
    })
    
    # --- Render submodule UI ---
    output$config_panel <- renderUI({
      ui <- current_mod()$ui(ns(current_mod()$id))
      req(ui$config)
      ui$config
    })
    
    output$results_panel <- renderUI({
      ui <- current_mod()$ui(ns(current_mod()$id))
      req(ui$results)
      ui$results
    })
    
    # --- Run submodule server and normalize its output ---
    model_out <- reactiveVal(NULL)
    
    observeEvent(input$analysis_type, {
      mod <- current_mod()
      if (is.null(mod)) return(model_out(NULL))
      
      result <- tryCatch(
        mod$server(mod$id, df),
        error = function(e) NULL
      )
      
      if (is.null(result)) {
        model_out(reactive({ list(type = mod$type, model = NULL) }))
        return()
      }
      
      if (is.reactive(result)) {
        model_out(reactive({
          val <- result()
          if (is.list(val)) modifyList(list(type = mod$type), val)
          else list(type = mod$type, model = val)
        }))
      } else if (is.list(result)) {
        model_out(reactive({ modifyList(list(type = mod$type), result) }))
      } else {
        model_out(reactive({ list(type = mod$type, model = result) }))
      }
    }, ignoreNULL = FALSE)
    
    # --- Expose unified model output ---
    reactive({
      res <- model_out()
      req(res)
      res()
    })
  })
}
