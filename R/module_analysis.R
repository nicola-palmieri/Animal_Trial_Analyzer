# ===============================================================
# 🧪 Table Analyzer — Analysis Coordinator
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

    # --- Initialize all submodules once ---
    handles <- list(
      desc  = descriptive_server("desc", df),
      anova1= one_way_anova_server("anova1", df),
      anova2= two_way_anova_server("anova2", df),
      lm    = lm_server("lm", df),
      lmm   = lmm_server("lmm", df),
      pairs = ggpairs_server("pairs", df),
      pca   = pca_server("pca", df)
    )

    # --- Helper to get current handle ---
    current <- reactive({
      switch(input$analysis_type,
        "Descriptive Statistics" = handles$desc,
        "One-way ANOVA"          = handles$anova1,
        "Two-way ANOVA"          = handles$anova2,
        "Linear Model (LM)"      = handles$lm,
        "Linear Mixed Model (LMM)" = handles$lmm,
        "Pairwise Correlation"   = handles$pairs,
        "PCA"                    = handles$pca
      )
    })

    # --- Render the right config and results panels ---
    output$config_panel <- renderUI({
      req(input$analysis_type)
      mod_ui <- switch(input$analysis_type,
        "Descriptive Statistics" = descriptive_ui(ns("desc")),
        "One-way ANOVA"          = one_way_anova_ui(ns("anova1")),
        "Two-way ANOVA"          = two_way_anova_ui(ns("anova2")),
        "Linear Model (LM)"      = lm_ui(ns("lm")),
        "Linear Mixed Model (LMM)" = lmm_ui(ns("lmm")),
        "Pairwise Correlation"   = ggpairs_ui(ns("pairs")),
        "PCA"                    = pca_ui(ns("pca"))
      )
      mod_ui$config
    })

    output$results_panel <- renderUI({
      req(input$analysis_type)
      mod_ui <- switch(input$analysis_type,
        "Descriptive Statistics" = descriptive_ui(ns("desc")),
        "One-way ANOVA"          = one_way_anova_ui(ns("anova1")),
        "Two-way ANOVA"          = two_way_anova_ui(ns("anova2")),
        "Linear Model (LM)"      = lm_ui(ns("lm")),
        "Linear Mixed Model (LMM)" = lmm_ui(ns("lmm")),
        "Pairwise Correlation"   = ggpairs_ui(ns("pairs")),
        "PCA"                    = pca_ui(ns("pca"))
      )
      mod_ui$results
    })

    # --- Unified model output ---
    reactive({
      res <- current()
      req(res)
      res()
    })
  })
}
