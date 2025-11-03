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
    
    # ---- Mapping of available modules ----
    modules <- list(
      "Descriptive Statistics" = list(id = "desc",  ui = descriptive_ui, server = descriptive_server, type = "desc"),
      "One-way ANOVA"          = list(id = "anova1", ui = one_way_anova_ui, server = one_way_anova_server, type = "anova1"),
      "Two-way ANOVA"          = list(id = "anova2", ui = two_way_anova_ui, server = two_way_anova_server, type = "anova2"),
      "Linear Model (LM)"      = list(id = "lm",     ui = lm_ui, server = lm_server, type = "lm"),
      "Linear Mixed Model (LMM)" = list(id = "lmm",  ui = lmm_ui, server = lmm_server, type = "lmm"),
      "Pairwise Correlation"   = list(id = "pairs",  ui = ggpairs_ui, server = ggpairs_server, type = "pairs"),
      "PCA"                    = list(id = "pca",    ui = pca_ui, server = pca_server, type = "pca")
    )
    
    # ---- Cache for lazily created servers ----
    server_cache <- reactiveValues()
    
    # ---- Current module getter ----
    current_mod <- reactive({
      type <- input$analysis_type
      if (is.null(type) || !nzchar(type)) return(NULL)
      modules[[type]]
    })
    
    current_ui <- reactive({
      mod <- current_mod()
      if (is.null(mod)) return(NULL)
      mod$ui(ns(mod$id))
    })
    
    # ---- Lazy server initialization ----
    ensure_module_server <- function(mod) {
      key <- mod$id
      if (!is.null(server_cache[[key]])) return(server_cache[[key]])
      
      result <- tryCatch(mod$server(mod$id, df), error = function(e) {
        warning(sprintf("Module '%s' failed to initialize: %s", key, conditionMessage(e)))
        NULL
      })
      
      # Wrap result into a reactive returning a list with 'type' and 'model'
      if (is.null(result)) {
        server_cache[[key]] <- reactive(list(type = mod$type, model = NULL))
      } else if (is.reactive(result)) {
        server_cache[[key]] <- reactive({
          val <- result()
          # If submodule already returns a list, just add 'type' key
          if (is.list(val)) c(val, list(type = mod$type))
          else list(type = mod$type, model = val)
        })
      } else {
        # Submodule returned a non-reactive object
        server_cache[[key]] <- reactive(list(type = mod$type, model = result))
      }
      
      server_cache[[key]]
    }
    
    # ---- Render active submodule UI ----
    output$config_panel <- renderUI({
      ui <- current_ui()
      if (is.null(ui)) return(NULL)
      ui$config
    })
    
    output$results_panel <- renderUI({
      ui <- current_ui()
      if (is.null(ui)) return(NULL)
      ui$results
    })
    
    # ---- Connect the current selected module's server ----
    current_server <- reactive({
      mod <- current_mod()
      if (is.null(mod)) return(NULL)
      ensure_module_server(mod)
    })
    
    # ---- Unified model output ----
    model_out <- reactive({
      srv <- current_server()
      if (is.null(srv)) return(NULL)
      srv()
    })
    
    # Return the active model output as a reactive
    model_out
  })
}
