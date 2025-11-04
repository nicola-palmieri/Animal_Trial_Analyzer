# ===============================================================
# 🧩 Visualization Coordinator (Lazy + Reactive)
# ===============================================================

visualize_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dynamic_ui"))
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------
    # 1️⃣ Reactive model info
    # -----------------------------------------------------------
    model_info <- reactive({
      req(model_fit())
      model_fit()
    })
    
    # -----------------------------------------------------------
    # 2️⃣ Detect analysis type (robust default)
    # -----------------------------------------------------------
    analysis_type <- reactive({
      info <- model_info()
      type <- info$type %||% "oneway_anova"
      tolower(type)
    })
    
    # -----------------------------------------------------------
    # 3️⃣ Visualization cache (like analysis_server)
    # -----------------------------------------------------------
    vis_cache <- reactiveValues()
    
    ensure_vis_server <- function(key, create_fn) {
      if (!is.null(vis_cache[[key]])) {
        return(vis_cache[[key]])
      }
      vis_cache[[key]] <- create_fn()
      vis_cache[[key]]
    }
    
    # -----------------------------------------------------------
    # 4️⃣ Dynamic UI loading (lazy initialization)
    # -----------------------------------------------------------
    output$dynamic_ui <- renderUI({
      type <- analysis_type()
      switch(
        type,
        "oneway_anova"   = visualize_oneway_ui(ns("oneway")),
        "twoway_anova"   = visualize_twoway_ui(ns("twoway")),
        "pairs"          = visualize_ggpairs_ui(ns("ggpairs")),
        "pca"            = visualize_pca_ui(ns("pca"), filtered_data()),
        "descriptive"    = visualize_descriptive_ui(ns("descriptive")),
        div("Visualization not yet implemented for this analysis type.")
      )
    })
    
    # -----------------------------------------------------------
    # 5️⃣ Attach or reuse visualization servers lazily
    # -----------------------------------------------------------
    observeEvent(analysis_type(), {
      type <- analysis_type()
      
      if (type == "oneway_anova") {
        ensure_vis_server("oneway", function() {
          visualize_oneway_server("oneway", filtered_data, model_info)
        })
      } else if (type == "twoway_anova") {
        ensure_vis_server("twoway", function() {
          visualize_twoway_server("twoway", filtered_data, model_info)
        })
      } else if (type == "pairs") {
        ensure_vis_server("ggpairs", function() {
          visualize_ggpairs_server("ggpairs", filtered_data, model_info)
        })
      } else if (type == "pca") {
        ensure_vis_server("pca", function() {
          visualize_pca_server("pca", filtered_data, model_info)
        })
      } else if (type == "descriptive") {
        ensure_vis_server("descriptive", function() {
          visualize_descriptive_server("descriptive", filtered_data, model_info)
        })
      }
    }, ignoreInit = FALSE)
    
  })
}
