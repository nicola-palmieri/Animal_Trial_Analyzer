# ===============================================================
# 🧩 Visualization Coordinator
# ===============================================================

visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_ui"))
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # reactive model info
    model_info <- reactive(model_fit())
    
    # detect analysis type
    analysis_type <- reactive({
      info <- model_info()
      if (is.null(info$type)) return("oneway_anova")  # default
      info$type
    })
    
    # dynamic UI placeholder
    output$dynamic_ui <- renderUI({
      type <- analysis_type()
      if (type == "oneway_anova") {
        visualize_oneway_ui(ns("oneway"))
      } else if (type == "twoway_anova") {
        visualize_twoway_ui(ns("twoway"))
      } else if (type == "ggpairs") {
        visualize_ggpairs_ui(ns("ggpairs"))
      } else if (type == "pca") {
        visualize_pca_ui(ns("pca")) 
      } else {
        div("Visualization not yet implemented for this analysis type.")
      }
    })
    
    observe({
      type <- analysis_type()
      if (type == "oneway_anova") {
        visualize_oneway_server("oneway", filtered_data, model_info)
      } else if (type == "twoway_anova") {
        visualize_twoway_server("twoway", filtered_data, model_info)
      } else if (type == "ggpairs") {
        visualize_ggpairs_server("ggpairs", filtered_data, model_info)
      } else if (type == "pca") {
        visualize_pca_server("pca", filtered_data, model_info)
      }
    })
    
  })
}
