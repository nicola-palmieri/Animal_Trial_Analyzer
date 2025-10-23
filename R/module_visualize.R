# ===============================================================
# 🧩 Visualization Coordinator
# ===============================================================

source("R/module_visualize_anova_one-way.R")
# later: source("R/visualize_pca.R"), etc.

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
      print(type)
      if (type == "oneway_anova") {
        visualize_oneway_ui(ns("oneway"))
      } else {
        div("Visualization not yet implemented for this analysis type.")
      }
    })
    
    # call appropriate submodule
    observe({
      type <- analysis_type()
      if (type == "oneway_anova") {
        visualize_oneway_server("oneway", filtered_data, model_info)
      }
    })
  })
}
