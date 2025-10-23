# ===============================================================
# 🧪 Visualization Module — One-way ANOVA
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize One-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c("Mean ± SE" = "mean_se"),
        selected = "mean_se"
      ),
      hr(),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, numericInput(ns("plot_width"), "Subplot width (px)", value = 300, min = 200, max = 1200, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 200, min = 200, max = 1200, step = 50))
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
    
    layout_state <- initialize_layout_state(input, session)
    
    plot_info <- reactive({
      info <- model_info()
      if (is.null(info) || info$type != "oneway_anova") return(NULL)
      data <- df()
      build_anova_plot_info(data, info, layout_state$effective_input)
    })
    
    observe_layout_synchronization(plot_info, layout_state, session)
    
    plot_obj <- reactive({
      info <- plot_info()
      if (is.null(info)) return(NULL)
      info$plot
    })
    
    plot_size <- reactive({
      info <- plot_info()
      if (is.null(info)) return(list(w = input$plot_width, h = input$plot_height))
      s <- plot_info()$layout
      list(
        w = input$plot_width * s$strata$cols * s$responses$ncol,
        h = input$plot_height * s$strata$rows * s$responses$nrow
      )
    })
    
    output$layout_controls <- renderUI({
      info <- model_info()
      if (is.null(info) || info$type != "oneway_anova") return(NULL)
      build_anova_layout_controls(ns, input, info, layout_state$default_ui_value)
    })
    
    output$plot <- renderPlot({
      info <- model_info()
      req(info, input$plot_type)
      
      # --- Only one option for now ---
      if (input$plot_type == "mean_se") {
        req(plot_obj())
        return(plot_obj())
      }
    
    }, width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(plot_obj())
        ggsave(
          filename = file,
          plot = plot_obj(),
          device = "png",
          dpi = 300,
          width = plot_size()$w / 96,
          height = plot_size()$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
