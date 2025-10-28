# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (GGpairs)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize pairwise correlation"),
      p("Visualize pairwise relationships and correlation coefficients among numeric variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c("Pairwise correlation matrix" = "ggpairs"),  # ✅ single option for now
        selected = "ggpairs"
      ),
      hr(),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("plot_width"),
            label = "Plot width (px)",
            value = 800,
            min = 400,
            max = 2000,
            step = 100
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("plot_height"),
            label = "Plot height (px)",
            value = 600,
            min = 400,
            max = 2000,
            step = 100
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download plot")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plots"))
    )
  )
}


visualize_ggpairs_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data and model
    df <- reactive(filtered_data())
    model_info <- reactive(model_fit())
    
    # Layout info placeholder
    output$layout_controls <- renderUI({
      info <- model_info()
      if (is.null(info) || info$type != "ggpairs") return(NULL)
      tagList(
        p("This plot shows pairwise scatterplots and Pearson correlations among numeric variables.")
      )
    })
    
    # Determine plot size dynamically
    plot_size <- reactive({
      list(w = input$plot_width, h = input$plot_height)
    })
    
    # --- Main Plot Rendering ---
    output$plots <- renderPlot({
      info <- model_info()
      if (is.null(info) || info$type != "ggpairs") return(NULL)
      validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
      
      # use visualization type (only one for now)
      if (input$plot_type == "ggpairs") {
        build_ggpairs_plot(info$data)
      }
    },
    width  = function() plot_size()$w,
    height = function() plot_size()$h,
    res    = 96)
    
    # --- Download handler ---
    output$download_plot <- downloadHandler(
      filename = function() paste0("ggpairs_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- model_info()
        req(info)
        validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
        
        g <- if (input$plot_type == "ggpairs") build_ggpairs_plot(info$data)
        
        width_in  <- input$plot_width / 96
        height_in <- input$plot_height / 96
        
        ggsave(
          filename = file,
          plot = g,
          device = "png",
          dpi = 300,
          width = width_in,
          height = height_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
