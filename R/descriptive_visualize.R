# ===============================================================
# Visualization Module — Descriptive Statistics
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 5 — Visualize Descriptive Statistics"),
      p("Explore distributions, variability, and normality across variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c(
          "Categorical Distributions" = "categorical",
          "Numeric Boxplots" = "boxplots",
          "Numeric Histograms" = "histograms",
          "CV (%)" = "cv",
          "Outlier Counts" = "outliers",
          "Missingness (%)" = "missing",
          "Shapiro–Wilk p-values" = "shapiro"
        ),
        selected = "boxplots"
      ),
      hr(),
      numericInput(
        ns("plot_width"),
        label = "Plot width (px)",
        value = 800,
        min = 400,
        max = 2000,
        step = 100
      ),
      numericInput(
        ns("plot_height"),
        label = "Plot height (px)",
        value = 600,
        min = 400,
        max = 2000,
        step = 100
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      h4("Descriptive Visualization"),
      plotOutput(ns("plot"))
    )
  )
}


visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    
    # ------------------------------------------------------------
    # 1️⃣ Base reactive: Data + summary check
    # ------------------------------------------------------------
    df <- reactive({
      data <- filtered_data()
      validate(need(!is.null(data) && is.data.frame(data), "No data available."))
      data
    })
    
    summary_info <- reactive({
      info <- descriptive_summary()
      validate(need(!is.null(info), "Run descriptive summary first."))
      validate(need(!is.null(info$summary), "Summary not available."))
      info
    })
    
    plot_size <- reactive({
      w <- suppressWarnings(as.numeric(input$plot_width))
      h <- suppressWarnings(as.numeric(input$plot_height))
      list(
        w = ifelse(is.na(w) || w <= 0, 800, w),
        h = ifelse(is.na(h) || h <= 0, 600, h)
      )
    })
    
    # ------------------------------------------------------------
    # 2️⃣ Build all plots using the shared helper
    # ------------------------------------------------------------
    plots_all <- reactive({
      info <- summary_info()
      
      summary_data <- info$summary
      if (is.reactive(summary_data)) summary_data <- summary_data()
      
      selected_vars <- info$selected_vars
      if (is.reactive(selected_vars)) selected_vars <- selected_vars()
      
      data_subset <- df()
      if (!is.null(selected_vars)) {
        data_subset <- data_subset[, intersect(names(data_subset), selected_vars), drop = FALSE]
      }
      
      build_descriptive_plots(summary_data, data_subset)
    })
    
    # ------------------------------------------------------------
    # 3️⃣ Pick correct plot based on selection
    # ------------------------------------------------------------
    build_current_plot <- reactive({
      plots <- plots_all()
      validate(need(!is.null(plots), "No plots available."))
      
      metrics <- plots$metrics
      switch(
        input$plot_type,
        categorical = plots$factors,
        boxplots = plots$boxplots,
        histograms = plots$histograms,
        cv = if (!is.null(metrics)) metrics$cv else NULL,
        outliers = if (!is.null(metrics)) metrics$outliers else NULL,
        missing = if (!is.null(metrics)) metrics$missing else NULL,
        shapiro = if (!is.null(metrics)) metrics$shapiro else NULL,
        NULL
      )
    })
    
    # ------------------------------------------------------------
    # 4️⃣ Render plot safely
    # ------------------------------------------------------------
    output$plot <- renderPlot({
      p <- build_current_plot()
      validate(need(!is.null(p), "Selected plot not available."))
      # patchwork objects are fine with print()
      print(p)
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    # ------------------------------------------------------------
    # 5️⃣ Download
    # ------------------------------------------------------------
    output$download_plot <- downloadHandler(
      filename = function() paste0("descriptive_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- build_current_plot()
        validate(need(!is.null(p), "No plot available to download."))
        
        size <- plot_size()
        ggsave(
          filename = file,
          plot = p,
          device = "png",
          dpi = 300,
          width = size$w / 96,
          height = size$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
