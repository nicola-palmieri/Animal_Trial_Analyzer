# ===============================================================
# 🧪 Visualization Module — PCA (Biplot)
# ===============================================================

visualize_pca_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Principal Component Analysis (PCA)"),
      p("Visualize multivariate structure using a PCA biplot."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c("PCA biplot" = "biplot"),
        selected = "biplot"
      ),
      hr(),
      selectInput(
        ns("pca_color"),
        label = "Color points by:",
        choices = "None",
        selected = "None"
      ),
      selectInput(
        ns("pca_shape"),
        label = "Shape points by:",
        choices = "None",
        selected = "None"
      ),
      selectInput(
        ns("pca_label"),
        label = "Label points by:",
        choices = "None",
        selected = "None"
      ),
      numericInput(
        ns("pca_label_size"),
        label = "Label size:",
        value = 2,
        min = 0.5,
        max = 6,
        step = 0.5
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
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      h4("PCA Biplot"),
      plotOutput(ns("plot"))
    )
  )
}


visualize_pca_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -- Reactives ------------------------------------------------
    df <- reactive({
      d <- filtered_data()
      validate(need(!is.null(d) && is.data.frame(d), "No data available."))
      d
    })
    
    info <- reactive({
      mi <- model_fit()
      validate(need(!is.null(mi) && identical(mi$type, "pca"), "Run PCA first."))
      validate(need(!is.null(mi$model), "PCA model missing."))
      mi
    })
    
    # -- Populate controls when data changes ----------------------
    observeEvent(df(), {
      d <- df()
      
      all_cols <- names(d)
      print(all_cols)
      # Build choices; keep "None" first
      choices <- c("None", all_cols)
      
      # Update inputs
      updateSelectInput(session, "pca_color", choices = choices)
      updateSelectInput(session, "pca_shape", choices = choices)
      updateSelectInput(session, "pca_label", choices = choices)
      
      # If current selections are invalid, reset to "None"
      if (is.null(input$pca_color) || !(input$pca_color %in% choices)) {
        updateSelectInput(session, "pca_color", selected = "None")
      }
      if (is.null(input$pca_shape) || !(input$pca_shape %in% choices)) {
        updateSelectInput(session, "pca_shape", selected = "None")
      }
      if (is.null(input$pca_label) || !(input$pca_label %in% choices)) {
        updateSelectInput(session, "pca_label", selected = "None")
      }
    }, ignoreInit = FALSE)
    
    # -- Size reactive --------------------------------------------
    plot_size <- reactive({
      w <- suppressWarnings(as.numeric(input$plot_width));  if (is.na(w) || w <= 0) w <- 800
      h <- suppressWarnings(as.numeric(input$plot_height)); if (is.na(h) || h <= 0) h <- 600
      list(w = w, h = h)
    })
    
    # -- Build current plot safely --------------------------------
    build_current_plot <- reactive({
      mi <- info()
      d  <- df()
      
      # Selected aesthetics (validate existence)
      col_var <- if (!is.null(input$pca_color) && input$pca_color != "None" && input$pca_color %in% names(d)) input$pca_color else NULL
      shp_var <- if (!is.null(input$pca_shape) && input$pca_shape != "None" && input$pca_shape %in% names(d)) input$pca_shape else NULL
      lbl_var <- if (!is.null(input$pca_label) && input$pca_label != "None" && input$pca_label %in% names(d)) input$pca_label else NULL
      lbl_sz  <- if (!is.null(input$pca_label_size) && !is.na(input$pca_label_size)) input$pca_label_size else 2
      
      validate(need(!is.null(mi$model$x) && nrow(mi$model$x) >= 2, "PCA scores not available."))
      
      # Only one viz type for now
      validate(need(!is.null(input$plot_type) && input$plot_type == "biplot", "Unsupported plot type."))
      
      build_pca_biplot(
        pca_obj   = mi$model,
        data      = d,                # full filtered dataset so color/shape/label work
        color_var = col_var,
        shape_var = shp_var,
        label_var = lbl_var,
        label_size = lbl_sz
      )
    })
    
    # -- Render plot ----------------------------------------------
    output$plot <- renderPlot({
      build_current_plot()
    },
    width  = function() plot_size()$w,
    height = function() plot_size()$h,
    res    = 96)
    
    # -- Download -------------------------------------------------
    output$download_plot <- downloadHandler(
      filename = function() paste0("pca_biplot_", Sys.Date(), ".png"),
      content  = function(file) {
        g <- build_current_plot()
        s <- plot_size()
        ggsave(
          filename = file,
          plot = g,
          device = "png",
          dpi = 300,
          width  = s$w / 96,
          height = s$h / 96,
          units  = "in",
          limitsize = FALSE
        )
      }
    )
  })
}