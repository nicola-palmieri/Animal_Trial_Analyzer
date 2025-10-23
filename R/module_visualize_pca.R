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
    
    df <- reactive(filtered_data())
    model_info <- reactive(model_fit())
    
    # --- Dynamically populate aesthetics dropdowns ---
    observeEvent(df(), {
      data <- df()
      req(data)
      
      # only continue if it's actually a data.frame
      if (!is.data.frame(data)) return()
      
      cols <- names(data)
      
      # make sure we have at least one column
      if (length(cols) == 0) {
        updateSelectInput(session, "pca_color", choices = "None", selected = "None")
        updateSelectInput(session, "pca_shape", choices = "None", selected = "None")
        updateSelectInput(session, "pca_label", choices = "None", selected = "None")
        return()
      }
      
      # ✅ populate all three aesthetic controls
      updateSelectInput(session, "pca_color", choices = c("None", cols))
      updateSelectInput(session, "pca_shape", choices = c("None", cols))
      updateSelectInput(session, "pca_label", choices = c("None", cols))
    })
    
    # --- Reactive plot size ---
    plot_size <- reactive({
      list(w = input$plot_width, h = input$plot_height)
    })
    
    # --- PCA plot ---
    output$plot <- renderPlot({
      info <- model_info()
      if (is.null(info) || info$type != "pca") return(NULL)
      validate(need(!is.null(info$model), "Run PCA first."))
      validate(need(!is.null(df()), "No dataset available for PCA visualization."))
      
      pca_obj <- info$model
      data <- df()
      
      color_var <- if (input$pca_color != "None") input$pca_color else NULL
      shape_var <- if (input$pca_shape != "None") input$pca_shape else NULL
      label_var <- if (input$pca_label != "None") input$pca_label else NULL
      label_size <- if (!is.na(input$pca_label_size)) input$pca_label_size else 2
      
      # ✅ validate selected variables exist
      if (!is.null(color_var) && !(color_var %in% names(data))) color_var <- NULL
      if (!is.null(shape_var) && !(shape_var %in% names(data))) shape_var <- NULL
      if (!is.null(label_var) && !(label_var %in% names(data))) label_var <- NULL
      
      if (input$plot_type == "biplot") {
        build_pca_biplot(
          pca_obj,
          data,
          color_var = color_var,
          shape_var = shape_var,
          label_var = label_var,
          label_size = label_size
        )
      }
    },
    width  = function() plot_size()$w,
    height = function() plot_size()$h,
    res    = 96)
    
    # --- Download handler ---
    output$download_plot <- downloadHandler(
      filename = function() paste0("pca_biplot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- model_info()
        req(info)
        validate(need(!is.null(info$model), "Run PCA first."))
        
        pca_obj <- info$model
        data <- df()
        
        color_var <- if (input$pca_color != "None") input$pca_color else NULL
        shape_var <- if (input$pca_shape != "None") input$pca_shape else NULL
        label_var <- if (input$pca_label != "None") input$pca_label else NULL
        label_size <- if (!is.na(input$pca_label_size)) input$pca_label_size else 2
        
        # ✅ validate again
        if (!is.null(color_var) && !(color_var %in% names(data))) color_var <- NULL
        if (!is.null(shape_var) && !(shape_var %in% names(data))) shape_var <- NULL
        if (!is.null(label_var) && !(label_var %in% names(data))) label_var <- NULL
        
        g <- build_pca_biplot(
          pca_obj,
          data,
          color_var = color_var,
          shape_var = shape_var,
          label_var = label_var,
          label_size = label_size
        )
        
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
