# ===============================================================
# Visualization Module - PCA (Biplot)
# ===============================================================

# Helper to detect categorical columns ----------------------------------------
.is_categorical <- function(x) {
  is.factor(x) || is.character(x) || is.logical(x)
}

.pca_aesthetic_choices <- function(data) {
  if (missing(data) || is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
    return(c("None" = "None"))
  }

  keep <- vapply(data, .is_categorical, logical(1))
  cat_cols <- names(data)[keep]

  if (length(cat_cols) == 0) {
    return(c("None" = "None"))
  }

  c("None" = "None", stats::setNames(cat_cols, cat_cols))
}

visualize_pca_ui <- function(id, filtered_data = NULL) {
  ns <- NS(id)
  choices <- .pca_aesthetic_choices(filtered_data)

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize principal component analysis (PCA)"),
      p("Visualize multivariate structure using a PCA biplot."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Select visualization type:",
        choices = c("PCA biplot" = "biplot"),
        selected = "biplot"
      ),
      hr(),
      selectInput(
        ns("pca_color"),
        label = "Color points by:",
        choices = choices,
        selected = "None"
      ),
      selectInput(
        ns("pca_shape"),
        label = "Shape points by:",
        choices = choices,
        selected = "None"
      ),
      selectInput(
        ns("pca_label"),
        label = "Label points by:",
        choices = choices,
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
      downloadButton(ns("download_plot"), "Download plot")
    ),
    mainPanel(
      width = 8,
      h4("PCA biplot"),
      plotOutput(ns("plot"))
    )
  )
}

visualize_pca_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    # -- Reactives ------------------------------------------------------------
    df <- reactive({
      data <- filtered_data()
      validate(need(!is.null(data) && is.data.frame(data), "No data available."))
      data
    })

    model_info <- reactive({
      info <- model_fit()
      validate(need(!is.null(info) && identical(info$type, "pca"), "Run PCA first."))
      validate(need(!is.null(info$model), "PCA model missing."))
      info
    })

    categorical_vars <- reactive({
      data <- df()
      keep <- vapply(data, .is_categorical, logical(1))
      names(data)[keep]
    })

    validate_choice <- function(value, pool) {
      if (is.null(value) || identical(value, "None") || !nzchar(value)) {
        return(NULL)
      }
      if (length(pool) == 0 || !(value %in% pool)) {
        return(NULL)
      }
      value
    }

    plot_size <- reactive({
      width <- suppressWarnings(as.numeric(input$plot_width))
      height <- suppressWarnings(as.numeric(input$plot_height))

      list(
        w = ifelse(is.na(width) || width <= 0, 800, width),
        h = ifelse(is.na(height) || height <= 0, 600, height)
      )
    })

    build_current_plot <- reactive({
      info <- model_info()
      data <- df()
      categorical <- categorical_vars()

      color_var <- validate_choice(input$pca_color, categorical)
      shape_var <- validate_choice(input$pca_shape, categorical)
      label_var <- validate_choice(input$pca_label, categorical)
      label_size <- ifelse(is.null(input$pca_label_size) || is.na(input$pca_label_size), 2, input$pca_label_size)

      validate(need(!is.null(info$model$x) && nrow(info$model$x) >= 2, "PCA scores not available."))
      validate(need(!is.null(input$plot_type) && input$plot_type == "biplot", "Unsupported plot type."))

      build_pca_biplot(
        pca_obj = info$model,
        data = data,
        color_var = color_var,
        shape_var = shape_var,
        label_var = label_var,
        label_size = label_size
      )
    })

    output$plot <- renderPlot({
      build_current_plot()
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() paste0("pca_biplot_", Sys.Date(), ".png"),
      content = function(file) {
        plot_obj <- build_current_plot()
        size <- plot_size()

        ggsave(
          filename = file,
          plot = plot_obj,
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
