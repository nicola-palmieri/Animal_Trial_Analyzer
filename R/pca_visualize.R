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
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
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


build_pca_biplot <- function(pca_obj, data, color_var = NULL, shape_var = NULL,
                             label_var = NULL, label_size = 2) {
  stopifnot(!is.null(pca_obj$x))
  
  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")
  
  if (!is.null(data) && nrow(data) == nrow(scores)) {
    plot_data <- cbind(scores, data)
  } else {
    plot_data <- scores
  }
  
  if (!is.null(label_var) && !identical(label_var, "") && !is.null(plot_data[[label_var]])) {
    label_values <- as.character(plot_data[[label_var]])
    label_values[is.na(label_values) | trimws(label_values) == ""] <- NA_character_
    
    if (any(!is.na(label_values))) {
      plot_data$label_value <- label_values
    } else {
      label_var <- NULL
    }
  } else {
    label_var <- NULL
  }
  
  color_levels <- NULL
  if (!is.null(color_var) && !is.null(plot_data[[color_var]])) {
    color_levels <- if (is.factor(plot_data[[color_var]])) {
      levels(plot_data[[color_var]])
    } else {
      unique(as.character(plot_data[[color_var]]))
    }
    color_levels <- color_levels[!is.na(color_levels)]
    plot_data[[color_var]] <- factor(as.character(plot_data[[color_var]]), levels = color_levels)
  }
  
  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))
  
  single_color <- resolve_single_color()
  g <- ggplot(plot_data, aes_mapping) +
    geom_point(
      size = 3,
      shape = if (is.null(shape_var)) 16 else NULL,
      color = if (is.null(color_var)) single_color else NULL
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = "PC1",
      y = "PC2",
      color = if (!is.null(color_var)) color_var else NULL,
      shape = if (!is.null(shape_var)) shape_var else NULL
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "right"
    )
  
  if (!is.null(color_var)) {
    palette <- resolve_palette_for_levels(color_levels)
    g <- g + scale_color_manual(values = palette)
  }
  
  if (!is.null(label_var)) {
    g <- g + ggrepel::geom_text_repel(
      aes(label = label_value),
      color = if (is.null(color_var)) single_color else NULL,
      size = label_size,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2,
      na.rm = TRUE
    )
  }
  
  g
}