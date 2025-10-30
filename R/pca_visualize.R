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
      uiOutput(ns("strata_selector")),
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
      checkboxInput(
        ns("show_loadings"),
        label = "Show loadings (arrows)",
        value = FALSE
      ),
      numericInput(
        ns("loading_scale"),
        label = "Loading arrow scale",
        value = 1.2, min = 0.1, max = 5, step = 0.1
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
    model_info <- reactive({
      info <- model_fit()
      validate(need(!is.null(info) && identical(info$type, "pca"), "Run PCA first."))
      info
    })

    pca_entries <- reactive({
      info <- model_info()
      entries <- info$model
      validate(need(!is.null(entries) && length(entries) > 0, "PCA model missing."))
      entries
    })

    resolve_entry <- function(entries, stratum = NULL) {
      if (is.null(entries) || length(entries) == 0) {
        return(NULL)
      }

      if (!is.null(stratum) && stratum %in% names(entries)) {
        return(entries[[stratum]])
      }

      valid <- entries[vapply(entries, function(x) !is.null(x) && !is.null(x$model), logical(1))]
      if (length(valid) > 0) {
        return(valid[[1]])
      }

      entries[[1]]
    }

    resolve_stratum_name <- function(entries, stratum = NULL) {
      if (is.null(entries) || length(entries) == 0) {
        return(NULL)
      }

      available <- names(entries)
      if (length(available) == 0) {
        return(NULL)
      }

      if (!is.null(stratum) && stratum %in% available) {
        return(stratum)
      }

      valid <- available[vapply(entries, function(x) !is.null(x) && !is.null(x$model), logical(1))]
      if (length(valid) > 0) {
        return(valid[[1]])
      }

      available[[1]]
    }

    output$strata_selector <- renderUI({
      entries <- pca_entries()
      if (length(entries) <= 1) {
        return(NULL)
      }

      choices <- names(entries)
      if (length(choices) == 0) {
        return(NULL)
      }

      default <- resolve_stratum_name(entries, input$active_stratum)
      selectInput(
        session$ns("active_stratum"),
        "Select stratum:",
        choices = choices,
        selected = default
      )
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

    observeEvent(list(pca_entries(), input$active_stratum), {
      entries <- pca_entries()
      stratum <- resolve_stratum_name(entries, input$active_stratum)
      entry <- resolve_entry(entries, stratum)
      data <- if (!is.null(entry)) entry$data else NULL
      choices <- .pca_aesthetic_choices(data)

      select_valid <- function(current) {
        if (!is.null(current) && current %in% choices) {
          current
        } else {
          "None"
        }
      }

      updateSelectInput(session, "pca_color", choices = choices, selected = select_valid(input$pca_color))
      updateSelectInput(session, "pca_shape", choices = choices, selected = select_valid(input$pca_shape))
      updateSelectInput(session, "pca_label", choices = choices, selected = select_valid(input$pca_label))
    }, ignoreNULL = FALSE)

    build_current_plot <- reactive({
      entries <- pca_entries()
      stratum <- resolve_stratum_name(entries, input$active_stratum)
      entry <- resolve_entry(entries, stratum)

      validate(need(!is.null(entry), "No PCA results available."))
      if (!is.null(entry$message) && nzchar(entry$message)) {
        validate(need(FALSE, entry$message))
      }

      data <- entry$data
      choices <- .pca_aesthetic_choices(data)

      color_var <- validate_choice(input$pca_color, choices)
      shape_var <- validate_choice(input$pca_shape, choices)
      label_var <- validate_choice(input$pca_label, choices)
      label_size <- ifelse(is.null(input$pca_label_size) || is.na(input$pca_label_size), 2, input$pca_label_size)

      validate(need(!is.null(entry$model$x) && nrow(entry$model$x) >= 2, "PCA scores not available."))
      validate(need(!is.null(input$plot_type) && input$plot_type == "biplot", "Unsupported plot type."))

      build_pca_biplot(
        pca_obj   = entry$model,
        data      = data,
        color_var = color_var,
        shape_var = shape_var,
        label_var = label_var,
        label_size = label_size,
        show_loadings = isTRUE(input$show_loadings),
        loading_scale = ifelse(is.null(input$loading_scale) || is.na(input$loading_scale), 1.2, input$loading_scale)
      )
    })

    output$plot <- renderPlot({
      build_current_plot()
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        entries <- pca_entries()
        stratum <- resolve_stratum_name(entries, input$active_stratum)
        suffix <- if (!is.null(stratum) && nzchar(stratum)) {
          paste0("_", gsub("[^A-Za-z0-9]+", "_", stratum))
        } else {
          ""
        }
        paste0("pca_biplot", suffix, "_", Sys.Date(), ".png")
      },
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
                             label_var = NULL, label_size = 2,
                             show_loadings = FALSE, loading_scale = 1.2) {
  stopifnot(!is.null(pca_obj$x))
  
  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")
  
  var_exp <- 100 * (pca_obj$sdev^2 / sum(pca_obj$sdev^2))
  x_lab <- sprintf("PC1 (%.1f%%)", var_exp[1])
  y_lab <- sprintf("PC2 (%.1f%%)", var_exp[2])
  
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
    color_levels <- if (is.factor(plot_data[[color_var]])) levels(plot_data[[color_var]]) else unique(as.character(plot_data[[color_var]]))
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
      x = x_lab,
      y = y_lab,
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
  
  # ---- Loadings as arrows (optional) ----
  if (isTRUE(show_loadings) && !is.null(pca_obj$rotation)) {
    R <- as.data.frame(pca_obj$rotation[, 1:2, drop = FALSE])
    R$variable <- rownames(pca_obj$rotation)
    
    # scale arrows to score space
    rx <- diff(range(scores$PC1, na.rm = TRUE))
    ry <- diff(range(scores$PC2, na.rm = TRUE))
    sx <- ifelse(is.finite(rx) && rx > 0, rx, 1)
    sy <- ifelse(is.finite(ry) && ry > 0, ry, 1)
    
    arrows_df <- transform(
      R,
      x = 0, y = 0,
      xend = PC1 * sx * loading_scale,
      yend = PC2 * sy * loading_scale
    )
    
    g <- g +
      geom_segment(
        data = arrows_df,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.02, "npc")),
        linewidth = 0.4,
        color = "grey30"
      ) +
      ggrepel::geom_text_repel(
        data = arrows_df,
        aes(x = xend, y = yend, label = variable),
        inherit.aes = FALSE,
        size = 3,
        color = "grey20",
        max.overlaps = Inf,
        segment.size = 0.2,
        box.padding = 0.2,
        point.padding = 0.2
      )
  }
  
  g
}
