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
      selectInput(
        ns("facet_var"),
        label = "Facet by variable:",
        choices = choices,
        selected = "None"
      ),
      uiOutput(ns("layout_controls")),
      hr(),
      numericInput(
        ns("pca_label_size"),
        label = "Label size:",
        value = 2,
        min = 0.5,
        max = 6,
        step = 0.5
      ),
      add_color_customization_ui(ns, multi_group = TRUE),
      checkboxInput(
        ns("show_loadings"),
        label = "Show loadings",
        value = FALSE
      ),
      numericInput(
        ns("loading_scale"),
        label = "Loading arrow scale",
        value = 1.2, min = 0.1, max = 5, step = 0.1
      ),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("plot_width"),
            label = "Plot width (px)",
            value = 800,
            min = 200,
            max = 2000,
            step = 50
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("plot_height"),
            label = "Plot height (px)",
            value = 600,
            min = 200,
            max = 2000,
            step = 50
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"))
    )
  )
}

visualize_pca_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # -- Reactives ------------------------------------------------------------
    model_info <- reactive({
      info <- model_fit()
      validate(need(!is.null(info) && identical(info$type, "pca"), "Run PCA first."))
      info
    })

    pca_entry <- reactive({
      info <- model_info()
      entry <- info$model
      validate(need(!is.null(entry), "PCA model missing."))
      entry
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

    available_choices <- reactive({
      data <- color_data()
      .pca_aesthetic_choices(data)
    })

    color_data <- reactive({
      entry <- pca_entry()
      if (!is.null(entry) && !is.null(entry$data)) {
        entry$data
      } else if (!is.null(filtered_data)) {
        if (is.reactive(filtered_data)) {
          filtered_data()
        } else {
          filtered_data
        }
      } else {
        NULL
      }
    })

    color_var_reactive <- reactive({
      var <- input$pca_color
      if (is.null(var) || identical(var, "None") || !nzchar(var)) {
        return(NULL)
      }

      data <- color_data()
      if (is.null(data) || !is.data.frame(data) || !var %in% names(data)) {
        return(NULL)
      }

      var
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = color_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )

    observeEvent(available_choices(), {
      choices <- available_choices()

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

    observeEvent(available_choices(), {
      facet_choices <- available_choices()

      selected <- if (!is.null(input$facet_var) && input$facet_var %in% facet_choices) {
        input$facet_var
      } else {
        "None"
      }

      updateSelectInput(session, "facet_var", choices = facet_choices, selected = selected)
    }, ignoreNULL = FALSE)

    output$layout_controls <- renderUI({
      facet_info <- facet_selection()
      if (is.null(facet_info$var) || length(facet_info$levels) <= 1) {
        return(NULL)
      }

      ns <- session$ns
      tagList(
        h4("Layout controls"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              ns("grid_rows"),
              "Grid rows",
              value = isolate(if (is.null(input$grid_rows)) NA else input$grid_rows),
              min = 1,
              max = 10,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              ns("grid_cols"),
              "Grid columns",
              value = isolate(if (is.null(input$grid_cols)) NA else input$grid_cols),
              min = 1,
              max = 10,
              step = 1
            )
          )
        )
      )
    })
    facet_selection <- reactive({
      data <- color_data()
      facet_var <- input$facet_var

      if (is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
        return(list(var = NULL, levels = NULL, column = NULL))
      }

      if (is.null(facet_var) || identical(facet_var, "None") || !nzchar(facet_var)) {
        return(list(var = NULL, levels = NULL, column = NULL))
      }

      if (!facet_var %in% names(data)) {
        return(list(var = NULL, levels = NULL, column = NULL))
      }

      column <- data[[facet_var]]
      if (is.null(column)) {
        return(list(var = NULL, levels = NULL, column = NULL))
      }

      if (is.factor(column)) {
        levels <- levels(stats::droplevels(column))
      } else {
        column_chr <- as.character(column)
        column_chr <- column_chr[!is.na(column_chr)]
        levels <- unique(column_chr)
      }

      levels <- levels[!is.na(levels)]

      list(var = facet_var, levels = levels, column = column)
    })

    build_message_panel <- function(title, message, show_title = TRUE) {
      base_plot <- ggplot() +
        theme_void() +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = message,
          size = 4,
          hjust = 0.5,
          vjust = 0.5
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off")

      if (isTRUE(show_title) && !is.null(title) && nzchar(title)) {
        base_plot +
          labs(title = title) +
          theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
      } else {
        base_plot +
          theme(plot.title = element_blank())
      }
    }

    sanitize_suffix <- function(value) {
      value <- value[1]
      safe <- gsub("[^A-Za-z0-9]+", "_", value)
      safe <- gsub("_+", "_", safe)
      safe <- gsub("^_|_$", "", safe)
      if (!nzchar(safe)) {
        "facet"
      } else {
        tolower(safe)
      }
    }

    plot_info <- reactive({
      req(input$plot_type)
      validate(need(input$plot_type == "biplot", "Unsupported plot type."))

      entry <- pca_entry()

      if (is.null(entry) || is.null(entry$model)) {
        message_text <- if (!is.null(entry$message) && nzchar(entry$message)) entry$message else "No PCA results available."
        message_plot <- build_message_panel(title = NULL, message = message_text, show_title = FALSE)
        defaults <- compute_default_grid(1L)
        layout <- basic_grid_layout(rows = 1, cols = 1, default_rows = 1, default_cols = 1)
        return(list(
          plot = message_plot,
          layout = layout,
          facet_levels = NULL,
          panels = 1L,
          warning = NULL,
          defaults = defaults,
          facet_var = NULL
        ))
      }

      if (is.null(entry$model$x) || nrow(entry$model$x) < 2) {
        message_plot <- build_message_panel(title = NULL, message = "PCA scores not available.", show_title = FALSE)
        defaults <- compute_default_grid(1L)
        layout <- basic_grid_layout(rows = 1, cols = 1, default_rows = 1, default_cols = 1)
        return(list(
          plot = message_plot,
          layout = layout,
          facet_levels = NULL,
          panels = 1L,
          warning = NULL,
          defaults = defaults,
          facet_var = NULL
        ))
      }

      data <- entry$data
      if (is.null(data) || nrow(data) == 0) {
        message_plot <- build_message_panel(title = NULL, message = "PCA data unavailable.", show_title = FALSE)
        defaults <- compute_default_grid(1L)
        layout <- basic_grid_layout(rows = 1, cols = 1, default_rows = 1, default_cols = 1)
        return(list(
          plot = message_plot,
          layout = layout,
          facet_levels = NULL,
          panels = 1L,
          warning = NULL,
          defaults = defaults,
          facet_var = NULL
        ))
      }

      choices <- available_choices()
      choice_pool <- unname(choices)
      color_var <- validate_choice(input$pca_color, choice_pool)
      shape_var <- validate_choice(input$pca_shape, choice_pool)
      label_var <- validate_choice(input$pca_label, choice_pool)
      label_size <- ifelse(is.null(input$pca_label_size) || is.na(input$pca_label_size), 2, input$pca_label_size)
      show_loadings <- isTRUE(input$show_loadings)
      loading_scale <- ifelse(is.null(input$loading_scale) || is.na(input$loading_scale), 1.2, input$loading_scale)

      facet_info <- facet_selection()
      facet_var <- facet_info$var
      facet_levels <- facet_info$levels
      facet_column <- facet_info$column

      subset_list <- list(All = seq_len(nrow(data)))
      if (!is.null(facet_var) && length(facet_levels) > 0) {
        subset_list <- list()
        facet_values <- if (is.factor(facet_column)) as.character(facet_column) else as.character(facet_column)
        for (level in facet_levels) {
          idx <- which(!is.na(facet_values) & facet_values == level)
          subset_list[[level]] <- idx
        }
      } else if (!is.null(facet_var)) {
        subset_list <- list(`No data` = integer())
      }

      color_levels <- NULL
      if (!is.null(color_var) && color_var %in% names(data)) {
        color_column <- data[[color_var]]
        if (is.factor(color_column)) {
          color_levels <- levels(stats::droplevels(color_column))
        } else {
          color_chr <- as.character(color_column)
          color_levels <- unique(color_chr[!is.na(color_chr)])
        }
      }

      scores <- as.data.frame(entry$model$x[, 1:2, drop = FALSE])
      names(scores)[1:2] <- c("PC1", "PC2")

      adjust_limits <- function(lims) {
        if (length(lims) != 2 || any(!is.finite(lims))) {
          return(c(-1, 1))
        }
        if (diff(range(lims)) == 0) {
          center <- lims[1]
          width <- if (abs(center) < 1) 1 else abs(center) * 0.1
          return(c(center - width, center + width))
        }
        lims
      }

      x_limits <- adjust_limits(range(scores$PC1, na.rm = TRUE))
      y_limits <- adjust_limits(range(scores$PC2, na.rm = TRUE))

      plot_list <- list()

      for (key in names(subset_list)) {
        idx <- subset_list[[key]]

        if (length(idx) == 0) {
          plot_list[[key]] <- build_message_panel(title = key, message = "No data available for this facet.", show_title = TRUE)
          next
        }

        local_color <- if (!is.null(color_var) && color_var %in% names(data)) color_var else NULL
        local_shape <- if (!is.null(shape_var) && shape_var %in% names(data)) shape_var else NULL
        local_label <- if (!is.null(label_var) && label_var %in% names(data)) label_var else NULL

        plot_obj <- build_pca_biplot(
          pca_obj = entry$model,
          data = data,
          color_var = local_color,
          shape_var = local_shape,
          label_var = local_label,
          label_size = label_size,
          show_loadings = show_loadings,
          loading_scale = loading_scale,
          custom_colors = custom_colors(),
          subset_rows = idx,
          color_levels = color_levels,
          x_limits = x_limits,
          y_limits = y_limits
        )

        if (!is.null(facet_var)) {
          plot_obj <- plot_obj +
            ggtitle(key) +
            theme(plot.title = element_text(size = 14, face = "bold"))
        }

        plot_list[[key]] <- plot_obj
      }

      plot_list <- Filter(Negate(is.null), plot_list)
      validate(need(length(plot_list) > 0, "No PCA plots available."))

      panel_count <- length(plot_list)
      defaults <- compute_default_grid(panel_count)

      use_custom_layout <- !is.null(facet_var) && panel_count > 1
      rows_input <- if (use_custom_layout) suppressWarnings(as.numeric(input$grid_rows)) else NA
      cols_input <- if (use_custom_layout) suppressWarnings(as.numeric(input$grid_cols)) else NA

      layout <- basic_grid_layout(
        rows = rows_input,
        cols = cols_input,
        default_rows = defaults$rows,
        default_cols = defaults$cols
      )

      validation <- validate_grid(panel_count, layout$nrow, layout$ncol)

      combined <- NULL
      if (isTRUE(validation$valid)) {
        combined <- patchwork::wrap_plots(
          plotlist = plot_list,
          nrow = layout$nrow,
          ncol = layout$ncol
        ) +
          patchwork::plot_layout(guides = "collect")
      }

      list(
        plot = combined,
        layout = layout,
        facet_levels = if (!is.null(facet_var)) names(plot_list) else NULL,
        panels = panel_count,
        warning = validation$message,
        defaults = defaults,
        facet_var = facet_var
      )
    })

    plot_size <- reactive({
      width <- suppressWarnings(as.numeric(input$plot_width))
      height <- suppressWarnings(as.numeric(input$plot_height))
      info <- plot_info()
      layout <- info$layout

      subplot_w <- ifelse(is.na(width) || width <= 0, 400, width)
      subplot_h <- ifelse(is.na(height) || height <= 0, 300, height)

      ncol <- if (!is.null(layout) && !is.null(layout$ncol)) max(1, layout$ncol) else 1
      nrow <- if (!is.null(layout) && !is.null(layout$nrow)) max(1, layout$nrow) else 1

      list(
        w = subplot_w * ncol,
        h = subplot_h * nrow
      )
    })

    observeEvent(plot_info(), {
      info <- plot_info()
      if (is.null(info) || is.null(info$defaults)) return()
      if (is.null(info$facet_var) || info$panels <= 1) return()

      rows <- info$defaults$rows
      cols <- info$defaults$cols
      if (is.null(rows) || is.null(cols)) return()

      sync_numeric_input(session, "grid_rows", input$grid_rows, rows)
      sync_numeric_input(session, "grid_cols", input$grid_cols, cols)
    }, ignoreNULL = FALSE)

    output$plot_warning <- renderUI({
      info <- plot_info()
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", info$warning)
      } else {
        NULL
      }
    })

    plot_obj <- reactive({
      info <- plot_info()
      if (!is.null(info$warning) || is.null(info$plot)) {
        return(NULL)
      }
      info$plot
    })

    output$plot <- renderPlot({
      info <- plot_info()
      if (!is.null(info$warning) || is.null(info$plot)) return(NULL)
      info$plot
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        info <- plot_info()
        facet_var <- info$facet_var
        suffix <- if (!is.null(facet_var)) {
          paste0("_facet_", sanitize_suffix(facet_var))
        } else {
          "_global"
        }
        paste0("pca_biplot", suffix, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- plot_info()
        req(is.null(info$warning))
        size <- plot_size()

        ggsave(
          filename = file,
          plot = info$plot,
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
                             show_loadings = FALSE, loading_scale = 1.2,
                             custom_colors = NULL, subset_rows = NULL,
                             color_levels = NULL, x_limits = NULL,
                             y_limits = NULL) {
  stopifnot(!is.null(pca_obj$x))

  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")

  if (!is.null(subset_rows)) {
    subset_rows <- unique(subset_rows)
    subset_rows <- subset_rows[subset_rows >= 1 & subset_rows <= nrow(scores)]
    scores <- scores[subset_rows, , drop = FALSE]
    if (!is.null(data)) {
      data <- data[subset_rows, , drop = FALSE]
    }
  }

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
  
  if (!is.null(color_var) && !is.null(plot_data[[color_var]])) {
    if (is.null(color_levels)) {
      color_levels <- if (is.factor(plot_data[[color_var]])) {
        levels(plot_data[[color_var]])
      } else {
        unique(as.character(plot_data[[color_var]]))
      }
    }
    color_levels <- unique(color_levels[!is.na(color_levels)])
    plot_data[[color_var]] <- factor(as.character(plot_data[[color_var]]), levels = color_levels)
  }
  
  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))
  
  single_color <- resolve_single_color(custom_colors)
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
    palette <- resolve_palette_for_levels(levels(plot_data[[color_var]]), custom = custom_colors)
    g <- g + scale_color_manual(values = palette)
  }
  
  if (!is.null(x_limits) || !is.null(y_limits)) {
    g <- g + coord_cartesian(xlim = x_limits, ylim = y_limits)
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
    rx <- diff(range(pca_obj$x[, 1], na.rm = TRUE))
    ry <- diff(range(pca_obj$x[, 2], na.rm = TRUE))
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
