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
      uiOutput(ns("layout_controls")),
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
            label = "Subplot width (px)",
            value = 400,
            min = 200,
            max = 2000,
            step = 50
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("plot_height"),
            label = "Subplot height (px)",
            value = 300,
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
      plotOutput(ns("plot"))
    )
  )
}

visualize_pca_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    layout_state <- initialize_layout_state(input, session)
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

    validate_choice <- function(value, pool) {
      if (is.null(value) || identical(value, "None") || !nzchar(value)) {
        return(NULL)
      }
      if (length(pool) == 0 || !(value %in% pool)) {
        return(NULL)
      }
      value
    }

    pick_reference_entry <- function(entries) {
      if (is.null(entries) || length(entries) == 0) {
        return(NULL)
      }

      valid <- entries[vapply(entries, function(x) !is.null(x) && !is.null(x$data) && nrow(x$data) > 0, logical(1))]
      if (length(valid) > 0) {
        return(valid[[1]])
      }

      entries[[1]]
    }

    available_choices <- reactive({
      entries <- pca_entries()
      entry <- pick_reference_entry(entries)
      data <- if (!is.null(entry)) entry$data else NULL
      .pca_aesthetic_choices(data)
    })

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

    output$layout_controls <- renderUI({
      entries <- pca_entries()
      if (length(entries) <= 1) {
        return(NULL)
      }

      ns <- session$ns
      tagList(
        h4("Layout controls"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              ns("strata_rows"),
              "Grid rows",
              value = isolate(layout_state$default_ui_value(input$strata_rows)),
              min = 1,
              max = 10,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              ns("strata_cols"),
              "Grid columns",
              value = isolate(layout_state$default_ui_value(input$strata_cols)),
              min = 1,
              max = 10,
              step = 1
            )
          )
        )
      )
    })

    build_message_panel <- function(title, message) {
      ggplot() +
        theme_void() +
        labs(title = title) +
        annotate(
          "text",
          x = 0.5,
          y = 0.5,
          label = message,
          size = 4,
          hjust = 0.5,
          vjust = 0.5
        ) +
        coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
    }

    sanitize_suffix <- function(value) {
      value <- value[1]
      safe <- gsub("[^A-Za-z0-9]+", "_", value)
      safe <- gsub("_+", "_", safe)
      safe <- gsub("^_|_$", "", safe)
      if (!nzchar(safe)) {
        "stratum"
      } else {
        tolower(safe)
      }
    }

    plot_info <- reactive({
      req(input$plot_type)
      validate(need(input$plot_type == "biplot", "Unsupported plot type."))
      entries <- pca_entries()
      validate(need(length(entries) > 0, "No PCA results available."))

      choices <- available_choices()
      color_var <- validate_choice(input$pca_color, choices)
      shape_var <- validate_choice(input$pca_shape, choices)
      label_var <- validate_choice(input$pca_label, choices)
      label_size <- ifelse(is.null(input$pca_label_size) || is.na(input$pca_label_size), 2, input$pca_label_size)
      show_loadings <- isTRUE(input$show_loadings)
      loading_scale <- ifelse(is.null(input$loading_scale) || is.na(input$loading_scale), 1.2, input$loading_scale)

      plot_list <- list()
      strata_names <- names(entries)
      if (is.null(strata_names) || length(strata_names) == 0) {
        strata_names <- paste0("Stratum ", seq_along(entries))
      }

      for (i in seq_along(entries)) {
        entry <- entries[[i]]
        title <- strata_names[[i]]
        if (!nzchar(title)) {
          title <- paste0("Stratum ", i)
        }

        if (is.null(entry)) {
          plot_list[[title]] <- build_message_panel(title, "No PCA results available.")
          next
        }

        if (!is.null(entry$message) && nzchar(entry$message)) {
          plot_list[[title]] <- build_message_panel(title, entry$message)
          next
        }

        if (is.null(entry$model) || is.null(entry$model$x) || nrow(entry$model$x) < 2) {
          plot_list[[title]] <- build_message_panel(title, "PCA scores not available.")
          next
        }

        data <- entry$data
        local_color <- if (!is.null(color_var) && !is.null(data) && color_var %in% names(data)) color_var else NULL
        local_shape <- if (!is.null(shape_var) && !is.null(data) && shape_var %in% names(data)) shape_var else NULL
        local_label <- if (!is.null(label_var) && !is.null(data) && label_var %in% names(data)) label_var else NULL

        plot_obj <- build_pca_biplot(
          pca_obj = entry$model,
          data = data,
          color_var = local_color,
          shape_var = local_shape,
          label_var = local_label,
          label_size = label_size,
          show_loadings = show_loadings,
          loading_scale = loading_scale
        ) +
          ggtitle(title) +
          theme(plot.title = element_text(size = 14, face = "bold"))

        plot_list[[title]] <- plot_obj
      }

      plot_list <- Filter(Negate(is.null), plot_list)
      validate(need(length(plot_list) > 0, "No PCA plots available."))

      layout <- resolve_grid_layout(
        n_items = length(plot_list),
        rows_input = layout_state$effective_input("strata_rows"),
        cols_input = layout_state$effective_input("strata_cols")
      )

      combined <- patchwork::wrap_plots(
        plotlist = plot_list,
        nrow = layout$nrow,
        ncol = layout$ncol
      ) +
        patchwork::plot_layout(guides = "collect")

      list(
        plot = combined,
        layout = layout,
        strata_names = names(plot_list)
      )
    })

    observe_layout_synchronization(plot_info, layout_state, session)

    plot_size <- reactive({
      width <- suppressWarnings(as.numeric(input$plot_width))
      height <- suppressWarnings(as.numeric(input$plot_height))
      info <- plot_info()
      layout <- info$layout

      subplot_w <- ifelse(is.na(width) || width <= 0, 400, width)
      subplot_h <- ifelse(is.na(height) || height <= 0, 300, height)

      list(
        w = subplot_w * max(1, layout$ncol),
        h = subplot_h * max(1, layout$nrow)
      )
    })

    plot_obj <- reactive({
      info <- plot_info()
      validate(need(!is.null(info$plot), "No PCA plots available."))
      info$plot
    })

    output$plot <- renderPlot({
      req(plot_obj())
      plot_obj()
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        info <- plot_info()
        strata <- info$strata_names
        suffix <- if (length(strata) == 1) {
          paste0("_", sanitize_suffix(strata))
        } else {
          "_all_strata"
        }
        paste0("pca_biplot", suffix, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- plot_info()
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
