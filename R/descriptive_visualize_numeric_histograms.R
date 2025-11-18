# ===============================================================
# 🟦 Descriptive Visualization — Numeric Histograms
# ===============================================================

visualize_numeric_histograms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("use_density"), "Show density instead of count", FALSE),
      "Switch between showing counts or densities for each histogram."
    ),
    fluidRow(
      column(6, with_help_tooltip(
        numericInput(ns("plot_width"),  "Subplot width (px)",  400, 200, 2000, 50),
        "Set the width of each histogram panel in pixels."
      )),
      column(6, with_help_tooltip(
        numericInput(ns("plot_height"), "Subplot height (px)", 300, 200, 2000, 50),
        "Set the height of each histogram panel in pixels."
      ))
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of histograms to display when several charts are shown.",
      cols_help = "Choose how many columns of histograms to display when several charts are shown."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, tagList(
        base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for histogram text elements."
        ),
        uiOutput(ns("common_legend_controls"))
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
      "Save the histograms as an image file."
    )
  )
}


visualize_numeric_histograms_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_numeric_histograms_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hash_key <- function(data, cols = NULL) {
      if (is.null(data) || !is.data.frame(data)) return("no-data")
      keep <- if (is.null(cols)) names(data) else intersect(cols, names(data))
      digest::digest(data[, keep, drop = FALSE], algo = "xxhash64")
    }
    
    active <- reactive({
      if (is.null(is_active)) TRUE else isTRUE(is_active())
    })
    
    subplot_sizes <- reactive({
      validate_subplot_dimensions(
        input$plot_width,
        input$plot_height,
        default_width = 400,
        default_height = 300
      )
    })
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    base_size <- base_size_server(input, default = 13)
    
    color_var <- reactive({
      info <- summary_info()
      dat <- filtered_data()
      g <- resolve_reactive(info$group_var)
      if (is.null(info) || is.null(dat) || !is.data.frame(dat)) return(NULL)
      if (is.null(g) || g == "" || g == "None" || !g %in% names(dat)) return(NULL)
      g
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, filtered_data, color_var, multi_group = TRUE
    )
    
    legend_state <- reactiveValues(
      enabled = FALSE,
      position = "bottom"
    )

    observeEvent(input$use_common_legend, {
      req(!is.null(input$use_common_legend))
      legend_state$enabled <- isTRUE(input$use_common_legend)
    }, ignoreNULL = TRUE)

    observeEvent(input$common_legend_position, {
      req(!is.null(input$common_legend_position))
      legend_state$position <- input$common_legend_position
    }, ignoreNULL = TRUE)

    state <- reactive({
      legend_allowed <- !is.null(color_var())
      valid_positions <- c("bottom", "top", "left", "right")
      resolved_position <- if (!is.null(legend_state$position) && legend_state$position %in% valid_positions) {
        legend_state$position
      } else {
        "bottom"
      }
      use_common_legend <- legend_allowed && isTRUE(legend_state$enabled)
      list(
        info = summary_info(),
        dat = filtered_data(),
        use_density = isTRUE(input$use_density),
        colors = custom_colors(),
        base_size = base_size(),
        rows = grid$rows(),
        cols = grid$cols(),
        common_legend = use_common_legend,
        legend_position = if (use_common_legend) resolved_position else NULL
      )
    })
    
    plot_info <- reactive({
      req(active())
      s <- state()
      info <- s$info
      req(!is.null(info))
      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else s$dat
      req(!is.null(dat), is.data.frame(dat), nrow(dat) > 0)
      build_descriptive_numeric_histogram(
        df = dat,
        selected_vars = resolve_reactive(info$selected_vars),
        group_var = resolve_reactive(info$group_var),
        strata_levels = resolve_reactive(info$strata_levels),
        use_density = s$use_density,
        nrow_input = s$rows,
        ncol_input = s$cols,
        custom_colors = s$colors,
        base_size = s$base_size,
        common_legend = s$common_legend,
        legend_position = s$legend_position
      )
    })

    observeEvent(plot_info(), {
      req(active())
      info <- plot_info()
      apply_grid_defaults_if_empty(input, session, "plot_grid", info$defaults)
    }, ignoreNULL = TRUE)

    common_legend_available <- reactive({
      req(active())
      info <- plot_info()
      has_group <- !is.null(color_var())
      n_panels <- info$panels %||% 0L
      has_group && n_panels > 1L
    })

    observeEvent(common_legend_available(), {
      if (!isTRUE(common_legend_available())) {
        legend_state$enabled <- FALSE
      }
    })

    output$common_legend_controls <- renderUI({
      legend_supported <- TRUE
      if (!isTRUE(common_legend_available()) || !legend_supported) {
        return(NULL)
      }

      checkbox <- div(
        class = "mt-3",
        with_help_tooltip(
          checkboxInput(
            ns("use_common_legend"),
            "Use common legend",
            value = isTRUE(legend_state$enabled)
          ),
          "Merge the legends across panels into a single shared legend."
        )
      )

      legend_position <- if (isTRUE(legend_state$enabled)) {
        div(
          class = "mt-2",
          with_help_tooltip(
            selectInput(
              ns("common_legend_position"),
              "Legend position",
              choices = c(
                "Bottom" = "bottom",
                "Right" = "right",
                "Top" = "top",
                "Left" = "left"
              ),
              selected = legend_state$position %||% "bottom"
            ),
            "Choose where the combined legend should be displayed."
          )
        )
      } else {
        NULL
      }

      tagList(checkbox, legend_position)
    })
    
    plot_dimensions <- reactive({
      req(active())
      lay <- plot_info()$layout
      nrow_l <- if (!is.null(lay$nrow)) as.integer(lay$nrow) else 1L
      ncol_l <- if (!is.null(lay$ncol)) as.integer(lay$ncol) else 1L
      list(
        width = max(200, subplot_sizes()$width  * ncol_l),
        height = max(200, subplot_sizes()$height * nrow_l),
        warning = subplot_sizes()$warning
      )
    })
    
    output$grid_warning <- renderUI({
      req(active())
      info <- plot_info()
      size_warning <- plot_dimensions()$warning
      warnings <- c(info$warning, size_warning)
      warnings <- warnings[!vapply(warnings, is.null, logical(1))]
      if (length(warnings) > 0)
        div(class = "alert alert-warning", HTML(paste(warnings, collapse = "<br>")))
    })
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_histograms_", Sys.Date(), ".png"),
      content = function(file) {
        req(active())
        info <- plot_info()
        req(is.null(info$warning), !is.null(info$plot))
        s <- plot_dimensions()
        ggplot2::ggsave(
          file, info$plot, device = "png", dpi = 300,
          width = s$width / 96, height = s$height / 96,
          units = "in", limitsize = FALSE
        )
      }
    )
    
    cached_plot <- reactiveVal(NULL)
    cached_key  <- reactiveVal(NULL)
    
    observe({
      req(active())
      s <- state()
      dat <- s$dat
      cols <- resolve_reactive(s$info$selected_vars)
      key <- paste(
        hash_key(dat, cols),
        resolve_reactive(s$info$group_var),
        resolve_reactive(s$info$strata_levels),
        s$use_density,
        s$colors,
        s$base_size,
        s$rows,
        s$cols,
        s$common_legend,
        s$legend_position %||% "",
        sep = "_"
      )
      
      if (!identical(key, cached_key())) {
        info <- isolate(plot_info())

        if (!is.null(info$warning)) {
          cached_plot(NULL)
          cached_key(key)
          return()
        }

        if (!is.null(info$plot)) {
          cached_plot(info$plot)
          cached_key(key)
        }
      }
    })
    
    output$plot <- renderPlot({
      req(active())
      p <- cached_plot()
      validate(need(!is.null(p), "Plot not ready"))
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    outputOptions(output, "plot", suspendWhenHidden = TRUE)
  })
}




build_descriptive_numeric_histogram <- function(df,
                                                selected_vars = NULL,
                                                group_var = NULL,
                                                strata_levels = NULL,
                                                use_density = FALSE,
                                                nrow_input = NULL,
                                                ncol_input = NULL,
                                                custom_colors = NULL,
                                                base_size = 13,
                                                common_legend = FALSE,
                                                legend_position = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(Filter(is.numeric, df))
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)

  if (is.null(group_var) || !group_var %in% names(df)) {
    group_var <- NULL
  } else {
    df[[group_var]] <- as.character(df[[group_var]])
    df[[group_var]][is.na(df[[group_var]]) | trimws(df[[group_var]]) == ""] <- "Missing"

    if (!is.null(strata_levels) && length(strata_levels) > 0) {
      keep_levels <- unique(strata_levels)
      df <- df[df[[group_var]] %in% keep_levels, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      df[[group_var]] <- factor(df[[group_var]], levels = keep_levels)
    } else {
      df[[group_var]] <- factor(df[[group_var]], levels = unique(df[[group_var]]))
    }
  }

  plots <- lapply(num_vars, function(var) {
    cols <- intersect(c(var, group_var), names(df))
    plot_data <- df[, cols, drop = FALSE]

    keep <- is.finite(plot_data[[var]])
    keep[is.na(keep)] <- FALSE
    plot_data <- plot_data[keep, , drop = FALSE]
    if (nrow(plot_data) == 0) return(NULL)

    if (!is.null(group_var)) {
      plot_data[[group_var]] <- droplevels(plot_data[[group_var]])
    }

    density_mode <- isTRUE(use_density) && length(unique(plot_data[[var]])) > 1
    base <- ggplot(plot_data, aes(x = .data[[var]]))
    y_label <- if (density_mode) "Density" else "Count"

    if (!is.null(group_var)) {
      group_levels <- levels(plot_data[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      if (density_mode) {
        base <- base +
          geom_density(aes(color = .data[[group_var]], fill = .data[[group_var]]), alpha = 0.3) +
          scale_color_manual(values = palette) +
          scale_fill_manual(values = palette) +
          labs(color = group_var, fill = group_var)
      } else {
        base <- base +
          geom_histogram(
            aes(fill = .data[[group_var]]),
            position = "identity",
            alpha = 0.5,
            bins = 30
          ) +
          scale_fill_manual(values = palette) +
          labs(fill = group_var)
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      if (density_mode) {
        base <- base + geom_density(fill = single_color, color = single_color, alpha = 0.35)
      } else {
        base <- base + geom_histogram(fill = single_color, color = single_color, bins = 30)
      }
    }

    base +
      theme_minimal(base_size = base_size) +
      labs(title = var, x = var, y = y_label)
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  n_panels <- length(plots)
  defaults <- compute_default_grid(n_panels)
  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols
  )
  layout <- adjust_grid_layout(n_panels, layout)

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)
  combined <- if (isTRUE(validation$valid)) {
    patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol) +
      patchwork::plot_annotation(
        theme = theme(plot.title = element_text(size = 16, face = "bold"))
      )
  } else {
    NULL
  }

  if (!is.null(combined)) {
    combined <- apply_common_legend_layout(
      combined,
      legend_position = legend_position,
      collect_guides = isTRUE(common_legend)
    )
  }

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = n_panels,
    warning = validation$message,
    defaults = defaults
  )
}
