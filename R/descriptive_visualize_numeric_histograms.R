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
      column(6, base_size_ui(
        ns,
        default = 13,
        help_text = "Adjust the base font size used for histogram text elements."
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

    resolve_input_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x)) x() else x
    }

    module_active <- reactive({
      if (is.null(is_active)) {
        TRUE
      } else {
        isTRUE(is_active())
      }
    })

    normalize_dimension <- function(value, default) {
      if (is.null(value) || !is.numeric(value) || is.na(value)) default else value
    }

    plot_width  <- reactive(normalize_dimension(input$plot_width, 400))
    plot_height <- reactive(normalize_dimension(input$plot_height, 300))

    color_var_reactive <- reactive({
      info <- summary_info()
      if (is.null(info)) return(NULL)

      group_var <- resolve_input_value(info$group_var)
      if (is.null(group_var) || identical(group_var, "") || identical(group_var, "None")) {
        return(NULL)
      }

      dat <- filtered_data()
      if (is.null(dat) || !is.data.frame(dat) || !group_var %in% names(dat)) {
        return(NULL)
      }

      group_var
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = filtered_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )

    base_size <- base_size_server(
      input = input,
      default = 13
    )

    cached_plot_info <- reactiveVal(NULL)
    invalidate_cache <- function() cached_plot_info(NULL)

    observeEvent(
      list(
        summary_info(),
        filtered_data(),
        input$use_density,
        custom_colors(),
        base_size()
      ),
      {
        invalidate_cache()
      },
      ignoreNULL = FALSE
    )

    grid_inputs <- plot_grid_server("plot_grid")

    observeEvent(grid_inputs$values(), {
      invalidate_cache()
    })

    compute_plot_info <- function() {
      info <- summary_info()
      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_input_value(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()
      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_input_value(info$selected_vars)
      group_var     <- resolve_input_value(info$group_var)
      strata_levels <- resolve_input_value(info$strata_levels)

      out <- build_descriptive_numeric_histogram(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        strata_levels = strata_levels,
        use_density = isTRUE(input$use_density),
        nrow_input = grid_inputs$rows(),
        ncol_input = grid_inputs$cols(),
        custom_colors = custom_colors(),
        base_size = base_size()
      )
      validate(need(!is.null(out), "No numeric variables available for plotting."))
      out
    }

    plot_info <- reactive({
      req(module_active())
      info <- cached_plot_info()
      if (is.null(info)) {
        info <- compute_plot_info()
        cached_plot_info(info)
      }
      info
    })


    plot_size <- reactive({
      req(module_active())
      info <- plot_info()
      if (is.null(info$layout)) {
        list(w = plot_width(), h = plot_height())
      } else {
        list(
          w = plot_width()  * info$layout$ncol,
          h = plot_height() * info$layout$nrow
        )
      }
    })

    output$grid_warning <- renderUI({
      req(module_active())
      info <- plot_info()
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", info$warning)
      } else {
        NULL
      }
    })

    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_histograms_", Sys.Date(), ".png"),
      content  = function(file) {
        req(module_active())
        info <- plot_info()
        req(is.null(info$warning))
        req(info$plot)
        s <- plot_size()
        ggplot2::ggsave(
          filename = file,
          plot = info$plot,
          device = "png",
          dpi = 300,
          width  = s$w / 96,
          height = s$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )

    output$plot <- renderPlot({
      req(module_active())
      info <- plot_info()
      if (!is.null(info$warning) || is.null(info$plot)) return(NULL)
      print(info$plot)
    },
    width = function() {
      req(module_active())
      plot_size()$w
    },
    height = function() {
      req(module_active())
      plot_size()$h
    },
    res = 96)
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
                                                base_size = 13) {
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

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = n_panels,
    warning = validation$message,
    defaults = defaults
  )
}
