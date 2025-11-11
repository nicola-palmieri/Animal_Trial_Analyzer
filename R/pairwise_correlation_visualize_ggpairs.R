# ===============================================================
# 🧪 Pairwise Correlation — GGPairs Visualization Module
# ===============================================================

pairwise_correlation_visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, with_help_tooltip(
        numericInput(ns("plot_width"),  "Subplot width (px)",  800, 200, 2000, 50),
        "Set the width in pixels for each panel of the correlation matrix."
      )),
      column(6, with_help_tooltip(
        numericInput(ns("plot_height"), "Subplot height (px)", 600, 200, 2000, 50),
        "Set the height in pixels for each panel of the correlation matrix."
      ))
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of panels to use when multiple strata are plotted.",
      cols_help = "Choose how many columns of panels to use when multiple strata are plotted."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, base_size_ui(
        ns,
        default = 11,
        help_text = "Adjust the base font size used for the correlation plot."
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download Plot", style = "width: 100%;"),
      "Save the current correlation figure as an image file."
    )
  )
}


pairwise_correlation_visualize_ggpairs_server <- function(id, filtered_data, correlation_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    sanitize_numeric <- function(value, default, min_val, max_val) {
      v <- suppressWarnings(as.numeric(value))
      if (!length(v) || is.na(v)) return(default)
      max(min_val, min(max_val, v))
    }
    
    plot_width  <- reactive(sanitize_numeric(input$plot_width,  800, 200, 2000))
    plot_height <- reactive(sanitize_numeric(input$plot_height, 600, 200, 2000))
    
    color_var_reactive <- reactive({
      info <- correlation_info()
      if (is.null(info)) return(NULL)
      group_var <- resolve_reactive(info$group_var)
      if (is.null(group_var)) return(NULL)
      group_var <- as.character(group_var)[1]
      if (identical(group_var, "None") || !nzchar(group_var)) return(NULL)
      dat <- filtered_data()
      if (!is.data.frame(dat) || !group_var %in% names(dat)) return(NULL)
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
    
    base_size <- base_size_server(input = input, default = 11)
    grid_inputs <- plot_grid_server("plot_grid")
    
    build_ggpairs_plot <- function(data, color_value, title = NULL, base_size_value = 11) {
      validate(need(is.data.frame(data) && nrow(data) > 0, "No data available for plotting."))
      numeric_cols <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
      numeric_cols <- numeric_cols[, colSums(!is.na(numeric_cols)) > 0, drop = FALSE]
      validate(need(ncol(numeric_cols) >= 2, "Need at least two numeric columns for GGPairs plot."))
      
      plot_obj <- GGally::ggpairs(
        numeric_cols,
        progress = FALSE,
        upper = list(continuous = GGally::wrap("cor", size = 4, colour = color_value)),
        lower = list(continuous = GGally::wrap("points", alpha = 0.6, colour = color_value, size = 1.5)),
        diag  = list(continuous = GGally::wrap("densityDiag", fill = color_value, alpha = 0.4))
      ) +
        ggplot2::theme_minimal(base_size = base_size_value) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", size = 9),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()
        )
      
      if (!is.null(title)) plot_obj <- plot_obj + ggplot2::labs(title = title)
      plot_obj
    }
    
    convert_ggmatrix_to_plot <- function(plot_obj) {
      if (!inherits(plot_obj, "ggmatrix")) return(plot_obj)
      gtable <- GGally::ggmatrix_gtable(plot_obj)
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotation_custom(grob = gtable, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf)
    }
    
    plot_info <- reactive({
      info <- correlation_info()
      validate(need(!is.null(info), "Correlation results are not available."))
      results_accessor <- info$results
      results <- resolve_reactive(results_accessor)
      validate(need(!is.null(results), "Run the correlation analysis to generate plots."))
      
      if (!is.null(results$message)) validate(need(FALSE, results$message))
      
      data <- filtered_data()
      validate(need(!is.null(data) && nrow(data) > 0, "No data available."))
      
      selected_vars <- resolve_reactive(results$selected_vars)
      if (is.null(selected_vars) || length(selected_vars) < 2)
        selected_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      validate(need(length(selected_vars) >= 2, "Need at least two numeric columns for GGPairs plot."))
      
      group_var <- resolve_reactive(info$group_var)
      if (!is.null(group_var)) group_var <- as.character(group_var)[1]
      if (is.null(group_var) || identical(group_var, "None") || !nzchar(group_var)) group_var <- NULL
      
      strata_order <- resolve_reactive(info$strata_order)
      
      if (is.null(group_var)) {
        plot_data <- data[, selected_vars, drop = FALSE]
        color_choice <- resolve_single_color(custom_colors())
        plot_obj <- build_ggpairs_plot(plot_data, color_choice, base_size_value = base_size())
        defaults <- compute_default_grid(1L)
        layout <- list(nrow = defaults$rows, ncol = defaults$cols)
        list(plot = plot_obj, layout = layout, panels = 1L, warning = NULL, defaults = defaults)
      } else {
        available_levels <- names(results$matrices)
        if (!length(available_levels)) available_levels <- unique(as.character(data[[group_var]]))
        if (length(strata_order)) available_levels <- strata_order[strata_order %in% available_levels]
        available_levels <- available_levels[nzchar(available_levels)]
        validate(need(length(available_levels) > 0, "No strata available for plotting."))
        
        colors <- resolve_palette_for_levels(available_levels, custom = custom_colors())
        plots <- lapply(stats::setNames(available_levels, available_levels), function(level) {
          subset_rows <- !is.na(data[[group_var]]) & as.character(data[[group_var]]) == level
          subset_data <- data[subset_rows, selected_vars, drop = FALSE]
          if (!nrow(subset_data)) return(NULL)
          convert_ggmatrix_to_plot(
            build_ggpairs_plot(subset_data, colors[[level]], title = level, base_size_value = base_size())
          )
        })
        plots <- Filter(Negate(is.null), plots)
        validate(need(length(plots) > 0, "No data available for the selected strata."))
        
        panel_count <- length(plots)
        defaults <- compute_default_grid(panel_count)
        layout <- basic_grid_layout(
          rows = grid_inputs$rows(),
          cols = grid_inputs$cols(),
          default_rows = defaults$rows,
          default_cols = defaults$cols
        )
        layout <- adjust_grid_layout(panel_count, layout)
        validation <- validate_grid(panel_count, layout$nrow, layout$ncol)
        
        combined <- NULL
        if (isTRUE(validation$valid))
          combined <- patchwork::wrap_plots(plotlist = plots, nrow = layout$nrow, ncol = layout$ncol)
        
        list(
          plot = combined,
          layout = layout,
          panels = panel_count,
          warning = validation$message,
          defaults = defaults
        )
      }
    })
    
    # ---- New unified sizing logic ----
    size_val <- reactiveVal(list(w = 800, h = 600))
    
    observe({
      info <- plot_info()
      req(info)

      layout <- info$layout
      base_w <- plot_width()
      base_h <- plot_height()

      plot_w <- if (is.null(base_w) || is.na(base_w) || base_w <= 0) 800 else base_w
      plot_h <- if (is.null(base_h) || is.na(base_h) || base_h <= 0) 600 else base_h

      if (is.null(layout)) {
        size_val(list(w = plot_w, h = plot_h))
      } else {
        nrow_l <- ifelse(is.null(layout$nrow), 1L, as.integer(layout$nrow))
        ncol_l <- ifelse(is.null(layout$ncol), 1L, as.integer(layout$ncol))
        size_val(list(
          w = plot_w * ncol_l,
          h = plot_h * nrow_l
        ))
      }
    })
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("pairwise_correlation_ggpairs_", Sys.Date(), ".png"),
      content = function(file) {
        info <- plot_info()
        req(is.null(info$warning))
        req(info$plot)
        s <- size_val()
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
    
    list(
      plot = reactive(plot_info()$plot),
      width = reactive(size_val()$w),
      height = reactive(size_val()$h),
      warning = reactive(plot_info()$warning)
    )
  })
}

