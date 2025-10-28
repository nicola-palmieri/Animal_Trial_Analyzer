# ===============================================================
# 🧪 Pairwise Correlation — GGPairs Visualization Module
# ===============================================================

pairwise_correlation_visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  800, 200, 2000, 50)),
      column(6, numericInput(ns("plot_height"), "Subplot height (px)", 600, 200, 2000, 50))
    ),
    fluidRow(
      column(6, numericInput(ns("grid_rows"),    "Grid rows",    1, 1, 10, 1)),
      column(6, numericInput(ns("grid_cols"),    "Grid columns", 1, 1, 10, 1))
    ),
    hr(),
    downloadButton(ns("download_plot"), "Download Plot")
  )
}


pairwise_correlation_visualize_ggpairs_server <- function(id, filtered_data, correlation_info) {
  moduleServer(id, function(input, output, session) {

    resolve_input_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x)) x() else x
    }

    sanitize_numeric <- function(value, default, min_val, max_val) {
      v <- suppressWarnings(as.numeric(value))
      if (length(v) == 0 || is.na(v)) return(default)
      v <- max(min_val, min(max_val, v))
      v
    }

    plot_width <- reactive({
      sanitize_numeric(input$plot_width, 800, 200, 2000)
    })

    plot_height <- reactive({
      sanitize_numeric(input$plot_height, 600, 200, 2000)
    })

    grid_rows <- reactive({
      as.integer(sanitize_numeric(input$grid_rows, 1, 1, 10))
    })

    grid_cols <- reactive({
      as.integer(sanitize_numeric(input$grid_cols, 1, 1, 10))
    })

    build_ggpairs_plot <- function(data, color_value, title = NULL) {
      validate(need(is.data.frame(data) && nrow(data) > 0, "No data available for plotting."))

      numeric_cols <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
      numeric_cols <- numeric_cols[, colSums(!is.na(numeric_cols)) > 0, drop = FALSE]

      validate(need(ncol(numeric_cols) >= 2, "Need at least two numeric columns for GGPairs plot."))

      plot_obj <- GGally::ggpairs(
        numeric_cols,
        progress = FALSE,
        upper = list(
          continuous = GGally::wrap("cor", size = 4, colour = color_value)
        ),
        lower = list(
          continuous = GGally::wrap("points", alpha = 0.6, colour = color_value, size = 1.5)
        ),
        diag = list(
          continuous = GGally::wrap("densityDiag", fill = color_value, alpha = 0.4)
        )
      ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", size = 9),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank()
        )

      if (!is.null(title)) {
        plot_obj <- plot_obj + ggplot2::labs(title = title)
      }

      plot_obj
    }

    plot_info <- reactive({
      info <- correlation_info()
      validate(need(!is.null(info), "Correlation results are not available."))

      results_accessor <- info$results
      results <- resolve_input_value(results_accessor)

      validate(need(!is.null(results), "Run the correlation analysis to generate plots."))

      if (!is.null(results$message)) {
        validate(need(FALSE, results$message))
      }

      data <- filtered_data()
      validate(need(!is.null(data) && nrow(data) > 0, "No data available."))

      selected_vars <- resolve_input_value(results$selected_vars)
      if (is.null(selected_vars) || length(selected_vars) < 2) {
        numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
        selected_vars <- numeric_vars
      }

      validate(need(length(selected_vars) >= 2, "Need at least two numeric columns for GGPairs plot."))

      group_var <- resolve_input_value(info$group_var)
      if (is.null(group_var) || identical(group_var, "None") || identical(group_var, "")) {
        group_var <- NULL
      }

      strata_order <- resolve_input_value(info$strata_order)

      if (is.null(group_var)) {
        plot_data <- data[, selected_vars, drop = FALSE]
        plot_obj <- build_ggpairs_plot(plot_data, basic_color_palette[1])
        layout <- list(nrow = 1L, ncol = 1L)
        list(plot = plot_obj, layout = layout)
      } else {
        available_levels <- NULL
        if (!is.null(results$matrices)) {
          available_levels <- names(results$matrices)
        }
        if (is.null(available_levels) || length(available_levels) == 0) {
          available_levels <- unique(as.character(data[[group_var]]))
        }
        if (!is.null(strata_order) && length(strata_order) > 0) {
          available_levels <- strata_order[strata_order %in% available_levels]
        }
        available_levels <- available_levels[nzchar(available_levels)]
        validate(need(length(available_levels) > 0, "No strata available for plotting."))

        colors <- basic_color_palette[seq_len(min(length(basic_color_palette), length(available_levels)))]
        if (length(colors) < length(available_levels)) {
          extra <- rep("#7F7F7F", length(available_levels) - length(colors))
          colors <- c(colors, extra)
        }
        names(colors) <- available_levels

        plots <- list()
        for (level in available_levels) {
          subset_rows <- !is.na(data[[group_var]]) & as.character(data[[group_var]]) == level
          subset_data <- data[subset_rows, selected_vars, drop = FALSE]
          if (nrow(subset_data) == 0) {
            next
          }
          plots[[level]] <- build_ggpairs_plot(subset_data, colors[[level]], title = level)
        }

        validate(need(length(plots) > 0, "No data available for the selected strata."))

        combined <- patchwork::wrap_plots(plotlist = plots, nrow = grid_rows(), ncol = grid_cols())
        layout <- list(nrow = grid_rows(), ncol = grid_cols())
        list(plot = combined, layout = layout)
      }
    })

    plot_width_total <- reactive({
      info <- plot_info()
      layout <- info$layout
      w <- plot_width()
      if (!is.null(layout$ncol)) {
        w <- w * max(1L, as.integer(layout$ncol))
      }
      w
    })

    plot_height_total <- reactive({
      info <- plot_info()
      layout <- info$layout
      h <- plot_height()
      if (!is.null(layout$nrow)) {
        h <- h * max(1L, as.integer(layout$nrow))
      }
      h
    })

    output$download_plot <- downloadHandler(
      filename = function() paste0("pairwise_correlation_ggpairs_", Sys.Date(), ".png"),
      content = function(file) {
        info <- plot_info()
        plot_obj <- info$plot
        req(plot_obj)
        ggplot2::ggsave(
          filename = file,
          plot = plot_obj,
          device = "png",
          dpi = 300,
          width = plot_width_total() / 96,
          height = plot_height_total() / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )

    list(
      plot = reactive({ plot_info()$plot }),
      width = reactive(plot_width_total()),
      height = reactive(plot_height_total())
    )
  })
}
