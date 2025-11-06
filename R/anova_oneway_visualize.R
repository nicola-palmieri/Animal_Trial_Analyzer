# ===============================================================
# 🧪 Visualization Module — One-way ANOVA
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize one-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Select visualization type:",
        choices = c("Mean ± SE" = "mean_se"),
        selected = "mean_se"
      ),
      checkboxInput(
        ns("show_barplot"),
        label = "Show barplot with p-values",
        value = FALSE
      ),
      hr(),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, numericInput(ns("plot_width"), "Subplot width (px)", value = 400, min = 200, max = 1200, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50))
      ),
      add_color_customization_ui(ns, multi_group = FALSE),
      hr(),
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
    # ---- Plug in color customization module (single-color mode) ----
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    )

    build_significance_annotations <- function(pairwise_df, stats_df, factor_var) {
      if (is.null(pairwise_df) || nrow(pairwise_df) == 0) {
        return(NULL)
      }
      factor_cols <- intersect(c("Factor", "factor", "term", "Term"), names(pairwise_df))
      if (length(factor_cols) > 0) {
        factor_values <- pairwise_df[[factor_cols[1]]]
        if (is.factor(factor_values)) {
          factor_values <- as.character(factor_values)
        }
        matched <- factor_values == factor_var
        matched[is.na(matched)] <- FALSE
        pairwise_df <- pairwise_df[matched, , drop = FALSE]
      }

      if (nrow(pairwise_df) == 0 || !"p.value" %in% names(pairwise_df)) {
        return(NULL)
      }

      p_vals <- pairwise_df$p.value
      if (is.factor(p_vals)) {
        p_vals <- as.character(p_vals)
      }
      if (is.character(p_vals)) {
        cleaned <- gsub("\\*", "", p_vals, fixed = FALSE)
        cleaned <- trimws(cleaned)
        cleaned <- sub("^<", "", cleaned)
        p_vals <- suppressWarnings(as.numeric(cleaned))
      } else {
        p_vals <- suppressWarnings(as.numeric(p_vals))
      }
      valid <- !is.na(p_vals) & p_vals < 0.05
      if (!any(valid)) {
        return(NULL)
      }

      contrast_col <- intersect(c("contrast", "comparison", "Contrast", "Comparison"), names(pairwise_df))
      if (length(contrast_col) == 0) {
        return(NULL)
      }

      contrasts <- as.character(pairwise_df[[contrast_col[1]]])
      parts <- strsplit(contrasts, "\\s*-\\s*")
      group1 <- vapply(parts, function(x) if (length(x) >= 1) trimws(x[1]) else NA_character_, character(1))
      group2 <- vapply(parts, function(x) if (length(x) >= 2) trimws(x[length(x)]) else NA_character_, character(1))

      group1 <- group1[valid]
      group2 <- group2[valid]
      p_vals <- p_vals[valid]

      valid_pairs <- !is.na(group1) & !is.na(group2)
      if (!any(valid_pairs)) {
        return(NULL)
      }

      group1 <- group1[valid_pairs]
      group2 <- group2[valid_pairs]
      p_vals <- p_vals[valid_pairs]

      if (!factor_var %in% names(stats_df)) {
        return(NULL)
      }

      if (!is.factor(stats_df[[factor_var]])) {
        stats_df[[factor_var]] <- factor(as.character(stats_df[[factor_var]]))
      }

      level_values <- levels(stats_df[[factor_var]])
      level_values <- level_values[!is.na(level_values)]

      positions1 <- match(group1, level_values)
      positions2 <- match(group2, level_values)

      keep <- !is.na(positions1) & !is.na(positions2)
      if (!any(keep)) {
        return(NULL)
      }

      positions1 <- positions1[keep]
      positions2 <- positions2[keep]
      p_vals <- p_vals[keep]

      annotations <- ifelse(p_vals < 0.001, "***",
                            ifelse(p_vals < 0.01, "**", "*"))

      if ("ymax" %in% names(stats_df)) {
        upper_vals <- stats_df$ymax
      } else {
        se_vals <- if ("se" %in% names(stats_df)) stats_df$se else rep(0, nrow(stats_df))
        upper_vals <- stats_df$mean + se_vals
      }
      upper_vals[!is.finite(upper_vals)] <- stats_df$mean[!is.finite(upper_vals)]

      base_y <- suppressWarnings(max(upper_vals, na.rm = TRUE))
      if (!is.finite(base_y)) {
        base_y <- suppressWarnings(max(stats_df$mean, na.rm = TRUE))
      }
      if (!is.finite(base_y)) {
        base_y <- 0
      }

      step <- abs(base_y) * 0.1
      if (!is.finite(step) || step == 0) {
        step <- 0.1
      }

      order_idx <- order(p_vals)
      positions1 <- positions1[order_idx]
      positions2 <- positions2[order_idx]
      annotations <- annotations[order_idx]
      p_vals <- p_vals[order_idx]

      y_positions <- base_y + step * seq_along(p_vals)

      xmin_labels <- level_values[positions1]
      xmax_labels <- level_values[positions2]

      signif_df <- data.frame(
        xmin = positions1,
        xmax = positions2,
        annotations = annotations,
        y_position = y_positions,
        stringsAsFactors = FALSE
      )

      # Provide the original factor labels for downstream consumers that may want
      # to display or debug the comparisons.
      signif_df$xmin_label <- factor(xmin_labels, levels = level_values)
      signif_df$xmax_label <- factor(xmax_labels, levels = level_values)

      list(
        data = signif_df,
        max_y = suppressWarnings(max(y_positions, na.rm = TRUE))
      )
    }

    build_bar_plot_for_stats <- function(stats_df, title_text, factor_var, fill_color, pairwise_df) {
      if (is.null(stats_df) || nrow(stats_df) == 0) {
        return(NULL)
      }

      stats_df <- as.data.frame(stats_df)

      if (!factor_var %in% names(stats_df)) {
        return(NULL)
      }

      stats_df[[factor_var]] <- factor(as.character(stats_df[[factor_var]]))
      se_vals <- if ("se" %in% names(stats_df)) stats_df$se else rep(0, nrow(stats_df))
      stats_df$ymin <- stats_df$mean - se_vals
      stats_df$ymax <- stats_df$mean + se_vals

      annotation_info <- build_significance_annotations(pairwise_df, stats_df, factor_var)

      y_lower <- suppressWarnings(min(stats_df$ymin, na.rm = TRUE))
      y_upper <- suppressWarnings(max(stats_df$ymax, na.rm = TRUE))

      if (!is.null(annotation_info) && !is.null(annotation_info$max_y) && is.finite(annotation_info$max_y)) {
        if (!is.finite(y_upper)) {
          y_upper <- annotation_info$max_y
        } else {
          y_upper <- max(y_upper, annotation_info$max_y)
        }
      }

      y_limits <- NULL
      if (is.finite(y_lower) && is.finite(y_upper)) {
        # Always include 0 so bars are visible (geom_col draws from 0)
        low <- min(0, y_lower)
        span <- y_upper - low
        if (!is.finite(span) || span == 0) {
          span <- ifelse(abs(y_upper) > 0, abs(y_upper), 1)
        }
        padding <- span * 0.1
        if (!is.finite(padding) || padding == 0) {
          padding <- 0.1
        }
        y_limits <- c(low - padding * 0.25, y_upper + padding)
      }
      

      p <- ggplot(stats_df, aes(x = .data[[factor_var]], y = .data$mean)) +
        geom_col(fill = fill_color, color = fill_color, width = 0.65, stat = "identity") +
        geom_errorbar(aes(ymin = .data$ymin, ymax = .data$ymax), width = 0.15) +
        theme_minimal(base_size = 14) +
        labs(x = factor_var, y = "Mean ± SE") +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )

      if (!is.null(annotation_info) && !is.null(annotation_info$data) &&
          nrow(annotation_info$data) > 0) {
        p <- p + ggsignif::geom_signif(
          data = annotation_info$data,
          manual = TRUE,
          inherit.aes = FALSE,
          tip_length = 0.01,
          textsize = 4
        )
      }

      if (!is.null(y_limits) && all(is.finite(y_limits))) {
        p <- p + scale_y_continuous(limits = y_limits)
      }

      p + ggtitle(title_text) +
        theme(plot.title = element_text(size = 12, face = "bold"))
    }

    # ---- Build plot info ----
    plot_info <- reactive({
      info <- model_info()
      req(info)
      validate(
        need(info$type == "oneway_anova", "No one-way ANOVA results available for plotting.")
      )
      data <- df()
      layout_inputs <- list(
        strata_rows = input$strata_rows,
        strata_cols = input$strata_cols,
        resp_rows = input$resp_rows,
        resp_cols = input$resp_cols
      )

      build_anova_plot_info(
        data,
        info,
        layout_inputs,
        line_colors = custom_colors()
      )
    })
    
    plot_obj <- reactive({
      info <- plot_info()
      req(info)
      if (!is.null(info$warning) || is.null(info$plot)) {
        return(NULL)
      }
      info$plot
    })
    
    plot_size <- reactive({
      info <- plot_info()
      req(info)
      s <- plot_info()$layout
      list(
        w = input$plot_width * s$strata$cols * s$responses$ncol,
        h = input$plot_height * s$strata$rows * s$responses$nrow
      )
    })

    pairwise_results <- reactive({
      info <- model_info()
      req(info)

      posthoc <- info$posthoc
      if (is.null(posthoc) || length(posthoc) == 0) {
        return(NULL)
      }

      posthoc
    })

    bar_plot_obj <- reactive({
      if (!isTRUE(input$show_barplot)) {
        return(NULL)
      }

      info <- plot_info()
      if (is.null(info) || !is.null(info$warning)) {
        return(NULL)
      }

      stats <- info$summary_stats
      if (is.null(stats) || length(stats) == 0) {
        return(NULL)
      }

      meta <- model_info()
      req(meta)

      factor1 <- meta$factors$factor1
      if (is.null(factor1)) {
        return(NULL)
      }

      responses <- meta$responses
      has_strata <- isTRUE(info$has_strata)

      colors <- custom_colors()
      fill_color <- if (!is.null(colors) && length(colors) > 0) {
        unname(colors)[1]
      } else {
        resolve_single_color()
      }

      pairwise <- pairwise_results()
      response_plots <- list()

      if (has_strata) {
        for (resp in responses) {
          stats_entry <- stats[[resp]]
          if (is.null(stats_entry) || is.null(stats_entry$strata)) next

          stratum_plots <- list()
          stratum_names <- names(stats_entry$strata)

          for (stratum_name in stratum_names) {
            stratum_df <- stats_entry$strata[[stratum_name]]
            if (is.null(stratum_df) || nrow(stratum_df) == 0) next

            pairwise_df <- NULL
            if (!is.null(pairwise) && !is.null(pairwise[[resp]]) &&
                !is.null(pairwise[[resp]][[stratum_name]])) {
              pairwise_df <- pairwise[[resp]][[stratum_name]]
            }

            plot <- build_bar_plot_for_stats(stratum_df, stratum_name, factor1, fill_color, pairwise_df)
            if (!is.null(plot)) {
              stratum_plots[[stratum_name]] <- plot
            }
          }

          if (length(stratum_plots) == 0) next

          combined <- patchwork::wrap_plots(
            plotlist = stratum_plots,
            nrow = info$layout$strata$rows,
            ncol = info$layout$strata$cols
          )

          title_plot <- ggplot() +
            theme_void() +
            ggtitle(resp) +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.margin = margin(t = 0, r = 0, b = 6, l = 0)
            )

          response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
        }
      } else {
        for (resp in responses) {
          stats_entry <- stats[[resp]]
          if (is.null(stats_entry) || is.null(stats_entry$data) || nrow(stats_entry$data) == 0) next

          pairwise_df <- if (!is.null(pairwise)) pairwise[[resp]] else NULL
          plot <- build_bar_plot_for_stats(stats_entry$data, resp, factor1, fill_color, pairwise_df)
          if (!is.null(plot)) {
            response_plots[[resp]] <- plot
          }
        }
      }

      if (length(response_plots) == 0) {
        return(NULL)
      }

      if (length(response_plots) == 1) {
        return(response_plots[[1]])
      }

      patchwork::wrap_plots(
        plotlist = response_plots,
        nrow = info$layout$responses$nrow,
        ncol = info$layout$responses$ncol
      ) &
        patchwork::plot_layout(guides = "collect")
    })

    observeEvent(plot_info(), {
      info <- plot_info()
      if (is.null(info) || is.null(info$defaults) || is.null(info$layout)) {
        return()
      }

      if (!is.null(info$defaults$strata)) {
        defaults <- info$defaults$strata
        rows <- defaults$rows
        cols <- defaults$cols
        if (!is.null(rows) && !is.null(cols)) {
          sync_numeric_input(session, "strata_rows", input$strata_rows, rows)
          sync_numeric_input(session, "strata_cols", input$strata_cols, cols)
        }
      }

      if (!is.null(info$defaults$responses)) {
        defaults <- info$defaults$responses
        rows <- defaults$rows
        cols <- defaults$cols
        if (!is.null(rows) && !is.null(cols)) {
          sync_numeric_input(session, "resp_rows", input$resp_rows, rows)
          sync_numeric_input(session, "resp_cols", input$resp_cols, cols)
        }
      }
    }, ignoreNULL = FALSE)
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })

    output$plot_warning <- renderUI({
      info <- plot_info()
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", HTML(info$warning))
      } else {
        NULL
      }
    })
    
    # ---- Render plot ----
    output$plot <- renderPlot({
      info <- model_info()
      req(info, input$plot_type)

      plot <- if (isTRUE(input$show_barplot) && input$plot_type == "mean_se") {
        bar_plot_obj()
      } else {
        plot_obj()
      }

      if (is.null(plot)) return(NULL)
      plot
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    # ---- Download handler ----
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- plot_info()
        req(is.null(info$warning))
        plot <- if (isTRUE(input$show_barplot) && input$plot_type == "mean_se") {
          bar_plot_obj()
        } else {
          plot_obj()
        }
        req(plot)
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          dpi = 300,
          width = plot_size()$w / 96,
          height = plot_size()$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
