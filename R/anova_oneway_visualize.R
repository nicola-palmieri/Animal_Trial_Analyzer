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
    
    parse_posthoc_p <- function(values) {
      if (is.null(values)) {
        return(numeric(0))
      }
      if (is.numeric(values)) {
        return(as.numeric(values))
      }
      vals <- as.character(values)
      star_mask <- grepl("\\*", vals, fixed = TRUE)
      vals_clean <- gsub("\\*", "", vals, fixed = TRUE)
      vals_clean <- trimws(vals_clean)
      parsed <- vapply(vals_clean, function(v) {
        if (!nzchar(v)) return(NA_real_)
        if (substr(v, 1, 1) == "<") {
          suppressWarnings(as.numeric(sub("^<\\s*", "", v)))
        } else {
          suppressWarnings(as.numeric(v))
        }
      }, numeric(1))
      parsed[star_mask & !is.na(parsed)] <- pmax(0, parsed[star_mask & !is.na(parsed)] - 1e-06)
      parsed[star_mask & is.na(parsed)] <- 0.049
      parsed
    }

    build_significance_annotations <- function(posthoc_entry, factor_name, stats_df) {
      if (is.null(posthoc_entry) || !is.data.frame(posthoc_entry)) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }
      if (is.null(factor_name) || !factor_name %in% names(stats_df)) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }
      if (!"Factor" %in% names(posthoc_entry) || !"contrast" %in% names(posthoc_entry)) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }

      contrast_df <- posthoc_entry[posthoc_entry$Factor == factor_name, , drop = FALSE]
      if (nrow(contrast_df) == 0 || !"p.value" %in% names(contrast_df)) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }

      raw_p <- if ("p.value_raw" %in% names(contrast_df)) {
        suppressWarnings(as.numeric(contrast_df$p.value_raw))
      } else {
        parse_posthoc_p(contrast_df$p.value)
      }
      significant_rows <- which(!is.na(raw_p) & raw_p < 0.05)
      if (length(significant_rows) == 0) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }

      x_levels <- if (is.factor(stats_df[[factor_name]])) levels(stats_df[[factor_name]]) else unique(as.character(stats_df[[factor_name]]))
      x_levels <- x_levels[!is.na(x_levels)]
      stats_df[[factor_name]] <- factor(stats_df[[factor_name]], levels = x_levels)

      build_star <- function(p) {
        if (is.na(p)) return(NA_character_)
        if (p < 0.001) {
          "***"
        } else if (p < 0.01) {
          "**"
        } else if (p < 0.05) {
          "*"
        } else {
          NA_character_
        }
      }

      used_pairs <- character(0)
      rows <- lapply(significant_rows, function(idx) {
        contrast <- as.character(contrast_df$contrast[idx])
        parts <- trimws(unlist(strsplit(contrast, "-", fixed = TRUE)))
        if (length(parts) != 2) {
          parts <- trimws(unlist(strsplit(contrast, " - ", fixed = TRUE)))
        }
        if (length(parts) != 2) return(NULL)
        if (!all(parts %in% x_levels)) return(NULL)
        annotation <- build_star(raw_p[idx])
        if (!nzchar(annotation)) return(NULL)
        key <- paste(sort(parts), collapse = "__")
        if (key %in% used_pairs) return(NULL)
        used_pairs <<- c(used_pairs, key)
        list(
          group1 = parts[1],
          group2 = parts[2],
          annotation = annotation
        )
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0) {
        return(list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE)))
      }

      base_y <- suppressWarnings(max(stats_df$mean + stats_df$se, na.rm = TRUE))
      if (!is.finite(base_y)) base_y <- 0
      pad <- if (base_y == 0) 0.1 else abs(base_y) * 0.1

      annotations_df <- data.frame(
        xmin = factor(vapply(rows, `[[`, character(1), "group1"), levels = x_levels),
        xmax = factor(vapply(rows, `[[`, character(1), "group2"), levels = x_levels),
        annotation = vapply(rows, `[[`, character(1), "annotation"),
        stringsAsFactors = FALSE
      )
      annotations_df$xmin <- as.character(annotations_df$xmin)
      annotations_df$xmax <- as.character(annotations_df$xmax)

      if (nrow(annotations_df) == 0) {
        return(list(data = NULL, ymax = max(base_y, 0)))
      }

      annotations_df$y_position <- base_y + pad + (seq_len(nrow(annotations_df)) - 1) * pad

      list(
        data = annotations_df,
        ymax = max(annotations_df$y_position, na.rm = TRUE) + pad * 0.5
      )
    }

    build_barplot_with_significance <- function(info_obj, model_obj, fill_colors) {
      if (is.null(info_obj$summary_stats) || length(info_obj$summary_stats) == 0) {
        return(NULL)
      }

      factor1 <- model_obj$factors$factor1
      factor2 <- model_obj$factors$factor2
      posthoc <- model_obj$posthoc %||% list()
      responses <- names(info_obj$summary_stats)

      if (is.null(factor1) || length(responses) == 0) {
        return(NULL)
      }

      color_values <- fill_colors()
      single_fill <- if (is.null(color_values) || length(color_values) == 0) {
        resolve_single_color()
      } else {
        unname(color_values)[1]
      }

      build_panel_plot <- function(panel_info, resp_name, stratum_name = NULL) {
        stats_df <- panel_info$data
        if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)
        stats_df$mean <- as.numeric(stats_df$mean)
        stats_df$se <- as.numeric(stats_df$se)
        if (factor1 %in% names(stats_df) && !is.factor(stats_df[[factor1]])) {
          lvl_order <- unique(as.character(stats_df[[factor1]]))
          stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]), levels = lvl_order)
        }

        posthoc_entry <- NULL
        if (is.list(posthoc) && resp_name %in% names(posthoc)) {
          posthoc_entry <- posthoc[[resp_name]]
          if (is.list(posthoc_entry) && !is.data.frame(posthoc_entry)) {
            if (!is.null(stratum_name) && stratum_name %in% names(posthoc_entry)) {
              posthoc_entry <- posthoc_entry[[stratum_name]]
            } else {
              posthoc_entry <- NULL
            }
          }
        }

        annotation_info <- tryCatch(
          build_significance_annotations(posthoc_entry, factor1, stats_df),
          error = function(...) list(data = NULL, ymax = max(stats_df$mean + stats_df$se, na.rm = TRUE))
        )

        y_cap <- suppressWarnings(max(stats_df$mean + stats_df$se, na.rm = TRUE))
        if (!is.finite(y_cap)) y_cap <- 0
        if (!is.null(annotation_info$ymax) && is.finite(annotation_info$ymax)) {
          y_cap <- max(y_cap, annotation_info$ymax)
        }
        if (y_cap <= 0) {
          y_cap <- 1
        }

        p <- NULL
        if (is.null(factor2) || !factor2 %in% names(stats_df)) {
          fill_value <- single_fill
          p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
            geom_col(fill = fill_value, width = 0.65) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)
        } else {
          vals <- stats_df[[factor2]]
          group_levels <- if (is.factor(vals)) levels(vals) else unique(as.character(vals))
          group_levels <- group_levels[!is.na(group_levels)]

          if (length(group_levels) == 0) {
            fill_value <- single_fill
            p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
              geom_col(fill = fill_value, width = 0.65) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2)
          } else {
            palette <- resolve_palette_for_levels(group_levels, custom = color_values)
            stats_df[[factor2]] <- factor(as.character(stats_df[[factor2]]), levels = group_levels)
            dodge <- position_dodge(width = 0.7)
            p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean, fill = !!sym(factor2))) +
              geom_col(position = dodge, width = 0.6) +
              geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2, position = dodge) +
              scale_fill_manual(values = palette) +
              labs(fill = factor2)
            annotation_info$data <- NULL
          }
        }

        p <- p +
          scale_y_continuous(limits = c(0, y_cap), expand = expansion(mult = c(0, 0.05))) +
          labs(x = factor1, y = "Mean ± SE") +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title = element_text(size = 12, face = "bold")
          ) +
          ggtitle(panel_info$title)

        if (!is.null(annotation_info$data) && nrow(annotation_info$data) > 0) {
          p <- p +
            ggsignif::geom_signif(
              data = annotation_info$data,
              aes(xmin = xmin, xmax = xmax, annotations = annotation, y_position = y_position),
              inherit.aes = FALSE,
              tip_length = 0.01,
              textsize = 4
            )
        }

        p
      }

      response_plots <- list()
      for (resp_name in responses) {
        resp_summary <- info_obj$summary_stats[[resp_name]]
        if (is.null(resp_summary) || length(resp_summary$panels) == 0) next

        panel_plots <- lapply(resp_summary$panels, function(panel) {
          build_panel_plot(panel, resp_name, panel$stratum)
        })
        panel_plots <- Filter(Negate(is.null), panel_plots)
        if (length(panel_plots) == 0) next

        if (info_obj$has_strata) {
          combined <- patchwork::wrap_plots(
            plotlist = panel_plots,
            nrow = resp_summary$strata_layout$nrow,
            ncol = resp_summary$strata_layout$ncol
          )
          title_plot <- ggplot() +
            theme_void() +
            ggtitle(resp_name) +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.margin = margin(t = 0, r = 0, b = 6, l = 0)
            )
          response_plots[[resp_name]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
        } else {
          response_plots[[resp_name]] <- panel_plots[[1]]
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
        nrow = info_obj$layout$responses$nrow,
        ncol = info_obj$layout$responses$ncol
      ) &
        patchwork::plot_layout(guides = "collect")
    }

    plot_obj <- reactive({
      info <- plot_info()
      req(info)
      if (!is.null(info$warning) || is.null(info$plot)) {
        return(NULL)
      }

      if (isTRUE(input$show_barplot)) {
        model_details <- model_info()
        if (!is.null(model_details)) {
          bar_plot <- build_barplot_with_significance(info, model_details, custom_colors)
          if (!is.null(bar_plot)) {
            return(bar_plot)
          }
        }
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
      plot <- plot_obj()
      req(plot)
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
        plot <- plot_obj()
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
