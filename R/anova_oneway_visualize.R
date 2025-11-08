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
        choices = c(
          "Lineplots (mean ± SE)" = "lineplot_mean_se",
          "Barplots (mean ± SE)"  = "barplot_mean_se"
        ),
        selected = "lineplot_mean_se"
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
      req(info, input$plot_type)
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

      colors <- custom_colors()

      if (identical(input$plot_type, "barplot_mean_se")) {
        posthoc_all <- compile_anova_results(info)$posthoc
        plot_oneway_barplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors = colors,
          posthoc_all = posthoc_all
        )
      } else {
        build_anova_plot_info(
          data,
          info,
          layout_inputs,
          line_colors = colors
        )
      }
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
      s <- info$layout
      strata_rows <- if (!is.null(s$strata$rows)) s$strata$rows else 1
      strata_cols <- if (!is.null(s$strata$cols)) s$strata$cols else 1
      resp_rows <- if (!is.null(s$responses$rows)) s$responses$rows else 1
      resp_cols <- if (!is.null(s$responses$cols)) s$responses$cols else 1
      list(
        w = input$plot_width * strata_cols * resp_cols,
        h = input$plot_height * strata_rows * resp_rows
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
      if (is.null(info)) {
        return(NULL)
      }
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", HTML(info$warning))
      } else {
        NULL
      }
    })
    
    # ---- Render plot ----
    output$plot <- renderPlot({
      plot <- plot_obj()
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
        req(info)
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

extract_tukey_for_signif <- function(posthoc_entry) {
  if (is.null(posthoc_entry) || !is.data.frame(posthoc_entry)) return(NULL)
  
  df <- posthoc_entry
  
  # split contrast into group1 and group2
  parts <- strsplit(as.character(df$contrast), " - ")
  df$group1 <- vapply(parts, `[`, "", 1)
  df$group2 <- vapply(parts, `[`, "", 2)
  
  # clean p.value column
  df$p.value <- as.character(df$p.value)
  df$p.value <- gsub("\\*", "", df$p.value)          # remove any stars
  df$p.value <- gsub("<0\\.001", "0.0009", df$p.value)  # make "<0.001" numeric
  df$p.value <- suppressWarnings(as.numeric(df$p.value))
  
  df <- df %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::select(group1, group2, p.value)
  
  df
}


plot_oneway_barplot_meanse <- function(data, info, layout_values = list(), line_colors = NULL, posthoc_all = NULL) {

  factor1 <- info$factors$factor1
  responses <- info$responses
  if (is.null(factor1) || length(responses) == 0) {
    return(NULL)
  }

  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else character(0)
  if (has_strata && (is.null(strata_levels) || length(strata_levels) == 0)) {
    strata_levels <- unique(as.character(stats::na.omit(data[[strat_var]])))
  }

  order1 <- info$orders$order1
  if (!is.null(order1) && factor1 %in% names(data)) {
    data[[factor1]] <- factor(as.character(data[[factor1]]), levels = order1)
  }

  layout_input <- list(
    strata_rows = suppressWarnings(as.numeric(layout_values$strata_rows)),
    strata_cols = suppressWarnings(as.numeric(layout_values$strata_cols)),
    resp_rows   = suppressWarnings(as.numeric(layout_values$resp_rows)),
    resp_cols   = suppressWarnings(as.numeric(layout_values$resp_cols))
  )

  n_expected_strata <- if (has_strata) max(1L, length(strata_levels)) else 1L
  strata_defaults <- if (has_strata) compute_default_grid(n_expected_strata) else list(rows = 1L, cols = 1L)
  strata_layout <- basic_grid_layout(
    rows = layout_input$strata_rows,
    cols = layout_input$strata_cols,
    default_rows = strata_defaults$rows,
    default_cols = strata_defaults$cols
  )

  response_plots <- list()
  strata_panel_count <- if (has_strata) 0L else 1L

  fill_color <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }

  for (resp in responses) {

    if (has_strata && !is.null(strat_var) && strat_var %in% names(data)) {
      stratum_plots <- list()

      for (stratum in strata_levels) {
        subset_data <- data[!is.na(data[[strat_var]]) & data[[strat_var]] == stratum, , drop = FALSE]
        if (nrow(subset_data) == 0) {
          next
        }

        stats_df <- subset_data %>%
          dplyr::group_by(.data[[factor1]]) %>%
          dplyr::summarise(
            mean = mean(.data[[resp]], na.rm = TRUE),
            se   = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
            .groups = "drop"
          )

        if (nrow(stats_df) == 0) {
          next
        }

        levels_to_use <- if (!is.null(order1)) order1 else unique(as.character(stats_df[[factor1]]))
        stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]), levels = levels_to_use)

        signif_df <- NULL
        if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]]) && !is.null(posthoc_all[[resp]][[stratum]])) {
          signif_df <- extract_tukey_for_signif(posthoc_all[[resp]][[stratum]])
        }

        p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
          geom_col(
            fill  = fill_color,
            width = 0.6,
            alpha = 0.8
          ) +
          geom_errorbar(
            aes(ymin = mean - se, ymax = mean + se),
            width     = 0.15,
            color     = "gray40",
            linewidth = 0.5
          ) +
          theme_minimal(base_size = 14) +
          labs(
            x     = factor1,
            y     = "Mean ± SE",
            title = stratum
          ) +
          theme(
            plot.title    = element_text(size = 14, face = "bold", hjust = 0.5),
            axis.title.x  = element_text(margin = margin(t = 6)),
            axis.title.y  = element_text(margin = margin(r = 6)),
            axis.text.x   = element_text(angle = 30, hjust = 1),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = "gray90")
          )

        if (!is.null(signif_df) && nrow(signif_df) > 0) {
          signif_df <- signif_df %>%
            dplyr::filter(p.value < 0.05)
          max_y <- max(stats_df$mean + stats_df$se, na.rm = TRUE)

          signif_df <- signif_df %>%
            dplyr::mutate(
              y_position = seq(
                from = max_y * 1.05,
                by   = max_y * 0.05,
                length.out = n()
              ),
              label = dplyr::case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01  ~ "**",
                p.value < 0.05  ~ "*",
                TRUE            ~ sprintf("p=%.3f", p.value)
              )
            )

          p <- p + ggsignif::geom_signif(
            data      = signif_df,
            aes(xmin = group1, xmax = group2, annotations = label, y_position = y_position),
            manual    = TRUE,
            tip_length = 0.01,
            textsize   = 3.8,
            vjust      = 0.5,
            color      = "gray30"
          )
        }

        stratum_plots[[stratum]] <- p
      }

      if (length(stratum_plots) > 0) {
        strata_panel_count <- max(strata_panel_count, length(stratum_plots))
        current_layout <- adjust_grid_layout(length(stratum_plots), strata_layout)
        combined <- patchwork::wrap_plots(
          plotlist = stratum_plots,
          nrow = current_layout$nrow,
          ncol = current_layout$ncol
        )

        title_plot <- ggplot() +
          theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }

    } else {
      stats_df <- data %>%
        dplyr::group_by(.data[[factor1]]) %>%
        dplyr::summarise(
          mean = mean(.data[[resp]], na.rm = TRUE),
          se   = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
          .groups = "drop"
        )

      if (nrow(stats_df) == 0) {
        next
      }

      levels_to_use <- if (!is.null(order1)) order1 else unique(as.character(stats_df[[factor1]]))
      stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]), levels = levels_to_use)

      signif_df <- NULL
      if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
        signif_df <- extract_tukey_for_signif(posthoc_all[[resp]])
      }

      p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
        geom_col(fill = fill_color, width = 0.6, alpha = 0.8) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.15,
          color = "gray40",
          linewidth = 0.5
        ) +
        theme_minimal(base_size = 14) +
        labs(
          x = factor1,
          y = "Mean ± SE",
          title = resp
        ) +
        theme(
          plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          axis.title.x = element_text(margin = margin(t = 6)),
          axis.title.y = element_text(margin = margin(r = 6)),
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "gray90"),
          axis.text.x = element_text(angle = 30, hjust = 1)
        )

      if (!is.null(signif_df) && nrow(signif_df) > 0) {
        signif_df <- signif_df %>%
          dplyr::filter(p.value < 0.05)
        max_y <- max(stats_df$mean + stats_df$se, na.rm = TRUE)

        signif_df <- signif_df %>%
          dplyr::mutate(
            y_position = seq(
              from = max_y * 1.05,
              by   = max_y * 0.05,
              length.out = n()
            ),
            label = dplyr::case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              TRUE            ~ sprintf("p=%.3f", p.value)
            ),
            xmin = group1,
            xmax = group2,
            annotations = label
          )

        p <- p + ggsignif::geom_signif(
          data      = signif_df,
          aes(
            xmin       = group1,
            xmax       = group2,
            annotations = label,
            y_position = y_position
          ),
          manual    = TRUE,
          tip_length = 0.01,
          textsize   = 3.8,
          vjust      = 0.5,
          color      = "gray30"
        )
      }

      response_plots[[resp]] <- p
    }
  }

  if (length(response_plots) == 0) {
    return(NULL)
  }

  if (has_strata && strata_panel_count == 0L) {
    strata_panel_count <- n_expected_strata
  }

  if (has_strata) {
    strata_layout <- adjust_grid_layout(max(1L, strata_panel_count), strata_layout)
  }

  response_defaults <- compute_default_grid(length(response_plots))
  response_layout <- basic_grid_layout(
    rows = layout_input$resp_rows,
    cols = layout_input$resp_cols,
    default_rows = response_defaults$rows,
    default_cols = response_defaults$cols
  )
  response_layout <- adjust_grid_layout(length(response_plots), response_layout)

  strata_validation <- if (has_strata) {
    validate_grid(max(1L, strata_panel_count), strata_layout$nrow, strata_layout$ncol)
  } else {
    list(valid = TRUE, message = NULL)
  }
  response_validation <- validate_grid(length(response_plots), response_layout$nrow, response_layout$ncol)

  warnings <- c()
  if (has_strata && !strata_validation$valid && !is.null(strata_validation$message)) {
    warnings <- c(warnings, strata_validation$message)
  }
  if (!response_validation$valid && !is.null(response_validation$message)) {
    warnings <- c(warnings, response_validation$message)
  }
  warning_text <- if (length(warnings) > 0) paste(warnings, collapse = "<br/>") else NULL

  final_plot <- NULL
  if (is.null(warning_text)) {
    final_plot <- if (length(response_plots) == 1) {
      response_plots[[1]]
    } else {
      patchwork::wrap_plots(
        plotlist = response_plots,
        nrow = response_layout$nrow,
        ncol = response_layout$ncol
      )
    }
  }

  list(
    plot = final_plot,
    layout = list(
      strata = list(
        rows = if (has_strata) strata_layout$nrow else 1L,
        cols = if (has_strata) strata_layout$ncol else 1L
      ),
      responses = list(
        rows = response_layout$nrow,
        cols = response_layout$ncol
      )
    ),
    warning = warning_text,
    defaults = list(
      strata = strata_defaults,
      responses = response_defaults
    )
  )
}




