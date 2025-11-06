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
      
      if (input$plot_type == "lineplot_mean_se") {
        plot <- plot_obj()              # existing lineplot logic
        if (is.null(plot)) return(NULL)
        plot
        
      } else if (input$plot_type == "barplot_mean_se") {
        posthoc_all <- compile_anova_results(info)$posthoc
        print(posthoc_all)
        plot_oneway_barplot_meanse(
          df(),
          info,
          layout_values = list(
            strata_rows = input$strata_rows,
            strata_cols = input$strata_cols,
            resp_rows   = input$resp_rows,
            resp_cols   = input$resp_cols
          ),
          line_colors = custom_colors(),
          posthoc_all = posthoc_all
        )
      }
      
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
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else NULL
  
  # --- layout controls from UI ---
  strata_rows <- suppressWarnings(as.numeric(layout_values$strata_rows))
  strata_cols <- suppressWarnings(as.numeric(layout_values$strata_cols))
  resp_rows   <- suppressWarnings(as.numeric(layout_values$resp_rows))
  resp_cols   <- suppressWarnings(as.numeric(layout_values$resp_cols))
  
  factor1 <- info$factors$factor1
  responses <- info$responses
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else NULL
  
  response_plots <- list()
  
  # ---- color consistent with lineplot ----
  fill_color <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }
  
  # ---- iterate over each response ----
  for (resp in responses) {
    
    if (has_strata && !is.null(strat_var) && strat_var %in% names(data)) {
      
      stratum_plots <- list()
      for (stratum in strata_levels) {
        subset_data <- data[data[[strat_var]] == stratum, , drop = FALSE]
        if (nrow(subset_data) == 0) next
        
        stats_df <- subset_data %>%
          dplyr::group_by(.data[[factor1]]) %>%
          dplyr::summarise(
            mean = mean(.data[[resp]], na.rm = TRUE),
            se   = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
            .groups = "drop"
          )
        
        # ✅ ensure order and matching with contrast group names
        stats_df[[factor1]] <- factor(stats_df[[factor1]], levels = unique(stats_df[[factor1]]))
        
        # ---- optional significance layer ----
        signif_df <- NULL
        if (!is.null(posthoc_all)) {
          if (has_strata && !is.null(strat_var) && strat_var %in% names(data)) {
            # stratified case
            if (!is.null(posthoc_all[[resp]]) && !is.null(posthoc_all[[resp]][[stratum]])) {
              signif_df <- extract_tukey_for_signif(posthoc_all[[resp]][[stratum]])
            }
          } else {
            # non-stratified case
            if (!is.null(posthoc_all[[resp]])) {
              signif_df <- extract_tukey_for_signif(posthoc_all[[resp]])
            }
          }
        }
        
        # ✅ Step 2: diagnostic prints
        cat("\n--- DEBUG for", resp, if (has_strata) paste0(" (", stratum, ")"), "---\n")
        cat("signif_df:\n"); print(signif_df)
        cat("stats_df:\n"); print(stats_df)
        cat("factor1 levels:\n"); print(levels(stats_df[[factor1]]))
        cat("-------------------------------------------\n")
        
        # ---- Base barplot ----
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
        
        # ---- Add significance brackets (if Tukey post-hoc is available) ----
        if (!is.null(signif_df) && nrow(signif_df) > 0) {
          signif_df <- signif_df %>%
            dplyr::filter(p.value < 0.05)   # ✅ keep only significant
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
      
      # combine stratum plots
      if (length(stratum_plots) > 0) {
        ncol <- if (!is.na(strata_cols) && strata_cols > 0) strata_cols else ceiling(sqrt(length(stratum_plots)))
        nrow <- if (!is.na(strata_rows) && strata_rows > 0) strata_rows else ceiling(length(stratum_plots) / ncol)
        combined <- patchwork::wrap_plots(plotlist = stratum_plots, ncol = ncol, nrow = nrow)

        
        title_plot <- ggplot() +
          theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }
      
    } else {
      # ---- no stratification ----
      stats_df <- data %>%
        dplyr::group_by(.data[[factor1]]) %>%
        dplyr::summarise(
          mean = mean(.data[[resp]], na.rm = TRUE),
          se   = sd(.data[[resp]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp]]))),
          .groups = "drop"
        )
      
      # ✅ ensure order and matching with contrast group names
      stats_df[[factor1]] <- factor(stats_df[[factor1]], levels = unique(stats_df[[factor1]]))
      
      # ---- extract Tukey significance ----
      signif_df <- NULL
      if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
        signif_df <- extract_tukey_for_signif(posthoc_all[[resp]])
      }
      
      # ---- Base barplot ----
      p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
        geom_col(fill = fill_color, width = 0.6, alpha = 0.8) +
        geom_errorbar(
          aes(ymin = mean - se, ymax = mean + se),
          width = 0.15, color = "gray40", linewidth = 0.5
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
      
      # ---- Add significance brackets (if Tukey post-hoc is available) ----
      if (!is.null(signif_df) && nrow(signif_df) > 0) {
        signif_df <- signif_df %>%
          dplyr::filter(p.value < 0.05)   # ✅ keep only significant
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
  
  # ---- arrange all responses ----
  if (length(response_plots) == 1) {
    response_plots[[1]]
  } else {
    ncol <- if (!is.na(resp_cols) && resp_cols > 0) resp_cols else ceiling(sqrt(length(response_plots)))
    nrow <- if (!is.na(resp_rows) && resp_rows > 0) resp_rows else ceiling(length(response_plots) / ncol)
    patchwork::wrap_plots(plotlist = response_plots, ncol = ncol, nrow = nrow)
    
  }
}



