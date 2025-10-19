# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module 
# ===============================================================

source("R/module_visualize_helpers.R")

visualize_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize Outcomes"),
      p("Tweak the layout of the mean ± SE plots, download the figure, and wrap up your workflow."),
      hr(),
      uiOutput(ns("layout_controls")),
      hr(),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("subplot_width"),
            label = "Subplot width (px)",
            value = 300,
            min = 200,
            max = 1200,
            step = 50
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("subplot_height"),
            label = "Subplot height (px)",
            value = 200,
            min = 200,
            max = 1200,
            step = 50
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plots"))
    )
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    model_info <- reactive({
      model_fit()
    })

    active_analysis_type <- reactiveVal(NULL)

    observe({
      info <- model_info()
      if (is.null(info)) return()

      current_type <- if (!is.null(info$type)) info$type else "anova"
      if (!identical(active_analysis_type(), current_type)) {
        active_analysis_type(current_type)

        defaults <- if (identical(current_type, "ggpairs") || identical(current_type, "pca")) {
          list(width = 800, height = 600)
        } else {
          list(width = 300, height = 200)
        }

        updateNumericInput(session, "subplot_width", value = defaults$width)
        updateNumericInput(session, "subplot_height", value = defaults$height)
      }
    })
    
    # ---- Update labels when analysis type changes ----
    observe({
      type <- active_analysis_type()
      if (is.null(type)) return()
      
      if (identical(type, "ggpairs") || identical(type, "pca")) {
        updateNumericInput(session, "subplot_width", label = "Plot width (px)")
        updateNumericInput(session, "subplot_height", label = "Plot height (px)")
      } else {
        updateNumericInput(session, "subplot_width", label = "Subplot width (px)")
        updateNumericInput(session, "subplot_height", label = "Subplot height (px)")
      }
    })
    
    
    
    layout_overrides <- reactiveValues(
      strata_rows = 0,
      strata_cols = 0,
      resp_rows = 0,
      resp_cols = 0
    )
    
    layout_manual <- reactiveValues(
      strata_rows = FALSE,
      strata_cols = FALSE,
      resp_rows = FALSE,
      resp_cols = FALSE
    )
    
    suppress_updates <- reactiveValues(
      strata_rows = TRUE,
      strata_cols = TRUE,
      resp_rows = TRUE,
      resp_cols = TRUE
    )
    
    # DRY: generic watcher for the four numeric inputs
    observe_numeric_input <- function(name) {
      observeEvent(input[[name]], {
        if (isTRUE(suppress_updates[[name]])) {
          suppress_updates[[name]] <- FALSE
          return()
        }
        val <- suppressWarnings(as.numeric(input[[name]]))
        if (is.na(val) || val <= 0) {
          layout_overrides[[name]] <- 0L
          layout_manual[[name]] <- FALSE
        } else {
          layout_overrides[[name]] <- as.integer(val)
          layout_manual[[name]] <- TRUE
        }
      })
    }
    lapply(c("strata_rows", "strata_cols", "resp_rows", "resp_cols"), observe_numeric_input)
    
    effective_input <- function(name) {
      if (isTRUE(layout_manual[[name]])) layout_overrides[[name]] else 0
    }
    
    # Helper for default numeric value used in layout_controls
    default_ui_value <- function(cur_val) {
      val <- if (is.null(cur_val)) 1 else cur_val
      ifelse(is.na(val) || val <= 0, 1, val)
    }
    
    output$layout_controls <- renderUI({
      info <- model_info()
      if (is.null(info)) return(NULL)
      
      current_type <- if (!is.null(info$type)) info$type else "anova"
      
      if (identical(current_type, "ggpairs")) {
        return(build_anova_layout_controls(ns, input, info, default_ui_value))
      }
      
      if (identical(current_type, "ggpairs")) {
        return(build_ggpairs_layout_controls())
      }
      
      if (identical(current_type, "pca")) {
        data <- df()
        return(build_pca_layout_controls(ns, data))
      }
      
    })
    
    plot_obj_info <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$models) || length(info$models) == 0) {
        return(NULL)
      }
      
      data <- df()
      req(data)
      
      factor1 <- info$factors$factor1
      factor2 <- info$factors$factor2
      order1 <- info$orders$order1
      order2 <- info$orders$order2
      
      if (!is.null(factor1) && !is.null(order1)) {
        data[[factor1]] <- factor(data[[factor1]], levels = order1)
      }
      if (!is.null(factor2) && !is.null(order2)) {
        data[[factor2]] <- factor(data[[factor2]], levels = order2)
      }
      
      responses <- info$responses
      has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
      strat_var <- if (has_strata) info$strata$var else NULL
      strata_levels <- if (has_strata) info$strata$levels else character(0)
      if (has_strata && (is.null(strata_levels) || length(strata_levels) == 0)) {
        strata_levels <- unique(as.character(stats::na.omit(data[[strat_var]])))
      }
      
      response_plots <- list()
      max_strata_rows <- 1
      max_strata_cols <- 1
      
      compute_stats <- function(df_subset, resp_name) {
        if (is.null(factor2)) {
          df_subset |>
            dplyr::group_by(.data[[factor1]]) |>
            dplyr::summarise(
              mean = mean(.data[[resp_name]], na.rm = TRUE),
              se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
              .groups = "drop"
            )
        } else {
          df_subset |>
            dplyr::group_by(.data[[factor1]], .data[[factor2]]) |>
            dplyr::summarise(
              mean = mean(.data[[resp_name]], na.rm = TRUE),
              se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
              .groups = "drop"
            )
        }
      }
      
      build_plot <- function(stats_df, title_text, y_limits) {
        if (is.null(factor2)) {
          p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
            geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +
            geom_point(size = 3, color = "steelblue") +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                          width = 0.15, color = "gray40") +
            theme_minimal(base_size = 14) +
            labs(x = factor1, y = "Mean ± SE") +
            theme(
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank()
            )
        } else {
          p <- ggplot(stats_df, aes(
            x = !!sym(factor1),
            y = mean,
            color = !!sym(factor2),
            group = !!sym(factor2)
          )) +
            geom_line(linewidth = 1) +
            geom_point(size = 3) +
            geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                          width = 0.15) +
            theme_minimal(base_size = 14) +
            labs(
              x = factor1,
              y = "Mean ± SE",
              color = factor2
            ) +
            theme(
              panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank()
            )
        }
        
        if (!is.null(y_limits) && all(is.finite(y_limits))) {
          p <- p + scale_y_continuous(limits = y_limits)
        }
        
        p + ggtitle(title_text) +
          theme(plot.title = element_text(size = 12, face = "bold"))
      }
      
      for (resp in responses) {
        if (has_strata) {
          stratum_plots <- list()
          y_values <- c()
          
          for (stratum in strata_levels) {
            subset_data <- data[!is.na(data[[strat_var]]) & data[[strat_var]] == stratum, , drop = FALSE]
            if (nrow(subset_data) == 0) next
            
            stats_df <- compute_stats(subset_data, resp)
            if (nrow(stats_df) == 0) next
            
            y_values <- c(y_values, stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
            stratum_plots[[stratum]] <- stats_df
          }
          
          if (length(stratum_plots) == 0) next
          
          y_limits <- range(y_values, na.rm = TRUE)
          if (!all(is.finite(y_limits))) y_limits <- NULL
          
          strata_plot_list <- lapply(names(stratum_plots), function(stratum_name) {
            build_plot(stratum_plots[[stratum_name]], stratum_name, y_limits)
          })
          
          layout <- compute_layout(
            length(strata_plot_list),
            effective_input("strata_rows"),
            effective_input("strata_cols")
          )
          
          max_strata_rows <- max(max_strata_rows, layout$nrow)
          max_strata_cols <- max(max_strata_cols, layout$ncol)
          
          # First build the grid of strata plots
          combined <- patchwork::wrap_plots(
            plotlist = strata_plot_list,
            nrow = layout$nrow,
            ncol = layout$ncol
          )
          
          # Title above the grid (keep original layout)
          title_plot <- ggplot() +
            theme_void() +
            ggtitle(resp) +
            theme(
              plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
              plot.margin = margin(t = 0, r = 0, b = 6, l = 0)
            )
          
          # stack: title on top (small height), grid below (full height)
          response_plots[[resp]] <- title_plot / combined + plot_layout(heights = c(0.08, 1))
          
        } else {
          stats_df <- compute_stats(data, resp)
          if (nrow(stats_df) == 0) {
            next
          }
          
          y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
          y_limits <- range(y_values, na.rm = TRUE)
          if (!all(is.finite(y_limits))) {
            y_limits <- NULL
          }
          
          response_plots[[resp]] <- build_plot(stats_df, resp, y_limits)
          max_strata_rows <- max(max_strata_rows, 1)
          max_strata_cols <- max(max_strata_cols, 1)
        }
      }
      
      if (length(response_plots) == 0) {
        return(NULL)
      }
      
      resp_layout <- compute_layout(
        length(response_plots),
        effective_input("resp_rows"),
        effective_input("resp_cols")
      )
      
      final_plot <- if (length(response_plots) == 1) {
        response_plots[[1]]
      } else {
        # Preserve each response's patchwork title (exact original composition)
        patchwork::wrap_plots(
          plotlist = response_plots,
          nrow = resp_layout$nrow,
          ncol = resp_layout$ncol
        ) &
          patchwork::plot_layout(guides = "collect")
      }
      
      list(
        plot = final_plot,
        layout = list(
          strata = list(rows = max_strata_rows, cols = max_strata_cols),
          responses = resp_layout
        ),
        has_strata = has_strata,
        n_responses = length(response_plots)
      )
    })
    
    observeEvent(plot_obj_info(), {
      info <- plot_obj_info()
      if (is.null(info)) return()
      
      sync_input <- function(id, value, manual_key) {
        val <- ifelse(is.null(value) || value <= 0, 1, value)
        if (!isTRUE(layout_manual[[manual_key]])) {
          suppress_updates[[id]] <- TRUE
          updateNumericInput(session, id, value = val)
        }
      }
      
      if (isTRUE(info$has_strata)) {
        sync_input("strata_rows", info$layout$strata$rows, "strata_rows")
        sync_input("strata_cols", info$layout$strata$cols, "strata_cols")
      } else {
        sync_input("strata_rows", 1, "strata_rows")
        sync_input("strata_cols", 1, "strata_cols")
      }
      
      resp_rows_val <- if (info$n_responses <= 1) 1 else info$layout$responses$nrow
      resp_cols_val <- if (info$n_responses <= 1) 1 else info$layout$responses$ncol
      
      sync_input("resp_rows", resp_rows_val, "resp_rows")
      sync_input("resp_cols", resp_cols_val, "resp_cols")
    })
    
    plot_obj <- reactive({
      info <- plot_obj_info()
      if (is.null(info)) return(NULL)
      info$plot
    })
    
    # ---- Dynamic sizing logic (pixels) ----
    plot_size <- reactive({
      info <- plot_obj_info()
      w_sub <- input$subplot_width
      h_sub <- input$subplot_height
      
      if (is.null(w_sub) || is.na(w_sub)) w_sub <- 300
      if (is.null(h_sub) || is.na(h_sub)) h_sub <- 200
      
      if (is.null(info)) return(list(w = w_sub, h = h_sub))
      
      strata_cols <- info$layout$strata$cols
      strata_rows <- info$layout$strata$rows
      resp_cols <- info$layout$responses$ncol
      resp_rows <- info$layout$responses$nrow
      
      if (is.null(strata_cols) || strata_cols < 1) strata_cols <- 1
      if (is.null(strata_rows) || strata_rows < 1) strata_rows <- 1
      if (is.null(resp_cols) || resp_cols < 1) resp_cols <- 1
      if (is.null(resp_rows) || resp_rows < 1) resp_rows <- 1
      
      list(
        w = w_sub * strata_cols * resp_cols,
        h = h_sub * strata_rows * resp_rows
      )
    })
    
    # ---- Render Plot ----
    output$plots <- renderPlot({
      info <- model_info()
      req(info)
      
      if (!is.null(info$type)) {
        if (info$type == "ggpairs") {
          validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
          return(build_ggpairs_plot(info$data))
        }
        
        if (info$type == "pca") {
          validate(need(!is.null(info$model), "Run PCA first."))
          pca_obj <- info$model
          
          color_var <- if (!is.null(input$pca_color) && input$pca_color != "None") input$pca_color else NULL
          shape_var <- if (!is.null(input$pca_shape) && input$pca_shape != "None") input$pca_shape else NULL
          
          return(build_pca_biplot(pca_obj, info$data, color_var = color_var, shape_var = shape_var))
        }
        
      }
      
      req(plot_obj())
      plot_obj()
      
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    
    # ---- Download Plot ----
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- model_info()
        req(info)
        s <- plot_size()
        width_in  <- s$w / 96
        height_in <- s$h / 96
        
        # Decide which plot to build
        g <- NULL
        
        if (!is.null(info$type)) {
          if (info$type == "ggpairs") {
            validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
            g <- build_ggpairs_plot(info$data)
          } else if (info$type == "pca") {
            validate(need(!is.null(info$model), "Run PCA first."))
            pca_obj <- info$model
            
            # Include the color/shape options from inputs
            color_var <- if (!is.null(input$pca_color) && input$pca_color != "None") input$pca_color else NULL
            shape_var <- if (!is.null(input$pca_shape) && input$pca_shape != "None") input$pca_shape else NULL
            
            g <- build_pca_biplot(pca_obj, info$data, color_var = color_var, shape_var = shape_var)
          }
        }
        
        # Default (anova, lm, lmm, etc.)
        if (is.null(g)) {
          req(plot_obj())
          g <- plot_obj()
        }
        
        ggsave(
          filename = file,
          plot = g,
          device = "png",
          dpi = 300,
          width = width_in,
          height = height_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
    
    
  })
}
