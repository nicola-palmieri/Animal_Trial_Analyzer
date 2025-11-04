# ===============================================================
# 🟦 Descriptive Visualization — Numeric Boxplots
# ===============================================================

visualize_numeric_boxplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("show_points"), "Show individual data points", TRUE),
    fluidRow(
      column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  200, 200, 2000, 50)),
      column(6, numericInput(ns("plot_height"), "Subplot height (px)", 800, 200, 2000, 50))
    ),
    fluidRow(
      column(
        6,
        numericInput(
          ns("resp_rows"),
          "Grid rows",
          value = 1,
          min = 1,
          max = 10,
          step = 1
        )
      ),
      column(
        6,
        numericInput(
          ns("resp_cols"),
          "Grid columns",
          value = 6,
          min = 1,
          max = 10,
          step = 1
        )
      )
    ),
    hr(),
    downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
  )
}


visualize_numeric_boxplots_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_numeric_boxplots_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {

    layout_state <- initialize_layout_state(input, session)

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

    plot_width <- reactive({
      w <- input$plot_width
      if (is.null(w) || !is.numeric(w) || is.na(w)) 400 else w
    })

    plot_height <- reactive({
      h <- input$plot_height
      if (is.null(h) || !is.numeric(h) || is.na(h)) 300 else h
    })

    plot_info <- reactive({
      req(module_active())

      info <- summary_info()

      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_input_value(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_input_value(info$selected_vars)
      group_var     <- resolve_input_value(info$group_var)
      
      out <- build_descriptive_numeric_boxplot(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        show_points = isTRUE(input$show_points),
        nrow_input = layout_state$effective_input("resp_rows"),
        ncol_input = layout_state$effective_input("resp_cols")
      )

      validate(need(!is.null(out), "No numeric variables available for plotting."))
      sync_grid_controls(layout_state, input, session, "resp_rows", "resp_cols", out$layout)
      out
    })

    observe_layout_synchronization(plot_info, layout_state, session)

    plot_size <- reactive({
      req(module_active())
      info <- plot_info()
      layout <- info$layout
      if (is.null(layout)) {
        list(w = plot_width(), h = plot_height())
      } else {
        list(
          w = plot_width()  * layout$ncol,
          h = plot_height() * layout$nrow
        )
      }
    })
    
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_boxplots_", Sys.Date(), ".png"),
      content  = function(file) {
        req(module_active())
        info <- plot_info()
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


build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = TRUE,
                                              nrow_input = NULL,
                                              ncol_input = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  
  num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)
  
  # ensure discrete x if grouped
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.factor(df[[group_var]])
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(num_vars, function(var) {
    # skip all-NA vars early
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)
    
    if (!is.null(group_var)) {
      group_levels <- levels(df[[group_var]])
      palette <- resolve_palette_for_levels(group_levels)
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +
        scale_fill_manual(values = palette) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      if (isTRUE(show_points)) {
        p <- p +
          geom_jitter(aes(color = .data[[group_var]]), width = 0.2, alpha = 0.5, size = 1) +
          scale_color_manual(values = palette, guide = "none")
      }
    } else {
      # ✅ always provide an x aesthetic
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = resolve_single_color(), width = 0.3) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(color = resolve_single_color(), width = 0.05, alpha = 0.5, size = 1)
      }
    }
    
    if (inherits(p, "gg")) p else NULL
  })
  
  # keep only valid ggplots
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  layout <- resolve_grid_layout(
    n_items = length(plots),
    rows_input = suppressWarnings(as.numeric(nrow_input)),
    cols_input = suppressWarnings(as.numeric(ncol_input))
  )
  
  combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol) +
    patchwork::plot_annotation(
      theme = theme(plot.title = element_text(size = 16, face = "bold"))
    )
  
  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = length(plots)
  )
}