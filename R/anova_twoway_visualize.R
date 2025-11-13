# ===============================================================
# 🧪 Visualization Module — Two-way ANOVA (Simplified & Consistent)
# ===============================================================

visualize_twoway_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize two-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Lineplots (mean ± SE)" = "lineplot_mean_se",
            "Barplots (mean ± SE)"  = "barplot_mean_se"
          ),
          selected = "lineplot_mean_se"
        ),
        "Pick the chart style you prefer for viewing group means and uncertainty."
      ),
      uiOutput(ns("layout_controls")),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'barplot_mean_se'", ns("plot_type")),
        with_help_tooltip(
          checkboxInput(
            ns("show_bar_labels"),
            "Show value labels on bars",
            value = FALSE
          ),
          "Turn on labels to display the mean value on each bar."
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(6, with_help_tooltip(
            checkboxInput(
              ns("lineplot_show_lines"),
              "Connect means with lines",
              value = FALSE
            ),
            "Draw connecting lines between group means."
          )),
          column(6, with_help_tooltip(
            checkboxInput(
              ns("lineplot_show_jitter"),
              "Overlay jittered data",
              value = FALSE
            ),
            "Overlay raw observations with light jitter for context."
          ))
        )
      ),
      fluidRow(
        column(6, with_help_tooltip(
          numericInput(ns("plot_width"),  "Subplot width (px)",  value = 400, min = 200, max = 1200, step = 50),
          "Set how wide each subplot should be in pixels."
        )),
        column(6, with_help_tooltip(
          numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50),
          "Set how tall each subplot should be in pixels."
        ))
      ),
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = TRUE)),
        column(6, base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for the ANOVA plots."
        ))
      ),
      br(),
      with_help_tooltip(
        downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
        "Save the current figure as an image file."
      )
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      # Pre-mounted panels for instant switching
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        plotOutput(ns("plot_line"), height = "auto")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'barplot_mean_se'", ns("plot_type")),
        plotOutput(ns("plot_bar"), height = "auto")
      )
    )
  )
}


visualize_twoway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
    
    color_var <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$factors)) return(NULL)
      info$factors$factor2
    })
    
    factor2_levels <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$orders)) return(NULL)
      info$orders$order2
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, df,
      color_var_reactive = color_var,
      multi_group = TRUE,
      level_order_reactive = factor2_levels
    )
    
    base_size <- base_size_server(input, default = 13)
    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")
    
    active <- reactive(TRUE)
    
    state <- reactive({
      list(
        data        = df(),
        info        = model_info(),
        strata_rows = strata_grid$rows(),
        strata_cols = strata_grid$cols(),
        resp_rows   = response_grid$rows(),
        resp_cols   = response_grid$cols(),
        colors      = custom_colors(),
        base_size   = base_size(),
        show_labels = isTRUE(input$show_bar_labels),
        show_lines  = isTRUE(input$lineplot_show_lines),
        show_jitter = isTRUE(input$lineplot_show_jitter),
        plot_type   = input$plot_type
      )
    })

    compute_all_plots <- function(data,
                                  info,
                                  layout_inputs,
                                  colors,
                                  base_size_value,
                                  show_labels,
                                  show_lines,
                                  show_jitter) {
      if (is.null(info) || !identical(info$type, "twoway_anova") || is.null(data) || nrow(data) == 0) {
        return(list(
          lineplot_mean_se = list(plot = NULL, warning = "No data or results available.", layout = NULL),
          barplot_mean_se  = list(plot = NULL, warning = "No data or results available.", layout = NULL)
        ))
      }
      list(
        lineplot_mean_se = plot_anova_lineplot_meanse(
          data, info, layout_inputs,
          line_colors = colors,
          base_size = base_size_value,
          show_lines = show_lines,
          show_jitter = show_jitter
        ),
        barplot_mean_se = plot_anova_barplot_meanse(
          data, info, layout_values = layout_inputs,
          line_colors = colors,
          show_value_labels = show_labels,
          base_size = base_size_value,
          posthoc_all = info$posthoc
        )
      )
    }
    
    plot_info <- reactive({
      s <- state()
      req(!is.null(s$data), !is.null(s$info))
      layout_inputs <- list(
        strata_rows = s$strata_rows,
        strata_cols = s$strata_cols,
        resp_rows   = s$resp_rows,
        resp_cols   = s$resp_cols
      )
      res <- compute_all_plots(
        s$data, s$info, layout_inputs,
        s$colors, s$base_size,
        s$show_labels,
        s$show_lines,
        s$show_jitter
      )
      res[[if (!is.null(s$plot_type) && s$plot_type %in% names(res)) s$plot_type else "lineplot_mean_se"]]
    })
    
    # ---- Cached ggplot object to avoid flicker ----
    if (!requireNamespace("digest", quietly = TRUE)) stop("Please install the 'digest' package.")
    hash_key <- function(data) {
      if (is.null(data) || !is.data.frame(data)) return("no-data")
      digest::digest(data, algo = "xxhash64")
    }
    
    cached_plot <- reactiveVal(NULL)
    cached_key  <- reactiveVal(NULL)
    
    observe({
      s <- state()
      dat <- s$data
      key <- paste(
        hash_key(dat),
        s$plot_type,
        s$show_labels,
        s$show_lines,
        s$show_jitter,
        s$colors,
        s$base_size,
        sep = "_"
      )
      if (!identical(key, cached_key())) {
        info <- plot_info()
        if (!is.null(info$plot)) {
          cached_plot(info$plot)
          cached_key(key)
        }
      }
    })
    
    plot_dimensions <- reactive({
      info <- plot_info()
      lay <- info$layout
      nrow_l <- (lay$strata$rows %||% 1L) * (lay$responses$rows %||% 1L)
      ncol_l <- (lay$strata$cols %||% 1L) * (lay$responses$cols %||% 1L)
      list(
        width = max(200, as.numeric(input$plot_width  %||% 400) * ncol_l),
        height = max(200, as.numeric(input$plot_height %||% 300) * nrow_l)
      )
    })
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    output$plot_warning <- renderUI({
      info <- plot_info()
      if (!is.null(info$warning))
        div(class = "alert alert-warning", HTML(info$warning))
    })
    
    output$plot_line <- renderPlot({
      p <- cached_plot()
      if (is.null(p) || input$plot_type != "lineplot_mean_se") return(NULL)
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    output$plot_bar <- renderPlot({
      p <- cached_plot()
      if (is.null(p) || input$plot_type != "barplot_mean_se") return(NULL)
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- cached_plot()
        req(!is.null(p))
        s <- plot_dimensions()
        ggsave(
          file, p, device = "png", dpi = 300,
          width = s$width / 96, height = s$height / 96,
          units = "in", limitsize = FALSE
        )
      }
    )
    
    outputOptions(output, "plot_line", suspendWhenHidden = TRUE)
    outputOptions(output, "plot_bar",  suspendWhenHidden = TRUE)
    
  })
}

