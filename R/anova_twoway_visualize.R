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
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_twoway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    color_var_reactive <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$factors)) return(NULL)
      info$factors$factor2
    })
    
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )
    
    base_size <- base_size_server(input = input, default = 13)
    
    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")
    
    module_active <- reactive(TRUE)
    
    state <- reactive({
      list(
        data          = df(),
        info          = model_info(),
        strata_rows   = strata_grid$rows(),
        strata_cols   = strata_grid$cols(),
        resp_rows     = response_grid$rows(),
        resp_cols     = response_grid$cols(),
        colors        = custom_colors(),
        base_size     = base_size(),
        show_labels   = isTRUE(input$show_bar_labels),
        plot_type     = input$plot_type
      )
    })
    
    compute_all_plots <- function(data, info, layout_inputs, colors, base_size_value, show_labels) {
      if (is.null(info) || !identical(info$type, "twoway_anova") || is.null(data) || nrow(data) == 0) {
        return(list(
          lineplot_mean_se = list(plot = NULL, warning = "No data or results available.", layout = NULL),
          barplot_mean_se  = list(plot = NULL, warning = "No data or results available.", layout = NULL)
        ))
      }
      
      list(
        lineplot_mean_se = plot_anova_lineplot_meanse(
          data,
          info,
          layout_inputs,
          line_colors = colors,
          base_size = base_size_value
        ),
        barplot_mean_se = plot_anova_barplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors = colors,
          show_value_labels = show_labels,
          base_size = base_size_value,
          posthoc_all = info$posthoc
        )
      )
    }
    
    plot_info <- reactive({
      req(module_active())
      s <- state()
      req(!is.null(s$data), !is.null(s$info))
      layout_inputs <- list(
        strata_rows = s$strata_rows,
        strata_cols = s$strata_cols,
        resp_rows   = s$resp_rows,
        resp_cols   = s$resp_cols
      )
      res <- compute_all_plots(s$data, s$info, layout_inputs, s$colors, s$base_size, s$show_labels)
      res[[if (!is.null(s$plot_type) && s$plot_type %in% names(res)) s$plot_type else "lineplot_mean_se"]]
    })
    
    size_val <- reactiveVal(list(w = 400, h = 300))
    
    observeEvent(plot_info(), {
      req(module_active())
      info <- plot_info()
      layout <- info$layout
      if (is.null(layout)) {
        size_val(list(w = input$plot_width, h = input$plot_height))
      } else {
        strata <- layout$strata
        responses <- layout$responses
        nrow_l <- (strata$rows %||% 1L) * (responses$rows %||% 1L)
        ncol_l <- (strata$cols %||% 1L) * (responses$cols %||% 1L)
        size_val(list(
          w = input$plot_width  * ncol_l,
          h = input$plot_height * nrow_l
        ))
      }
    }, ignoreInit = FALSE)
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    output$plot_warning <- renderUI({
      info <- plot_info()
      if (!is.null(info$warning)) div(class = "alert alert-warning", HTML(info$warning)) else NULL
    })
    
    output$plot <- renderPlot({
      info <- plot_info()
      if (is.null(info$plot)) return(NULL)
      info$plot
    },
    width  = function() size_val()$w,
    height = function() size_val()$h,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- plot_info()
        req(info$plot, is.null(info$warning))
        s <- size_val()
        ggsave(
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
  })
}
