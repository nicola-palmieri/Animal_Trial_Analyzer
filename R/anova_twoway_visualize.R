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
          default = 14,
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

    # ---- Plug in color customization module (multi-color mode) ----
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

    base_size <- base_size_server(
      input = input,
      default = 14
    )

    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")

    cached_results <- reactiveValues(plots = list())

    plot_types <- c("lineplot_mean_se", "barplot_mean_se")

    compute_empty_result <- function(message = NULL) {
      list(
        plot = NULL,
        warning = message,
        layout = NULL,
        defaults = NULL
      )
    }

    empty_results <- function(message) {
      setNames(rep(list(compute_empty_result(message)), length(plot_types)), plot_types)
    }

    extract_or <- function(x, name, default) {
      if (is.null(x)) return(default)
      value <- x[[name]]
      if (is.null(value)) default else value
    }

    compute_all_plots <- function(data, info, layout_inputs, colors, base_size_value) {
      if (is.null(info)) {
        return(list())
      }

      if (!identical(info$type, "twoway_anova")) {
        return(empty_results("No two-way ANOVA results available for plotting."))
      }

      if (is.null(data) || nrow(data) == 0) {
        return(empty_results("No data available for plotting."))
      }

      safe_plot <- function(expr) {
        tryCatch({
          result <- expr
          if (is.null(result)) {
            compute_empty_result("Unable to generate plot.")
          } else {
            result
          }
        }, error = function(e) {
          compute_empty_result(e$message)
        })
      }

      line_colors <- if (length(colors) == 0) NULL else colors

      list(
        lineplot_mean_se = safe_plot(
          plot_anova_lineplot_meanse(
            data,
            info,
            layout_inputs,
            line_colors = line_colors,
            base_size = base_size_value
          )
        ),
        barplot_mean_se = safe_plot(
          plot_anova_barplot_meanse(
            data,
            info,
            layout_values = layout_inputs,
            line_colors = line_colors,
            show_value_labels = isTRUE(input$show_bar_labels),
            base_size = base_size_value,
            posthoc_all = info$posthoc
          )
        )
      )
    }

    observeEvent(
      list(
        model_info(),
        df(),
        strata_grid$values(),
        response_grid$values(),
        custom_colors(),
        base_size(),
        input$show_bar_labels
      ),
      {
        info <- model_info()
        data <- df()
        layout_inputs <- list(
          strata_rows = strata_grid$rows(),
          strata_cols = strata_grid$cols(),
          resp_rows = response_grid$rows(),
          resp_cols = response_grid$cols()
        )

        cached_results$plots <- compute_all_plots(
          data,
          info,
          layout_inputs,
          custom_colors(),
          base_size()
        )
      },
      ignoreNULL = FALSE
    )

    results <- reactive(cached_results$plots)

    current_result <- reactive({
      res <- results()
      if (!length(res)) return(NULL)
      res[[input$plot_type]]
    })

    plot_obj <- reactive({
      info <- current_result()
      if (is.null(info) || !is.null(info$warning)) return(NULL)
      info$plot
    })

    plot_size <- reactive({
      res <- current_result()
      layout <- if (is.null(res)) list() else extract_or(res, "layout", list())
      strata_layout <- extract_or(layout, "strata", list(rows = 1, cols = 1))
      response_layout <- extract_or(layout, "responses", list(rows = 1, cols = 1))
      strata_rows <- extract_or(strata_layout, "rows", 1)
      strata_cols <- extract_or(strata_layout, "cols", 1)
      resp_rows <- extract_or(response_layout, "rows", 1)
      resp_cols <- extract_or(response_layout, "cols", 1)
      list(
        w = input$plot_width * strata_cols * resp_cols,
        h = input$plot_height * strata_rows * resp_rows
      )
    })

    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })

    output$plot_warning <- renderUI({
      info <- current_result()
      if (is.null(info) || is.null(info$warning)) return(NULL)
      div(class = "alert alert-warning", HTML(info$warning))
    })

    output$plot <- renderPlot({
      plot <- plot_obj()
      req(plot)
      plot
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- current_result()
        req(info)
        req(is.null(info$warning))
        plot <- plot_obj()
        req(plot)
        size <- plot_size()
        ggsave(
          filename = file,
          plot = plot,
          device = "png",
          dpi = 300,
          width = size$w / 96,
          height = size$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
