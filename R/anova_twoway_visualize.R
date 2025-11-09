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
        "Help: Pick the chart style you prefer for viewing group means and uncertainty."
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
          "Help: Turn on labels to display the mean value on each bar."
        )
      ),
      fluidRow(
        column(6, with_help_tooltip(
          numericInput(ns("plot_width"),  "Subplot width (px)",  value = 400, min = 200, max = 1200, step = 50),
          "Help: Set how wide each subplot should be in pixels."
        )),
        column(6, with_help_tooltip(
          numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50),
          "Help: Set how tall each subplot should be in pixels."
        ))
      ),
      add_color_customization_ui(ns, multi_group = TRUE),
      br(),
      with_help_tooltip(
        downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
        "Help: Save the current figure as an image file."
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

    last_plot_type <- reactiveVal("lineplot_mean_se")
    
    observeEvent(input$plot_type, {
      last_plot_type(input$plot_type)
    }, ignoreInit = TRUE)

    cached_results <- reactiveValues(plots = list())

    compute_empty_result <- function(message = NULL) {
      list(
        plot = NULL,
        warning = message,
        layout = NULL,
        defaults = NULL
      )
    }

    compute_all_plots <- function(data, info, layout_inputs, colors) {
      if (is.null(info)) {
        return(list())
      }

      if (!identical(info$type, "twoway_anova")) {
        msg <- "No two-way ANOVA results available for plotting."
        return(list(
          lineplot_mean_se = compute_empty_result(msg),
          barplot_mean_se = compute_empty_result(msg)
        ))
      }

      if (is.null(data) || (!is.null(data) && nrow(data) == 0)) {
        msg <- "No data available for plotting."
        return(list(
          lineplot_mean_se = compute_empty_result(msg),
          barplot_mean_se = compute_empty_result(msg)
        ))
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

      line_colors <- colors
      if (is.null(line_colors) || length(line_colors) == 0) {
        line_colors <- NULL
      }

      results <- list()

      results$lineplot_mean_se <- safe_plot(
        plot_anova_lineplot_meanse(
          data,
          info,
          layout_inputs,
          line_colors = line_colors
        )
      )

      posthoc_data <- tryCatch(
        compile_anova_results(info)$posthoc,
        error = function(e) NULL
      )

      results$barplot_mean_se <- safe_plot(
        plot_anova_barplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors = line_colors,
          posthoc_all = posthoc_data,
          show_value_labels = isTRUE(input$show_bar_labels)
        )
      )

      results
    }

    observeEvent(
      list(
        model_info(),
        df(),
        input$strata_rows,
        input$strata_cols,
        input$resp_rows,
        input$resp_cols,
        custom_colors(),
        input$show_bar_labels
      ),
      {
        info <- model_info()
        data <- df()
        layout_inputs <- list(
          strata_rows = input$strata_rows,
          strata_cols = input$strata_cols,
          resp_rows = input$resp_rows,
          resp_cols = input$resp_cols
        )

        cached_results$plots <- compute_all_plots(data, info, layout_inputs, custom_colors())
      },
      ignoreNULL = FALSE
    )

    results <- reactive(cached_results$plots)

    current_result <- reactive({
      res <- results()
      if (length(res) == 0) {
        return(NULL)
      }
      res[[input$plot_type]]
    })

    plot_obj <- reactive({
      info <- current_result()
      if (is.null(info) || !is.null(info$warning) || is.null(info$plot)) {
        return(NULL)
      }
      info$plot
    })

    plot_size <- reactive({
      res <- current_result()
      layout <- if (!is.null(res)) res$layout else NULL
      strata_layout <- if (!is.null(layout) && !is.null(layout$strata)) layout$strata else list()
      response_layout <- if (!is.null(layout) && !is.null(layout$responses)) layout$responses else list()
      strata_rows <- if (!is.null(strata_layout$rows)) strata_layout$rows else 1
      strata_cols <- if (!is.null(strata_layout$cols)) strata_layout$cols else 1
      resp_rows <- if (!is.null(response_layout$rows)) response_layout$rows else 1
      resp_cols <- if (!is.null(response_layout$cols)) response_layout$cols else 1
      list(
        w = input$plot_width * strata_cols * resp_cols,
        h = input$plot_height * strata_rows * resp_rows
      )
    })

    observeEvent(results(), {
      res_list <- results()
      if (length(res_list) == 0) {
        return()
      }

      first_valid <- NULL
      for (item in res_list) {
        if (!is.null(item$defaults) && !is.null(item$layout)) {
          first_valid <- item
          break
        }
      }

      if (is.null(first_valid)) {
        return()
      }

      defaults <- first_valid$defaults

      if (!is.null(defaults$strata)) {
        rows <- defaults$strata$rows
        cols <- defaults$strata$cols
        if (!is.null(rows) && !is.null(cols)) {
          sync_numeric_input(session, "strata_rows", input$strata_rows, rows)
          sync_numeric_input(session, "strata_cols", input$strata_cols, cols)
        }
      }

      if (!is.null(defaults$responses)) {
        rows <- defaults$responses$rows
        cols <- defaults$responses$cols
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
      info <- current_result()
      if (is.null(info)) {
        return(NULL)
      }
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", HTML(info$warning))
      } else {
        NULL
      }
    })

    output$plot <- renderPlot({
      plot <- plot_obj()
      if (is.null(plot)) return(NULL)
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
