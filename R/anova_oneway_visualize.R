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
        "Pick the chart style you prefer for comparing group means and error bars."
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
          "Turn on labels to display the mean value on top of each bar."
        )
      ),
      fluidRow(
        column(6, with_help_tooltip(
          numericInput(ns("plot_width"), "Subplot width (px)", value = 400, min = 200, max = 1200, step = 50),
          "Adjust how wide each subplot should be in pixels."
        )),
        column(6, with_help_tooltip(
          numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50),
          "Adjust how tall each subplot should be in pixels."
        ))
      ),
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = FALSE)),
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


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- reactive(filtered_data())

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    )

    base_size <- base_size_server(
      input = input,
      default = 14
    )

    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")

    cached_results <- reactiveValues(plots = list())

    compute_empty_result <- function(message = NULL) {
      list(
        plot = NULL,
        warning = message,
        layout = NULL,
        defaults = NULL
      )
    }

    empty_plot_results <- function(message) {
      list(
        lineplot_mean_se = compute_empty_result(message),
        barplot_mean_se = compute_empty_result(message)
      )
    }

    compute_all_plots <- function(data, info, layout_inputs, colors, base_size_value) {
      if (is.null(info)) {
        return(list())
      }
      if (!identical(info$type, "oneway_anova")) {
        return(empty_plot_results("No one-way ANOVA results available for plotting."))
      }

      if (is.null(data) || nrow(data) == 0) {
        return(empty_plot_results("No data available for plotting."))
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

      results <- list()

      results$lineplot_mean_se <- safe_plot(
        plot_anova_lineplot_meanse(
          data,
          info,
          layout_inputs,
          line_colors = colors,
          base_size = base_size_value
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
          line_colors = colors,
          posthoc_all = posthoc_data,
          show_value_labels = isTRUE(input$show_bar_labels),
          base_size = base_size_value
        )
      )

      results
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
      res[[input$plot_type]]
    })

    plot_obj <- reactive({
      result <- current_result()
      if (is.null(result) || !is.null(result$warning)) {
        return(NULL)
      }
      result$plot
    })

    first_non_null <- function(x, default) if (is.null(x)) default else x

    plot_size <- reactive({
      layout <- first_non_null(current_result()$layout, list())
      strata <- first_non_null(layout$strata, list(rows = 1, cols = 1))
      responses <- first_non_null(layout$responses, list(rows = 1, cols = 1))

      list(
        w = input$plot_width * first_non_null(strata$cols, 1) * first_non_null(responses$cols, 1),
        h = input$plot_height * first_non_null(strata$rows, 1) * first_non_null(responses$rows, 1)
      )
    })

    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })

    output$plot_warning <- renderUI({
      info <- current_result()
      if (is.null(info) || is.null(info$warning)) {
        return(NULL)
      }
      div(class = "alert alert-warning", HTML(info$warning))
    })

    output$plot <- renderPlot(
      {
        plot <- plot_obj()
        if (is.null(plot)) {
          return(NULL)
        }
        plot
      },
      width = function() plot_size()$w,
      height = function() plot_size()$h,
      res = 96
    )

    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        res <- current_result()
        req(res)
        req(is.null(res$warning))
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



