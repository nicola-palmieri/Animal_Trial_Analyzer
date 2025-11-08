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
        column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  value = 400, min = 200, max = 1200, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50))
      ),
      add_color_customization_ui(ns, multi_group = TRUE),
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
    restoring_plot_type <- reactiveVal(FALSE)

    observeEvent(model_info(), {
      restoring_plot_type(TRUE)
      selected <- isolate(last_plot_type())
      if (!is.null(selected)) {
        updateSelectInput(session, "plot_type", selected = selected)
      }
      session$onFlushed(function() {
        restoring_plot_type(FALSE)
      }, once = TRUE)
    }, ignoreNULL = FALSE)

    observeEvent(input$plot_type, {
      if (isTRUE(restoring_plot_type())) {
        return()
      }
      last_plot_type(input$plot_type)
    }, ignoreNULL = FALSE)

    plot_info <- reactive({
      info <- model_info()
      plot_type <- last_plot_type()
      req(info, plot_type)
      validate(
        need(info$type == "twoway_anova", "No two-way ANOVA results available for plotting.")
      )

      data <- df()
      layout_inputs <- list(
        strata_rows = input$strata_rows,
        strata_cols = input$strata_cols,
        resp_rows = input$resp_rows,
        resp_cols = input$resp_cols
      )

      line_colors <- custom_colors()
      if (is.null(line_colors) || length(line_colors) == 0) {
        line_colors <- NULL
      }

      if (identical(plot_type, "barplot_mean_se")) {
        posthoc_all <- compile_anova_results(info)$posthoc
        plot_anova_barplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors = line_colors,
          posthoc_all = posthoc_all
        )
      } else {
        plot_anova_lineplot_meanse(
          data,
          info,
          layout_inputs,
          line_colors = line_colors
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
