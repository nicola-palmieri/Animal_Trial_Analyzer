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
    hr(),
    fluidRow(
      column(
        6,
        numericInput(
          ns("n_rows"),
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
          ns("n_cols"),
          "Grid columns",
          value = 6,
          min = 1,
          max = 10,
          step = 1
        )
      )
    ),
    hr(),
    downloadButton(ns("download_plot"), "Download plot")
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
        nrow_input = input$n_rows,
        ncol_input = input$n_cols
      )

      validate(need(!is.null(out), "No numeric variables available for plotting."))
      out
    })

    plot_size <- reactive({
      req(module_active())
      info <- plot_info()
      if (is.null(info$layout)) {
        list(w = plot_width(), h = plot_height())
      } else {
        list(
          w = plot_width()  * info$layout$ncol,
          h = plot_height() * info$layout$nrow
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
      validate(need(!is.null(info$plot), "No plot available."))
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
