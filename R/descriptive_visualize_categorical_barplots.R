# ===============================================================
# 🟦 Descriptive Visualization — Categorical Barplots
# ===============================================================

visualize_categorical_barplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("show_proportions"), "Show proportions instead of counts", FALSE),
    fluidRow(
      column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  400, 200, 2000, 50)),
      column(6, numericInput(ns("plot_height"), "Subplot height (px)", 300, 200, 2000, 50))
    ),
    hr(),
    fluidRow(
      column(
        6,
        numericInput(
          ns("n_rows"),
          "Grid rows",
          value = 3,
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
          value = 2,
          min = 1,
          max = 10,
          step = 1
        )
      )
    ),
    hr(),
    downloadButton(ns("download_plot"), "Download Plot")
  )
}

visualize_categorical_barplots_server <- function(id, filtered_data, summary_info) {
  moduleServer(id, function(input, output, session) {
    
    resolve_input_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x)) x() else x
    }
    
    plot_width <- reactive({
      w <- input$plot_width
      if (is.null(w) || !is.numeric(w) || is.na(w)) 400 else w
    })
    
    plot_height <- reactive({
      h <- input$plot_height
      if (is.null(h) || !is.numeric(h) || is.na(h)) 300 else h
    })
    
    plot_info <- reactive({
      info <- summary_info()

      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_input_value(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_input_value(info$selected_vars)
      group_var     <- resolve_input_value(info$group_var)
      strata_levels <- resolve_input_value(info$strata_levels)

      out <- build_descriptive_categorical_plot(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        strata_levels = strata_levels,
        show_proportions = isTRUE(input$show_proportions),
        nrow_input = input$n_rows,
        ncol_input = input$n_cols
      )
      validate(need(!is.null(out), "No categorical variables available for plotting."))
      out
    })
    
    plot_size <- reactive({
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
      filename = function() paste0("categorical_barplots_", Sys.Date(), ".png"),
      content  = function(file) {
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
    
    return(list(
      plot   = reactive({ plot_info()$plot }),
      width  = reactive(plot_size()$w),
      height = reactive(plot_size()$h)
    ))
  })
}
