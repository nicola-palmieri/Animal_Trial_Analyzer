# ===============================================================
# 🟦 Descriptive Visualization — Categorical Barplots
# ===============================================================

visualize_categorical_barplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("show_proportions"), "Show proportions instead of counts", FALSE),
    fluidRow(
      column(
        6,
        numericInput(ns("plot_width"),  "Subplot width (px)",  400, 200, 2000, 50)
      ),
      column(
        6,
        numericInput(ns("plot_height"), "Subplot height (px)", 300, 200, 2000, 50)
      )
    ),
    downloadButton(ns("download_plot"), "Download Plot")
  )
}

visualize_categorical_barplots_server <- function(id, filtered_data, summary_info) {
  moduleServer(id, function(input, output, session) {
    
    # Build using your existing central builder
    # -> build_descriptive_plots(summary_data, data_subset, layout_overrides)
    obj <- reactive({
      dat <- filtered_data()
      info <- summary_info()
      validate(need(!is.null(dat) && is.data.frame(dat), "No data available."))
      validate(need(!is.null(info), "Summary not available."))
      
      summary_data <- if (is.reactive(info$summary)) info$summary() else info$summary
      validate(need(!is.null(summary_data), "Summary table missing."))
      
      # layout_overrides can be extended later; for now let builder decide
      all_plots <- build_descriptive_plots(info, dat)
      
      validate(need(!is.null(all_plots$factors), "No categorical plots available."))
      all_plots$factors  # this is a list with $plot, $layout, $panels
    })
    
    # Download uses the same plot
    output$download_plot <- downloadHandler(
      filename = function() paste0("categorical_barplots_", Sys.Date(), ".png"),
      content  = function(file) {
        p <- obj()
        ggsave(
          filename = file,
          plot = p$plot,
          device = "png",
          dpi = 300,
          width  = (input$plot_width  %||% 400) / 96,
          height = (input$plot_height %||% 300) / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
    
    # 👉 Return reactives to the parent (so the parent can render)
    return(list(
      plot   = reactive({ obj()$plot }),
      width  = reactive({ input$plot_width  %||% 400 }),
      height = reactive({ input$plot_height %||% 300 })
    ))
  })
}
