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
    
    plot_obj <- reactive({
      dat <- filtered_data()
      info <- summary_info()
      
      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))
      validate(need(!is.null(info), "Summary not available."))
      
      selected_vars <- resolve_input_value(info$selected_vars)
      group_var <- resolve_input_value(info$group_var)
      
      plot_info <- build_descriptive_categorical_plot(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        show_proportions = isTRUE(input$show_proportions)
      )
      
      validate(need(!is.null(plot_info), "No categorical variables available for plotting."))
      plot_info
    })
    
    plot_size <- reactive({
      info <- plot_obj()
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
        info <- plot_obj()
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
    
    # 🔹 Return the adjusted sizes
    return(list(
      plot   = reactive({ plot_obj()$plot }),
      width  = reactive(plot_size()$w),
      height = reactive(plot_size()$h)
    ))
  })
}

