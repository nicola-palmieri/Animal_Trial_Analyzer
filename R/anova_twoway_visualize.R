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
        choices = c("Mean ± SE" = "mean_se"),
        selected = "mean_se"
      ),
      hr(),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, numericInput(ns("plot_width"), "Subplot width (px)",  value = 400, min = 200, max = 1200, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50))
      ),
      hr(),
      add_color_customization_ui(ns, multi_group = TRUE),
      hr(),
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plot"), height = "auto")   # ✅ same as one-way
    )
  )
}


visualize_twoway_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    model_info <- reactive(model_fit())
    
    layout_state <- initialize_layout_state(input, session)

    # ---- color customization ----
    color_var_reactive <- reactive({
      info <- model_info()
      if (is.null(info)) return(NULL)
      info$factors$factor2   # color lines by factor2
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )

    plot_info <- reactive({
      info <- model_info()
      if (is.null(info) || info$type != "twoway_anova") return(NULL)
      data <- df()
      line_colors <- custom_colors()
      if (is.null(line_colors) || length(line_colors) == 0) {
        line_colors <- NULL
      }
      build_anova_plot_info(
        data,
        info,
        layout_state$effective_input,
        line_colors = line_colors
      )
    })

    observe_layout_synchronization(plot_info, layout_state, session)
    
    plot_obj <- reactive({
      info <- plot_info()
      if (is.null(info)) return(NULL)
      info$plot
    })
    
    plot_size <- reactive({
      info <- plot_info()
      if (is.null(info)) return(list(w = input$plot_width, h = input$plot_height))
      s <- info$layout
      list(
        w = input$plot_width  * s$strata$cols   * s$responses$ncol,
        h = input$plot_height * s$strata$rows   * s$responses$nrow
      )
    })
    
    output$layout_controls <- renderUI({
      info <- model_info()
      if (is.null(info) || info$type != "twoway_anova") return(NULL)
      build_anova_layout_controls(ns, input, info, layout_state$default_ui_value)
    })
    
    # ✅ simpler, consistent naming and structure
    output$plot <- renderPlot({
      info <- model_info()
      req(info, input$plot_type)

      if (input$plot_type == "mean_se") {
        req(plot_obj())
        plot_obj()
      }
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0(input$plot_type, "_twoway_anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(plot_obj())
        s <- plot_size()
        ggsave(
          filename = file,
          plot = plot_obj(),
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
