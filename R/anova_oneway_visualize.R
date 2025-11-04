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
      selectInput(
        ns("plot_type"),
        label = "Select visualization type:",
        choices = c("Mean ± SE" = "mean_se"),
        selected = "mean_se"
      ),
      hr(),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, numericInput(ns("plot_width"), "Subplot width (px)", value = 400, min = 200, max = 1200, step = 50)),
        column(6, numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50))
      ),
      hr(),
      add_color_customization_ui(ns, multi_group = FALSE),
      hr(),
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_ui"))
    )
  )
}


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
    layout_state <- initialize_layout_state(input, session)
    
    # ---- Plug in color customization module (single-color mode) ----
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    )
    
    # ---- Build plot info ----
    plot_info <- reactive({
      info <- model_info()
      req(info)
      validate(
        need(info$type == "oneway_anova", "No one-way ANOVA results available for plotting.")
      )
      data <- df()
      build_anova_plot_info(
        data,
        info,
        layout_state$effective_input,
        line_colors = custom_colors()
      )
    })
    
    observe_layout_synchronization(plot_info, layout_state, session)
    
    plot_obj <- reactive({
      info <- plot_info()
      req(info)
      layout <- info$layout
      strata_valid <- isTRUE(layout$strata$valid %||% TRUE)
      resp_valid <- isTRUE(layout$responses$valid %||% TRUE)
      if (!strata_valid || !resp_valid) return(NULL)
      info$plot
    })

    plot_size <- reactive({
      info <- plot_info()
      req(info)
      layout <- info$layout
      strata_cols <- layout$strata$cols %||% 1
      strata_rows <- layout$strata$rows %||% 1
      strata_valid <- isTRUE(layout$strata$valid %||% TRUE)
      resp_valid <- isTRUE(layout$responses$valid %||% TRUE)
      if (!strata_valid || !resp_valid) {
        list(w = input$plot_width, h = input$plot_height)
      } else {
        list(
          w = input$plot_width * strata_cols * layout$responses$ncol,
          h = input$plot_height * strata_rows * layout$responses$nrow
        )
      }
    })

    output$plot_ui <- renderUI({
      info <- plot_info()
      req(info)
      layout <- info$layout
      container <- function(content) {
        div(class = "ta-plot-container", content)
      }
      if (!isTRUE(layout$strata$valid %||% TRUE)) {
        return(container(div(class = "alert alert-warning", layout$strata$message)))
      }
      if (!isTRUE(layout$responses$valid %||% TRUE)) {
        return(container(div(class = "alert alert-warning", layout$responses$message)))
      }
      container(plotOutput(ns("plot"), height = "auto"))
    })

    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info, layout_state$default_ui_value)
    })
    
    # ---- Render plot ----
    output$plot <- renderPlot({
      info <- model_info()
      req(info, input$plot_type)
      if (input$plot_type == "mean_se") {
        plot_to_draw <- plot_obj()
        req(plot_to_draw)
        plot_to_draw
      }
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    # ---- Download handler ----
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        req(plot_obj())
        layout <- plot_info()$layout
        req(isTRUE(layout$strata$valid %||% TRUE))
        req(isTRUE(layout$responses$valid %||% TRUE))
        ggsave(
          filename = file,
          plot = plot_obj(),
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
