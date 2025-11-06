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
      add_color_customization_ui(ns, multi_group = FALSE),
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


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
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
      layout_inputs <- list(
        strata_rows = input$strata_rows,
        strata_cols = input$strata_cols,
        resp_rows = input$resp_rows,
        resp_cols = input$resp_cols
      )

      build_anova_plot_info(
        data,
        info,
        layout_inputs,
        line_colors = custom_colors()
      )
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
      s <- plot_info()$layout
      list(
        w = input$plot_width * s$strata$cols * s$responses$ncol,
        h = input$plot_height * s$strata$rows * s$responses$nrow
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
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", HTML(info$warning))
      } else {
        NULL
      }
    })
    
    # ---- Render plot ----
    output$plot <- renderPlot({
      info <- model_info()
      req(info, input$plot_type)
      if (input$plot_type == "mean_se") {
        plot <- plot_obj()
        if (is.null(plot)) return(NULL)
        plot
      }
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    # ---- Download handler ----
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- plot_info()
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
