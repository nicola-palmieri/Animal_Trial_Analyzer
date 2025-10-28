# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (GGpairs)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize pairwise correlation"),
      p("Visualize pairwise relationships and correlation coefficients among numeric variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Select visualization type:",
        choices = c("Pairwise correlation matrix" = "ggpairs"),  # ✅ single option for now
        selected = "ggpairs"
      ),
      hr(),
      uiOutput(ns("strata_selector")),
      hr(),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("plot_width"),
            label = "Plot width (px)",
            value = 800,
            min = 400,
            max = 2000,
            step = 100
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("plot_height"),
            label = "Plot height (px)",
            value = 600,
            min = 400,
            max = 2000,
            step = 100
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download plot")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plots"))
    )
  )
}


visualize_ggpairs_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive data and model
    model_info <- reactive(model_fit())

    stratified_results <- reactive({
      info <- model_info()
      if (is.null(info) || info$type != "pairwise_correlation") return(NULL)
      res_accessor <- info$results
      if (is.null(res_accessor) || !is.function(res_accessor)) return(NULL)
      res_accessor()
    })

    available_strata <- reactive({
      results <- stratified_results()
      if (is.null(results) || is.null(results$plots)) return(NULL)
      plots <- results$plots
      if (is.null(plots) || length(plots) == 0) return(NULL)
      nm <- names(plots)
      if (is.null(nm) || length(nm) == 0) {
        nm <- rep("Overall", length(plots))
        names(plots) <- nm
      }
      nm <- nm[nzchar(nm)]
      if (length(nm) == 0) return(NULL)
      nm
    })

    output$strata_selector <- renderUI({
      choices <- available_strata()
      if (is.null(choices)) return(NULL)
      selectInput(
        ns("selected_stratum"),
        label = "Select stratum:",
        choices = choices,
        selected = choices[[1]]
      )
    })

    # Determine plot size dynamically
    plot_size <- reactive({
      list(w = input$plot_width, h = input$plot_height)
    })

    # --- Main Plot Rendering ---
    output$plots <- renderPlot({
      info <- model_info()
      if (is.null(info) || info$type != "pairwise_correlation") return(NULL)
      results <- stratified_results()
      if (is.null(results) || is.null(results$plots) || length(results$plots) == 0) {
        validate(need(FALSE, "No correlation results available."))
      }

      # use visualization type (only one for now)
      if (input$plot_type == "ggpairs") {
        stratum <- input$selected_stratum
        plots <- results$plots
        if (is.null(stratum) || !stratum %in% names(plots)) {
          stratum <- names(plots)[[1]]
        }
        plot_obj <- plots[[stratum]]
        validate(need(!is.null(plot_obj), "Selected stratum has no data to plot."))
        print(plot_obj)
      }
    },
    width  = function() plot_size()$w,
    height = function() plot_size()$h,
    res    = 96)

    # --- Download handler ---
    output$download_plot <- downloadHandler(
      filename = function() paste0("ggpairs_plot_", Sys.Date(), ".png"),
      content = function(file) {
        info <- model_info()
        req(info)
        validate(need(info$type == "pairwise_correlation", "Pairwise correlation results are not available."))
        results <- stratified_results()
        req(results)
        plots <- results$plots
        req(plots)
        stratum <- input$selected_stratum
        if (is.null(stratum) || !stratum %in% names(plots)) {
          stratum <- names(plots)[[1]]
        }
        g <- plots[[stratum]]
        validate(need(!is.null(g), "Selected stratum has no data to plot."))

        width_in  <- input$plot_width / 96
        height_in <- input$plot_height / 96

        ggsave(
          filename = file,
          plot = g,
          device = "png",
          dpi = 300,
          width = width_in,
          height = height_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
