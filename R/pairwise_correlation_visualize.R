# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (Dispatcher)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize pairwise correlation"),
      p("Visualize pairwise relationships and correlation coefficients among numeric variables."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c("Pairwise scatterplot matrix" = "GGPairs"),
          selected = "GGPairs"
        ),
        "Choose how to visualise the pairwise relationships between variables."
      ),
      uiOutput(ns("sub_controls"))
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_ggpairs_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    correlation_info <- reactive({
      info <- model_fit()
      if (is.null(info) || is.null(info$type) || info$type != "pairs") {
        return(NULL)
      }
      info
    })

    ggpairs_handle <- pairwise_correlation_visualize_ggpairs_server(
      "ggpairs",
      filtered_data,
      correlation_info
    )

    active_handle <- reactive({
      info <- correlation_info()
      type <- input$plot_type

      if (is.null(info) || is.null(type)) {
        return(NULL)
      }

      switch(type,
             "GGPairs" = ggpairs_handle,
             NULL)
    })

    output$sub_controls <- renderUI({
      info <- correlation_info()
      if (is.null(info)) {
        helpText("Run the pairwise correlation analysis to configure plots.")
      } else if (identical(input$plot_type, "GGPairs")) {
        pairwise_correlation_visualize_ggpairs_ui(ns("ggpairs"))
      }
    })

    output$plot_warning <- renderUI({
      handle <- active_handle()
      if (is.null(handle)) return(NULL)

      warning_text <- handle$warning()
      if (!is.null(warning_text)) {
        div(class = "alert alert-warning", HTML(warning_text))
      }
    })

    output$plot <- renderPlot({
      handle <- active_handle()
      req(handle)

      warning_text <- handle$warning()
      if (!is.null(warning_text)) return(NULL)

      plot_obj <- handle$plot()
      validate(need(!is.null(plot_obj), "No plot available."))
      print(plot_obj)
    },
    width = function() {
      handle <- active_handle()
      if (is.null(handle)) 800 else handle$width()
    },
    height = function() {
      handle <- active_handle()
      if (is.null(handle)) 600 else handle$height()
    },
    res = 96)
  })
}
