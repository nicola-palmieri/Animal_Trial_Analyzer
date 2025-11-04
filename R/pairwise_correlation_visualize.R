# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (Dispatcher)
# ===============================================================

visualize_ggpairs_ui <- function(id, selected_plot_type = NULL) {
  ns <- NS(id)
  plot_choices <- c("Pairwise scatterplot matrix" = "GGPairs")
  selected_value <- selected_plot_type
  if (is.null(selected_value) || !(selected_value %in% plot_choices)) {
    selected_value <- plot_choices[[1]]
  }
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize pairwise correlation"),
      p("Visualize pairwise relationships and correlation coefficients among numeric variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Select visualization type:",
        choices = plot_choices,
        selected = selected_value
      ),
      hr(),
      uiOutput(ns("sub_controls"))
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
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

    active <- reactiveVal(NULL)

    output$sub_controls <- renderUI({
      info <- correlation_info()
      if (is.null(info)) {
        helpText("Run the pairwise correlation analysis to configure plots.")
      } else if (identical(input$plot_type, "GGPairs")) {
        pairwise_correlation_visualize_ggpairs_ui(ns("ggpairs"))
      } else {
        NULL
      }
    })

    observeEvent(list(input$plot_type, correlation_info()), {
      info <- correlation_info()
      type <- input$plot_type

      if (is.null(info) || is.null(type)) {
        active(NULL)
        return()
      }

      handle <- switch(type,
                       "GGPairs" = pairwise_correlation_visualize_ggpairs_server("ggpairs", filtered_data, correlation_info),
                       NULL)
      active(handle)
    }, ignoreNULL = FALSE)

    output$plot <- renderPlot({
      h <- active()
      req(h)
      plot_obj <- h$plot()
      validate(need(!is.null(plot_obj), "No plot available."))
      print(plot_obj)
    },
    width = function() {
      h <- active()
      if (is.null(h)) 800 else h$width()
    },
    height = function() {
      h <- active()
      if (is.null(h)) 600 else h$height()
    },
    res = 96)
  })
}
