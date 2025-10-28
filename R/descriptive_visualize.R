# ===============================================================
# Visualization Module — Descriptive Statistics (Dispatcher)
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 5 — Visualize Descriptive Statistics"),
      p("Explore distributions, variability, and normality across variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c(
          "Categorical Distributions" = "categorical",
          "Numeric Boxplots"          = "boxplots",
          "Numeric Histograms"        = "histograms",
          "CV (%)"                    = "cv",
          "Outlier Counts"            = "outliers",
          "Missingness (%)"           = "missing",
          "Shapiro–Wilk p-values"     = "shapiro"
        ),
        selected = "categorical"
      ),
      hr(),
      uiOutput(ns("sub_controls"))  # controls from active submodule
    ),
    mainPanel(
      width = 8,
      plotOutput(ns("plot"), height = "auto")  # parent-owned plotOutput
    )
  )
}


visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # holder for the active submodule's returned reactives
    active <- reactiveVal(NULL)
    
    # ==========================================================
    # 🔹 Inject the correct UI for each submodule
    # ==========================================================
    output$sub_controls <- renderUI({
      switch(input$plot_type,
             "categorical" = visualize_categorical_barplots_ui(ns("categorical")),
             "boxplots"    = visualize_numeric_boxplots_ui(ns("boxplots")),
             "histograms"  = h5("Histogram controls not yet implemented."),
             "cv"          = h5("CV controls not yet implemented."),
             "outliers"    = h5("Outlier controls not yet implemented."),
             "missing"     = h5("Missingness controls not yet implemented."),
             "shapiro"     = h5("Shapiro–Wilk controls not yet implemented.")
      )
    })
    
    # ==========================================================
    # 🔹 Dynamically start the right submodule server
    # ==========================================================
    observeEvent(input$plot_type, {
      type <- input$plot_type
      if (is.null(type) || length(type) == 0) return()
      
      handle <- switch(type[[1]],
                       "categorical" = visualize_categorical_barplots_server("categorical", filtered_data, descriptive_summary),
                       "boxplots"    = visualize_numeric_boxplots_server("boxplots", filtered_data, descriptive_summary),
                       NULL
      )
      active(handle)
    }, ignoreInit = FALSE)
    
    # ==========================================================
    # 🔹 Parent renders whatever the active submodule produces
    # ==========================================================
    output$plot <- renderPlot({
      h <- active()
      req(h)
      p <- h$plot()
      validate(need(!is.null(p), "No plot available."))
      print(p)
    },
    width  = function() { h <- active(); if (is.null(h)) 800 else h$width()  },
    height = function() { h <- active(); if (is.null(h)) 600 else h$height() },
    res = 96)
  })
}
