# ===============================================================
# Visualization Module — Descriptive Statistics (Dispatcher)
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 5 — Visualize descriptive statistics"),
      p("Explore distributions, variability, and normality across variables."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Categorical barplots" = "categorical",
            "Numeric boxplots"          = "boxplots",
            "Numeric histograms"        = "histograms",
            "CV (%)"                    = "cv",
            "Outlier counts"            = "outliers",
            "Missingness (%)"           = "missing"
          ),
          selected = "categorical"
        ),
        "Choose the descriptive chart that best answers your question."
      ),
      uiOutput(ns("sub_controls"))  # controls from active submodule
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_ui"))  # plot output provided by the active submodule
    )
  )
}


visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    active_type <- reactive({
      type <- input$plot_type
      if (is.null(type) || !nzchar(type[1])) "categorical" else type[1]
    })
    

    # ==========================================================
    # 🔹 Inject the correct UI for each submodule
    # ==========================================================
    output$sub_controls <- renderUI({
      type <- active_type()
      switch(type,
             "categorical" = visualize_categorical_barplots_ui(ns("categorical")),
             "boxplots"    = visualize_numeric_boxplots_ui(ns("boxplots")),
             "histograms"  = visualize_numeric_histograms_ui(ns("histograms")),
             "cv"          = visualize_cv_ui(ns("cv")),
             "outliers"    = visualize_outliers_ui(ns("outliers")),
             "missing"     = visualize_missing_ui(ns("missing")),
             div("No controls available for this plot type.")
      )
    })

    output$plot_ui <- renderUI({
      type <- active_type()
      switch(type,
             "categorical" = visualize_categorical_barplots_plot_ui(ns("categorical")),
             "boxplots"    = visualize_numeric_boxplots_plot_ui(ns("boxplots")),
             "histograms"  = visualize_numeric_histograms_plot_ui(ns("histograms")),
             "cv"          = visualize_cv_plot_ui(ns("cv")),
             "outliers"    = visualize_outliers_plot_ui(ns("outliers")),
             "missing"     = visualize_missing_plot_ui(ns("missing")),
             div("Plot not available for this selection.")
      )
    })

    categorical_active <- reactive(active_type() == "categorical")
    boxplots_active    <- reactive(active_type() == "boxplots")
    histograms_active  <- reactive(active_type() == "histograms")
    cv_active          <- reactive(active_type() == "cv")
    outliers_active    <- reactive(active_type() == "outliers")
    missing_active     <- reactive(active_type() == "missing")

    visualize_categorical_barplots_server(
      "categorical",
      filtered_data,
      descriptive_summary,
      is_active = categorical_active
    )
    visualize_numeric_boxplots_server(
      "boxplots",
      filtered_data,
      descriptive_summary,
      is_active = boxplots_active
    )
    visualize_numeric_histograms_server(
      "histograms",
      filtered_data,
      descriptive_summary,
      is_active = histograms_active
    )
    visualize_cv_server(
      "cv",
      filtered_data,
      descriptive_summary,
      is_active = cv_active
    )
    visualize_outliers_server(
      "outliers",
      filtered_data,
      descriptive_summary,
      is_active = outliers_active
    )
    visualize_missing_server(
      "missing",
      filtered_data,
      descriptive_summary,
      is_active = missing_active
    )
  })
}
