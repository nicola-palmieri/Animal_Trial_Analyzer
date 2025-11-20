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
            "Numeric boxplots"     = "boxplots",
            "Numeric histograms"   = "histograms",
            "CV (%)"               = "cv",
            "Outlier counts"       = "outliers",
            "Missingness (%)"      = "missing"
          ),
          selected = "categorical"
        ),
        "Choose the descriptive chart that best answers your question."
      ),
      div(
        class = "mb-3",
        actionButton(ns("apply_plots"), "Apply changes", width = "100%")
      ),
      uiOutput(ns("sub_controls"))
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_ui"))
    )
  )
}



visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    applied_type <- reactiveVal("categorical")

    observeEvent(input$apply_plots, {
      req(input$plot_type)
      applied_type(input$plot_type)
    }, ignoreInit = FALSE)

    # Active plot type
    active_type <- reactive(applied_type())
    
    # Always mount all submodules once (outputs suspended when hidden)
    visualize_categorical_barplots_server("categorical", filtered_data, descriptive_summary,
                                          is_active = reactive(active_type() == "categorical"))
    visualize_numeric_boxplots_server("boxplots", filtered_data, descriptive_summary,
                                      is_active = reactive(active_type() == "boxplots"))
    visualize_numeric_histograms_server("histograms", filtered_data, descriptive_summary,
                                        is_active = reactive(active_type() == "histograms"))
    visualize_cv_server("cv", filtered_data, descriptive_summary,
                        is_active = reactive(active_type() == "cv"))
    visualize_outliers_server("outliers", filtered_data, descriptive_summary,
                              is_active = reactive(active_type() == "outliers"))
    visualize_missing_server("missing", filtered_data, descriptive_summary,
                             is_active = reactive(active_type() == "missing"))
    
    # Render once; use conditionalPanel to show the correct one
    output$sub_controls <- renderUI({
      tagList(
        conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical'", ns("plot_type")),
          visualize_categorical_barplots_ui(ns("categorical"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'boxplots'", ns("plot_type")),
          visualize_numeric_boxplots_ui(ns("boxplots"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'histograms'", ns("plot_type")),
          visualize_numeric_histograms_ui(ns("histograms"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'cv'", ns("plot_type")),
          visualize_cv_ui(ns("cv"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'outliers'", ns("plot_type")),
          visualize_outliers_ui(ns("outliers"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'missing'", ns("plot_type")),
          visualize_missing_ui(ns("missing"))
        )
      )
    })
    
    output$plot_ui <- renderUI({
      tagList(
        conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical'", ns("plot_type")),
          visualize_categorical_barplots_plot_ui(ns("categorical"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'boxplots'", ns("plot_type")),
          visualize_numeric_boxplots_plot_ui(ns("boxplots"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'histograms'", ns("plot_type")),
          visualize_numeric_histograms_plot_ui(ns("histograms"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'cv'", ns("plot_type")),
          visualize_cv_plot_ui(ns("cv"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'outliers'", ns("plot_type")),
          visualize_outliers_plot_ui(ns("outliers"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'missing'", ns("plot_type")),
          visualize_missing_plot_ui(ns("missing"))
        )
      )
    })
  })
}
