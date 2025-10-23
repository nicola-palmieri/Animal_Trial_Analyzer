# ===============================================================
# 🧪 Animal Trial Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      p("Select numeric variables to include in the correlation matrix."),
      selectInput(ns("vars"), "Variables:", choices = NULL, multiple = TRUE),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
      h5("Correlation Matrix"),
      verbatimTextOutput(ns("summary"))
    )
  )
}

ggpairs_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(data_reactive())
    
    # ---- Update variable selector ----
    observe({
      req(df())
      num_vars <- names(df())[sapply(df(), is.numeric)]
      updateSelectInput(session, "vars", choices = num_vars, selected = num_vars)
    })
    
    # ---- Compute correlation matrix ----
    observeEvent(input$run, {
      req(df(), input$vars)
      dat <- df()[, input$vars, drop = FALSE]
      output$summary <- renderPrint({
        if (ncol(dat) < 2)
          return(cat("Need at least two numeric columns."))
        cor_matrix <- cor(dat, use = "pairwise.complete.obs")
        print(round(cor_matrix, 2))
      })
    })
    
    # ---- Return structured output for visualization ----
    reactive({
      req(df())
      num_vars <- names(df())[sapply(df(), is.numeric)]
      selected_vars <- if (length(input$vars)) input$vars else num_vars
      dat <- df()[, selected_vars, drop = FALSE]
      
      list(
        type = "ggpairs",
        models = NULL,
        responses = selected_vars,
        strata = NULL,
        factors = list(factor1 = NULL, factor2 = NULL),
        orders = list(order1 = NULL, order2 = NULL),
        data = dat
      )
    })
  })
}
