# ===============================================================
# 🧪 Table Analyzer — PCA Module
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      selectInput(ns("vars"), "Numeric variables:", choices = NULL, multiple = TRUE),
      fluidRow(
        column(6, actionButton(ns("run_pca"), "Show PCA summary", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"))
      )
    ),
    results = tagList(
      h5("PCA summary and loadings"),
      verbatimTextOutput(ns("summary"))
    )
  )
}

pca_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    # Dynamically populate numeric variable list
    observe({
      num_vars <- names(df())[sapply(df(), is.numeric)]
      updateSelectInput(session, "vars", choices = num_vars, selected = num_vars)
    })
    
    # Run PCA
    pca_result <- eventReactive(input$run_pca, {
      req(input$vars)
      dat <- df()[, input$vars, drop = FALSE]
      validate(need(ncol(dat) > 1, "Select at least two numeric variables for PCA."))
      prcomp(dat, center = TRUE, scale. = TRUE)
    })
    
    # Verbatim output: summary + loadings
    output$summary <- renderPrint({
      req(pca_result())
      model <- pca_result()
      cat("── PCA Summary ──\n")
      print(summary(model))
      cat("\n── PCA Loadings (rotation matrix) ──\n")
      print(round(model$rotation, 3))
      cat("\n── PCA Explained Variance (%) ──\n")
      var_exp <- 100 * model$sdev^2 / sum(model$sdev^2)
      print(round(var_exp, 2))
      invisible()
    })
    
    # Download combined results
    output$download_all <- downloadHandler(
      filename = function() paste0("PCA_results_", Sys.Date(), ".txt"),
      content = function(file) {
        sink(file)
        on.exit(sink(), add = TRUE)
        model <- pca_result()
        cat("── PCA Summary ──\n")
        print(summary(model))
        cat("\n── PCA Loadings (rotation matrix) ──\n")
        print(round(model$rotation, 3))
        cat("\n── PCA Explained Variance (%) ──\n")
        var_exp <- 100 * model$sdev^2 / sum(model$sdev^2)
        print(round(var_exp, 2))
      }
    )
    
    # Return structured reactive for integration
    reactive({
      list(
        type = "pca",
        model = pca_result(),
        data = df(),
        vars = input$vars
      )
    })
  })
}
