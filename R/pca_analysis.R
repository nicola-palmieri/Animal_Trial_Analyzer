# ===============================================================
# 🧪 Animal Trial Analyzer — PCA Module (analysis side only)
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      p("Select numeric variables to include in the PCA. The data will be centered and scaled automatically."),
      selectInput(ns("vars"), "Variables:", choices = NULL, multiple = TRUE),
      fluidRow(
        column(6, actionButton(ns("run_pca"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
      h5("PCA Results Summary"),
      verbatimTextOutput(ns("summary")),
      DT::dataTableOutput(ns("loadings_table"))
    )
  )
}

pca_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    observe({
      num_vars <- names(df())[sapply(df(), is.numeric)]
      updateSelectInput(session, "vars", choices = num_vars, selected = num_vars)
    })
    
    pca_result <- eventReactive(input$run_pca, {
      req(input$vars)
      dat <- df()[, input$vars, drop = FALSE]
      prcomp(dat, center = TRUE, scale. = TRUE)
    })
    
    output$summary <- renderPrint({
      req(pca_result())
      summary(pca_result())
    })
    
    output$loadings_table <- DT::renderDataTable({
      req(pca_result())
      loadings <- as.data.frame(pca_result()$rotation)
      DT::datatable(round(loadings, 3), options = list(pageLength = 5))
    })
    
    # Return structured output for visualize module
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
