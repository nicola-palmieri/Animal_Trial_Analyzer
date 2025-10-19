# ===============================================================
# 🧪 Animal Trial Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      br(),
      actionButton(ns("run"), "Run Pairwise Correlation", width = "100%")
    ),
    results = tagList(
      verbatimTextOutput(ns("summary"))
    )
  )
}

ggpairs_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(data_reactive())
    
    observeEvent(input$run, {
      req(df())
      numeric_df <- df()[, sapply(df(), is.numeric), drop = FALSE]
      output$summary <- renderPrint({
        if (ncol(numeric_df) < 2)
          return(cat("Need at least two numeric columns."))
        cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
        print(round(cor_matrix, 2))
      })
    })
    
    # return the subset of numeric data to visualize
    reactive({
      req(df())
      numeric_df <- df()[, sapply(df(), is.numeric), drop = FALSE]
      list(
        type = "ggpairs",
        models = NULL,
        responses = names(numeric_df),
        strata = NULL,
        factors = list(factor1 = NULL, factor2 = NULL),
        orders = list(order1 = NULL, order2 = NULL),
        data = numeric_df
      )
    })
    
  })
}
  