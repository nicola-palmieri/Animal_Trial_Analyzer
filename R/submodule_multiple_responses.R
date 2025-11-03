# ===============================================================
# 🔁 Multi-Response Selector Module
# ===============================================================

multi_response_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(
      ns("multi_resp"),
      "Allow multiple response variables",
      value = FALSE
    ),
    uiOutput(ns("response_ui"))
  )
}

multi_response_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    df <- reactive({
      if (is.function(data)) data() else data
    })

    # --- Render selectInput dynamically
    output$response_ui <- renderUI({
      d <- df()
      req(d)
      num_vars <- names(d)[sapply(d, is.numeric)]

      # fallback selection
      current_selection <- input$response
      if (is.null(current_selection) || !all(current_selection %in% num_vars)) {
        current_selection <- if (length(num_vars) > 0) num_vars[1] else NULL
      }

      selectInput(
        session$ns("response"),
        if (isTRUE(input$multi_resp))
          "Response variables (numeric):"
        else
          "Response variable (numeric):",
        choices = num_vars,
        selected = current_selection,
        multiple = isTRUE(input$multi_resp)
      )
    })

    # --- Return standardized reactive vector
    reactive({
      req(input$response)
      res <- input$response
      if (!isTRUE(input$multi_resp)) res <- res[1]
      unique(res)
    })
  })
}
