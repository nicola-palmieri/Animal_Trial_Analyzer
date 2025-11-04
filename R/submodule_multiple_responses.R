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
    ns <- session$ns
    
    # --- Reactive data wrapper (clean + guarded)
    df <- reactive({
      d <- if (is.function(data)) data() else data
      req(d)
      d
    })
    
    # --- Render selectInput dynamically
    output$response_ui <- renderUI({
      d <- req(df())
      num_vars <- names(d)[sapply(d, is.numeric)]
      validate(need(length(num_vars) > 0, "No numeric variables available."))
      
      current_selection <- input$response
      if (is.null(current_selection) || !all(current_selection %in% num_vars)) {
        current_selection <- if (length(num_vars) > 0) num_vars[1] else NULL
      }
      
      selectInput(
        ns("response"),
        label = if (isTRUE(input$multi_resp))
          "Response variables (numeric):"
        else
          "Response variable (numeric):",
        choices = num_vars,
        selected = current_selection,
        multiple = isTRUE(input$multi_resp)
      )
    })
    
    # --- Standardized reactive vector output
    selected_responses <- reactive({
      res <- req(input$response)
      if (!isTRUE(input$multi_resp)) res <- res[1]
      unique(res)
    })
    
    return(selected_responses)
  })
}
