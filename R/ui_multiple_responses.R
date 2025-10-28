# ===============================================================
# 🔁 Shared utilities for multiple-response models
# ===============================================================

# --- UI builder for the checkbox and response selector
render_response_inputs <- function(ns, data, input) {
  df <- if (is.function(data)) data() else data
  req(df)
  num_vars <- names(df)[sapply(df, is.numeric)]
  
  current_selection <- input$response
  if (is.null(current_selection) || !all(current_selection %in% num_vars)) {
    current_selection <- if (length(num_vars) > 0) num_vars[1] else NULL
  }
  
  tagList(
    checkboxInput(
      ns("multi_resp"),
      "Allow multiple response variables",
      value = isTRUE(input$multi_resp)
    ),
    selectInput(
      ns("response"),
      if (isTRUE(input$multi_resp)) "Response variables (numeric):" else "Response variable (numeric):",
      choices = num_vars,
      selected = current_selection,
      multiple = isTRUE(input$multi_resp)
    )
  )
}

# --- Server-side helper to retrieve a standardized list of responses
get_selected_responses <- function(input) {
  req(input$response)
  responses <- input$response
  if (!isTRUE(input$multi_resp)) responses <- responses[1]
  unique(responses)
}
