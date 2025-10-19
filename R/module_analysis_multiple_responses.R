# ===============================================================
# 🔁 Shared utilities for multiple-response models
# ===============================================================

# --- UI builder for the checkbox and response selector
render_response_inputs <- function(ns, data, input) {
  req(data())
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  tagList(
    checkboxInput(
      ns("multi_resp"),
      "Enable multiple response variables",
      value = isTRUE(input$multi_resp)
    ),
    selectInput(
      ns("response"),
      if (isTRUE(input$multi_resp)) "Select response variables:" else "Select response variable:",
      choices = num_vars,
      selected = if (length(num_vars) > 0) num_vars[1],
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