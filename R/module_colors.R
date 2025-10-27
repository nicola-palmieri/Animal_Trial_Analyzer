# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

# ---- UI ----
add_color_customization_ui <- function(ns, multi_group = TRUE) {
  tags$details(
    tags$summary(strong("Advanced options")),
    uiOutput(ns("color_custom_ui"))
  )
}

# ---- SERVER logic ----
add_color_customization_server <- function(ns, input, output, data, color_var_reactive, multi_group = TRUE) {
  output$color_custom_ui <- renderUI({
    req(data())
    color_var <- color_var_reactive()
    if (isTRUE(multi_group) && is.null(color_var)) return(NULL)
    
    if (isTRUE(multi_group)) {
      render_color_inputs(ns, data, color_var)
    } else {
      colourpicker::colourInput(
        ns("single_color"),
        label = "Plot color",
        value = "#1f77b4"
      )
    }
  })
  
  reactive({
    if (isTRUE(multi_group)) {
      req(data())
      color_var <- color_var_reactive()
      req(color_var)
      
      lvls <- levels(as.factor(data()[[color_var]]))
      cols <- sapply(seq_along(lvls), function(i) {
        input[[paste0("col_", color_var, "_", i)]]
      })
      names(cols) <- lvls
      cols
    } else {
      single_col <- input$single_color
      if (is.null(single_col)) single_col <- "#1f77b4"
      single_col
    }
  })
}

