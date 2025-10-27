# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

add_color_customization_ui <- function(ns, multi_group = TRUE) {
  tags$details(
    tags$summary(strong("Advanced options")),
    uiOutput(ns("color_custom_ui"))
  )
}

# ---- SERVER ----
add_color_customization_server <- function(ns, input, output, data, color_var_reactive, multi_group = TRUE) {
  output$color_custom_ui <- renderUI({
    req(data())
    color_var <- color_var_reactive()
    
    if (isTRUE(multi_group)) {
      if (is.null(color_var)) return(NULL)
      render_color_inputs(ns, data, color_var)
    } else {
      tagList(
        br(),
        h5("Line color"),
        color_dropdown_input(ns, "single_color", basic_color_palette, ncol = 4)
      )
    }
  })
  
  reactive({
    if (isTRUE(multi_group)) {
      req(data())
      color_var <- color_var_reactive()
      req(color_var)
      
      lvls <- levels(as.factor(data()[[color_var]]))
      base_palette <- RColorBrewer::brewer.pal(8, "Set2")
      cols <- vapply(seq_along(lvls), function(i) {
        input_val <- input[[paste0("col_", color_var, "_", i)]]
        if (is.null(input_val) || identical(input_val, "")) {
          base_palette[(i - 1) %% length(base_palette) + 1]
        } else {
          input_val
        }
      }, character(1))
      names(cols) <- lvls
      cols
    } else {
      selected_color <- input$single_color
      if (is.null(selected_color)) selected_color <- "steelblue"
      selected_color
    }
  })
}

