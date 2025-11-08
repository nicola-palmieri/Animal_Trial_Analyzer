# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

add_color_customization_ui <- function(ns, multi_group = TRUE) {
  uiOutput(ns("color_custom_ui"))
}

# ---- SERVER ----
add_color_customization_server <- function(ns, input, output, data, color_var_reactive, multi_group = TRUE) {
  default_color <- "steelblue"
  
  # ---- Dynamic UI ----
  output$color_custom_ui <- renderUI({
    req(data())
    
    color_var <- color_var_reactive() %||% ""
    
    # Single color UI shown when multi-group off or no color_var available
    if (!isTRUE(multi_group) || color_var %in% c("", "None")) {
      tagList(
        br(),
        h5("Color"),
        with_help_tooltip(
          color_dropdown_input(ns, "single_color", basic_color_palette, ncol = 4),
          "Help: Choose the colour used for the entire plot."
        )
      )
    } else {
      render_color_inputs(ns, data, color_var)
    }
  })
  
  # ---- Reactive color mapping ----
  reactive({
    # --- Single-color mode (no grouping variable or disabled) ---
    if (!isTRUE(multi_group)) {
      return(input$single_color %||% default_color)
    }
    
    color_var <- color_var_reactive() %||% ""
    if (color_var %in% c("", "None")) {
      return(input$single_color %||% default_color)
    }
    
    dataset <- data()
    req(dataset)
    
    if (!color_var %in% names(dataset)) {
      return(input$single_color %||% default_color)
    }
    
    lvls <- levels(as.factor(dataset[[color_var]]))
    base_palette <- rep(basic_color_palette, length.out = length(lvls))
    
    cols <- vapply(seq_along(lvls), function(i) {
      input[[paste0("col_", color_var, "_", i)]] %||% base_palette[i]
    }, character(1))
    
    names(cols) <- lvls
    cols
  })
}
