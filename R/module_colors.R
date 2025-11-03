# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

add_color_customization_ui <- function(ns, multi_group = TRUE) {
  tags$details(
    tags$summary(strong("Advanced options")),
    br(),
    uiOutput(ns("color_custom_ui"))
  )
}

# ---- SERVER ----
add_color_customization_server <- function(ns, input, output, data, color_var_reactive, multi_group = TRUE) {
  output$color_custom_ui <- renderUI({
    req(data())
    color_var <- color_var_reactive()

    single_color_ui <- tagList(
      br(),
      h5("Line color"),
      color_dropdown_input(ns, "single_color", basic_color_palette, ncol = 4)
    )

    if (isTRUE(multi_group)) {
      if (is.null(color_var) || identical(color_var, "") || identical(color_var, "None")) {
        single_color_ui
      } else {
        render_color_inputs(ns, data, color_var)
      }
    } else {
      single_color_ui
    }
  })

  reactive({
    if (isTRUE(multi_group)) {
      color_var <- color_var_reactive()
      if (is.null(color_var) || identical(color_var, "") || identical(color_var, "None")) {
        selected_color <- input$single_color
        if (is.null(selected_color) || identical(selected_color, "")) selected_color <- "steelblue"
        return(selected_color)
      }

      dataset <- data()
      if (is.null(dataset) || !color_var %in% names(dataset)) {
        selected_color <- input$single_color
        if (is.null(selected_color) || identical(selected_color, "")) selected_color <- "steelblue"
        return(selected_color)
      }

      lvls <- levels(as.factor(dataset[[color_var]]))
      base_palette <- rep(basic_color_palette, length.out = length(lvls))
      cols <- vapply(seq_along(lvls), function(i) {
        input_val <- input[[paste0("col_", color_var, "_", i)]]
        if (is.null(input_val) || identical(input_val, "")) {
          base_palette[i]
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

