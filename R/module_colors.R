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
        h5("Color"),
        with_help_tooltip(
          color_dropdown_input(ns, "single_color", basic_color_palette, ncol = 4),
          "Choose the colour used for the entire plot."
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

# ===============================================================
# 🎨 UI helper to assign colors per level of a factor
# ===============================================================

render_color_inputs <- function(ns, data, color_var) {
  if (is.null(color_var) || color_var == "None") return(NULL)
  if (!color_var %in% names(data())) return(NULL)
  
  values <- data()[[color_var]]
  lvls <- if (is.factor(values)) levels(values) else unique(as.character(values))
  lvls <- lvls[!is.na(lvls)]
  default_palette <- rep(basic_color_palette, length.out = length(lvls))
  
  tagList(
    h5("Colors"),
    lapply(seq_along(lvls), function(i) {
      selected <- default_palette[i]
      tags$div(
        style = "margin-bottom: 8px;",
        tags$label(lvls[i], style = "display:block; margin-bottom: 4px;"),
        with_help_tooltip(
          color_dropdown_input(
            ns,
            id = paste0("col_", color_var, "_", i),
            palette = basic_color_palette,
            ncol = 4,
            selected = selected
          ),
          sprintf("Pick the colour that will represent %s in the plot.", lvls[i])
        )
      )
    })
  )
}

resolve_single_color <- function(custom = NULL) {
  if (!is.null(custom) && length(custom) > 0) {
    candidate <- unname(custom[[1]])
    if (!is.null(candidate) && nzchar(candidate)) {
      return(candidate)
    }
  }
  basic_color_palette[1]
}

resolve_palette_for_levels <- function(levels, custom = NULL) {
  if (is.null(levels) || length(levels) == 0) {
    return(resolve_single_color())
  }
  
  unique_levels <- unique(as.character(levels))
  palette_size <- length(basic_color_palette)
  n_levels <- length(unique_levels)
  
  if (!is.null(custom) && length(custom) > 0) {
    if (!is.null(names(custom))) {
      ordered <- custom[unique_levels]
      if (all(!is.na(ordered))) {
        return(ordered)
      }
    } else if (length(custom) >= n_levels) {
      return(stats::setNames(custom[seq_len(n_levels)], unique_levels))
    }
  }
  
  if (n_levels <= palette_size) {
    palette <- basic_color_palette[seq_len(n_levels)]
  } else {
    repeats <- ceiling(n_levels / palette_size)
    palette <- rep(basic_color_palette, repeats)[seq_len(n_levels)]
  }
  
  stats::setNames(palette, unique_levels)
}
