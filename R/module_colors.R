# ===============================================================
# 🎨 Module for colors customization
# ===============================================================

add_color_customization_ui <- function(ns, multi_group = TRUE) {
  helpText("All plots use the fixed TableAnalyzer color palette.")
}

# ---- SERVER ----
add_color_customization_server <- function(ns, input, output, data, color_var_reactive, multi_group = TRUE) {
  reactive({
    if (!isTRUE(multi_group)) {
      return(get_primary_color())
    }

    dat <- data()
    color_var <- color_var_reactive()

    if (is.null(dat) || is.null(color_var) || !nzchar(color_var) || !color_var %in% names(dat)) {
      return(get_palette_for_n(1L))
    }

    colors <- get_palette_for_values(dat[[color_var]])
    if (length(colors) == 0) {
      get_palette_for_n(1L)
    } else {
      colors
    }
  })
}

