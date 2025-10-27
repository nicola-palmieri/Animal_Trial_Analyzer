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
    h5(paste("Customize colors for", color_var)),
    lapply(seq_along(lvls), function(i) {
      selected <- default_palette[i]
      tags$div(
        style = "margin-bottom: 8px;",
        tags$label(lvls[i], style = "display:block; margin-bottom: 4px;"),
        color_dropdown_input(
          ns,
          id = paste0("col_", color_var, "_", i),
          palette = basic_color_palette,
          ncol = 4,
          selected = selected
        )
      )
    })
  )
}
