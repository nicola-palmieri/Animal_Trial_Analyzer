# ===============================================================
# 🎨 UI helper to assign colors per level of a factor
# ===============================================================

render_color_inputs <- function(ns, data, color_var) {
  if (is.null(color_var) || color_var == "None") return(NULL)
  if (!color_var %in% names(data())) return(NULL)
  
  values <- data()[[color_var]]
  lvls <- if (is.factor(values)) levels(values) else unique(as.character(values))
  lvls <- lvls[!is.na(lvls)]
  
  tagList(
    h5(paste("Customize colors for", color_var)),
    lapply(seq_along(lvls), function(i) {
      colourpicker::colourInput(
        ns(paste0("col_", color_var, "_", i)),
        label = lvls[i],
        value = RColorBrewer::brewer.pal(8, "Set2")[i %% 8 + 1]
      )
    })
  )
}
