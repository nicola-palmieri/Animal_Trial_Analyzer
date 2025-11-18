# ===============================================================
# 📏 Submodule — Subplot Size Controls
# ===============================================================

subplot_size_defaults <- function() {
  list(
    width = list(
      label = "Subplot width (px)",
      value = 400,
      min = 200,
      max = 2000,
      step = 50,
      help = "Adjust how wide each subplot should be in pixels."
    ),
    height = list(
      label = "Subplot height (px)",
      value = 300,
      min = 200,
      max = 2000,
      step = 50,
      help = "Adjust how tall each subplot should be in pixels."
    )
  )
}

subplot_size_ui <- function(
    ns,
    width_value = NULL,
    height_value = NULL,
    width_help = NULL,
    height_help = NULL,
    width_label = NULL,
    height_label = NULL,
    width_id = "plot_width",
    height_id = "plot_height") {
  defaults <- subplot_size_defaults()

  width_value  <- if (is.null(width_value)) defaults$width$value else width_value
  height_value <- if (is.null(height_value)) defaults$height$value else height_value
  width_help   <- if (is.null(width_help)) defaults$width$help else width_help
  height_help  <- if (is.null(height_help)) defaults$height$help else height_help
  width_label  <- if (is.null(width_label)) defaults$width$label else width_label
  height_label <- if (is.null(height_label)) defaults$height$label else height_label

  fluidRow(
    column(6, with_help_tooltip(
      numericInput(
        ns(width_id),
        width_label,
        value = width_value,
        min = defaults$width$min,
        max = defaults$width$max,
        step = defaults$width$step
      ),
      width_help
    )),
    column(6, with_help_tooltip(
      numericInput(
        ns(height_id),
        height_label,
        value = height_value,
        min = defaults$height$min,
        max = defaults$height$max,
        step = defaults$height$step
      ),
      height_help
    ))
  )
}
