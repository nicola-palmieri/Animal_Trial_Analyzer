# ===============================================================
# 📐 Shared subplot dimension helpers
# ===============================================================

validate_subplot_dimensions <- function(width_input,
                                        height_input,
                                        default_width = 400,
                                        default_height = 300) {
  sanitize <- function(value, default, label) {
    numeric_value <- suppressWarnings(as.numeric(value[1]))
    if (length(value) == 0 || is.na(numeric_value)) {
      return(list(
        value = default,
        warning = sprintf("⚠️ Please enter a subplot %s.", label)
      ))
    }

    if (!is.finite(numeric_value) || numeric_value <= 0) {
      return(list(
        value = default,
        warning = sprintf("⚠️ Subplot %s must be greater than 0.", label)
      ))
    }

    list(value = numeric_value, warning = NULL)
  }

  width <- sanitize(width_input, default_width, "width")
  height <- sanitize(height_input, default_height, "height")
  warnings <- c(width$warning, height$warning)

  list(
    width = width$value,
    height = height$value,
    warning = if (length(warnings) > 0) paste(warnings, collapse = "<br>") else NULL
  )
}

