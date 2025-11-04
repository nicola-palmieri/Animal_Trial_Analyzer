# ===============================================================
# 🧱 Basic grid layout helpers
# ===============================================================

basic_grid_value <- function(value,
                             default = 1L,
                             min_value = 1L,
                             max_value = 10L) {
  if (is.null(value) || length(value) == 0) {
    return(as.integer(default))
  }

  raw <- suppressWarnings(as.integer(value[1]))
  if (is.na(raw)) {
    return(as.integer(default))
  }

  adjusted <- max(as.integer(min_value), raw)
  if (!is.null(max_value)) {
    adjusted <- min(as.integer(max_value), adjusted)
  }

  as.integer(adjusted)
}

basic_grid_layout <- function(rows = NULL,
                              cols = NULL,
                              default_rows = 1L,
                              default_cols = 1L,
                              min_rows = 1L,
                              min_cols = 1L,
                              max_rows = 10L,
                              max_cols = 10L) {
  list(
    nrow = basic_grid_value(
      value = rows,
      default = default_rows,
      min_value = min_rows,
      max_value = max_rows
    ),
    ncol = basic_grid_value(
      value = cols,
      default = default_cols,
      min_value = min_cols,
      max_value = max_cols
    )
  )
}
