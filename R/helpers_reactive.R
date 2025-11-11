resolve_reactive <- function(value, default = NULL) {
  if (is.null(value)) {
    return(default)
  }

  resolved <- if (is.reactive(value)) value() else value

  if (is.null(resolved)) default else resolved
}
