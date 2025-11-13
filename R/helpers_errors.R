format_safe_error_message <- function(title, details = NULL) {
  if (is.null(title) || !nzchar(title)) {
    title <- "Error"
  }

  if (inherits(details, "condition")) {
    details <- conditionMessage(details)
  }

  if (is.null(details)) {
    details <- ""
  }

  if (is.list(details)) {
    details <- unlist(details, recursive = TRUE, use.names = FALSE)
  }

  details <- vapply(details, as.character, character(1), USE.NAMES = FALSE)
  details <- trimws(details)
  details <- details[nzchar(details)]

  if (length(details) == 0) {
    return(paste0(title, ":"))
  }

  paste0(title, ":\n", paste(details, collapse = "\n"))
}

validate_numeric_columns <- function(data, columns, context_label = "response variables") {
  if (is.null(data) || !is.data.frame(data) || length(columns) == 0) {
    return(invisible(TRUE))
  }

  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    shiny::validate(shiny::need(
      FALSE,
      sprintf(
        "The following columns are no longer available: %s.",
        paste(missing_cols, collapse = ", ")
      )
    ))
  }

  non_numeric <- columns[!vapply(columns, function(col) is.numeric(data[[col]]), logical(1))]
  if (length(non_numeric) > 0) {
    shiny::validate(shiny::need(
      FALSE,
      sprintf(
        "The selected %s must be numeric. Please check their type in the Upload tab: %s.",
        context_label,
        paste(non_numeric, collapse = ", ")
      )
    ))
  }

  invisible(TRUE)
}
