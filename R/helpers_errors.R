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
