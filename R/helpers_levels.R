# ===============================================================
# 🔧 Helpers for level-order widgets used across the app
# ===============================================================

#' Collect the available levels for an "order of levels" widget.
#'
#' This helper centralizes the logic that was previously duplicated
#' across LM, LMM, ANOVA, and stratification modules. In every case we
#' want the behaviour to match the regression modules: factor levels are
#' shown as-is, while character vectors drop `NA`s before taking the
#' unique values.
#'
#' @param values A vector (factor or character) containing the candidate
#'   levels.
#'
#' @return A character vector of unique levels suitable for a selectInput.
resolve_order_levels <- function(values) {
  if (is.null(values)) return(character())

  if (is.factor(values)) {
    levels(values)
  } else {
    values <- values[!is.na(values)]
    unique(as.character(values))
  }
}

