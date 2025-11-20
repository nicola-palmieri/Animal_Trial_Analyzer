#### Section: Utility Helpers ####

update_numeric_range <- function(current_range, values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(current_range)
  new_range <- range(values)
  if (any(!is.finite(new_range))) return(current_range)
  if (is.null(current_range)) {
    new_range
  } else {
    c(min(current_range[1], new_range[1]), max(current_range[2], new_range[2]))
  }
}

expand_axis_limits <- function(range_vals, lower_mult = 0.05, upper_mult = 0.12) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) return(range_vals)
  span <- diff(range_vals)
  if (!is.finite(span) || span == 0) {
    span <- max(1, abs(range_vals[2]))
  }
  c(range_vals[1] - span * lower_mult, range_vals[2] + span * upper_mult)
}

ensure_barplot_zero_baseline <- function(range_vals) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) {
    return(range_vals)
  }

  lower <- range_vals[1]
  if (is.na(lower)) return(range_vals)

  # Keep barplots anchored at zero to avoid negative baselines when no annotations
  range_vals[1] <- 0
  range_vals
}

sanitize_name <- function(name) {
  safe <- gsub("[^A-Za-z0-9]+", "_", name)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

anova_protect_vars <- function(vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)

  vals <- vapply(vars, function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return("")
    if (grepl("^`.*`$", v)) v else paste0("`", v, "`")
  }, character(1))

  vals[nzchar(vals)]
}
