# ===============================================================
# 🧩 Descriptive Statistics — Helper Functions
# ===============================================================

# ---- Main computation wrapper ----
compute_descriptive_summary <- function(data, group_var = NULL) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  group_data <- if (!is.null(group_var)) group_by(data, .data[[group_var]]) else data
  
  skim_out <- if (!is.null(group_var)) {
    group_data %>% skim()
  } else {
    skim(data)
  }
  
  cv_out <- group_data %>%
    summarise(across(
      where(is.numeric),
      ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      .names = "cv_{.col}"
    ), .groups = "drop")
  
  outlier_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ {
        q <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
      },
      .names = "outliers_{.col}"
    ), .groups = "drop")
  
  missing_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ 100 * mean(is.na(.x)),
      .names = "missing_{.col}"
    ), .groups = "drop")
  
  shapiro_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ tryCatch(shapiro.test(.x)$p.value, error = function(e) NA_real_),
      .names = "shapiro_{.col}"
    ), .groups = "drop")
  
  list(
    skim = skim_out,
    cv = cv_out,
    outliers = outlier_out,
    missing = missing_out,
    shapiro = shapiro_out
  )
}

# ---- Shared printing ----
print_summary_sections <- function(results) {
  # --- Reformat skim() headers ---
  lines <- capture.output(print(results$skim))
  
  # Replace the "Variable type: character/numeric" banner lines
  lines <- stringr::str_replace(
    lines,
    "^\\s*──\\s*Variable type:\\s*factor\\s*─+\\s*$",
    "── Categorical variables ──"
  )
  lines <- stringr::str_replace(
    lines,
    "^\\s*──\\s*Variable type:\\s*numeric\\s*─+\\s*$",
    "── Numeric variables ──"
  )
  
  cat(paste(lines, collapse = "\n"), "\n")
  
  cat("\n── Coefficient of Variation (CV%) ──\n")
  print(results$cv)
  cat("\n── Outlier Counts (IQR rule) ──\n")
  print(results$outliers)
  cat("\n── Missingness Summary (% Missing) ──\n")
  print(results$missing)
  cat("\n── Shapiro–Wilk Normality Test (p-values) ──\n")
  print(results$shapiro)
  cat("\nInterpretation:\n")
  cat("  • CV% > 20 may indicate high variability.\n")
  cat("  • Outliers = # of animals beyond 1.5×IQR.\n")
  cat("  • Missing% > 5 may indicate measurement gaps.\n")
  cat("  • Shapiro p < 0.05 → non-normal distribution.\n")
}
