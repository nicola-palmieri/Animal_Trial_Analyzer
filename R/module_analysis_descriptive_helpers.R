# ------------------------------------------------------------
# 🧾 Helper to print all sections consistently
# ------------------------------------------------------------
print_summary_sections <- function(results) {
  print(results$skim)
  cat("\n── Coefficient of Variation (CV%) ──\n")
  print(results$cv)
  cat("\n── Outlier Counts (IQR rule) ──\n")
  print(results$outliers)
  cat("\n── Shapiro–Wilk Normality Test (p-values) ──\n")
  print(results$shapiro)
  cat("\nInterpretation:\n")
  cat("  • CV% > 20 may indicate high variability.\n")
  cat("  • Outliers = # of animals beyond 1.5×IQR.\n")
  cat("  • Shapiro p < 0.05 → non-normal distribution.\n")
}