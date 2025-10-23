# ===============================================================
# 🧩 Helpers for LMM
# ===============================================================

compute_icc <- function(model) {
  if (!inherits(model, "merMod")) return(NA_real_)
  
  vc <- as.data.frame(VarCorr(model))
  if (nrow(vc) < 2) return(NA_real_)
  
  # residual variance is always the last row
  var_residual <- vc$vcov[nrow(vc)]
  
  # compute ICC for each random effect
  icc_list <- lapply(seq_len(nrow(vc) - 1), function(i) {
    var_random <- vc$vcov[i]
    icc_value <- var_random / (var_random + var_residual)
    data.frame(
      Group = vc$grp[i],
      ICC   = round(icc_value, 3),
      stringsAsFactors = FALSE
    )
  })
  
  icc_df <- do.call(rbind, icc_list)
  rownames(icc_df) <- NULL
  icc_df
}
