# ===============================================================
# 🧬 Linear Mixed Model (LMM) — single random intercept
# ===============================================================

source("R/module_analysis_lmm_helpers.R")
source("R/module_analysis_regression_shared.R")
source("R/module_analysis_multiple_responses.R")

lmm_ui <- function(id) regression_ui(id, "lmm", allow_multi_response = TRUE)

lmm_server <- function(id, data) regression_server(id, data, "lmm", allow_multi_response = TRUE)
