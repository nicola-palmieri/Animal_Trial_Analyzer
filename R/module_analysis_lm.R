# ===============================================================
# 🧮 Linear Model (LM) — fixed effects only
# ===============================================================

source("R/module_analysis_lm_helpers.R")
source("R/module_analysis_regression_shared.R")

lm_ui <- function(id) regression_ui(id, "lm")

lm_server <- function(id, data) regression_server(id, data, "lm")
