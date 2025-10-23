# ===============================================================
# 🧬 Linear Mixed Model (LMM) — single random intercept
# ===============================================================

lmm_ui <- function(id) regression_ui(id, "lmm", allow_multi_response = TRUE)

lmm_server <- function(id, data) regression_server(id, data, "lmm", allow_multi_response = TRUE)
