# ===============================================================
# 🧰 Reactive helpers shared across modules
# ===============================================================

use_filtered_df <- function(filtered_data) {
  reactive({
    req(filtered_data())
    filtered_data()
  })
}
