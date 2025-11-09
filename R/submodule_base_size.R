# ===============================================================
# 🔤 Base size controls shared submodule
# ===============================================================

base_size_ui <- function(ns,
                         input_id = "plot_base_size",
                         default = 13,
                         min = 6,
                         max = 30,
                         step = 1,
                         help_text = "Adjust the base font size used for plot text.") {
  tagList(
    h5("Base size"),
    with_help_tooltip(
      numericInput(
        inputId = ns(input_id),
        label = "Base font size",
        value = default,
        min = min,
        max = max,
        step = step
      ),
      help_text
    )
  )
}

base_size_server <- function(input,
                             input_id = "plot_base_size",
                             default = 13) {
  reactive({
    value <- input[[input_id]]
    if (is.null(value) || !is.numeric(value) || length(value) == 0 || is.na(value)) {
      default
    } else {
      value
    }
  })
}
