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
    shiny::singleton(
      tags$style(
        HTML(
          "\n        .ta-base-size-input .shiny-input-container,\n        .ta-base-size-input .form-group {\n          margin-bottom: 0;\n        }\n        .ta-base-size-input input.form-control {\n          height: 32px;\n          padding: 4px 10px;\n        }\n      "
        )
      )
    ),
    h5("Base size"),
    div(
      class = "ta-base-size-input",
      with_help_tooltip(
        numericInput(
          inputId = ns(input_id),
          label = NULL,
          value = default,
          min = min,
          max = max,
          step = step,
          width = "100%"
        ),
        help_text
      )
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

compute_label_text_size <- function(base_size,
                                    divisor = 4,
                                    min_size = 2.5,
                                    max_size = 6) {
  if (is.null(base_size) || !is.numeric(base_size) || !is.finite(base_size)) {
    return(min_size)
  }

  size <- base_size / divisor
  size <- max(min_size, size)
  size <- min(max_size, size)
  size
}
