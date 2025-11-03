# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

# -- Utility to resolve either a reactive expression or raw data frame
.resolve_data <- function(data) {
  if (is.function(data)) data() else data
}

MAX_STRATIFICATION_LEVELS <- 10

guard_stratification_levels <- function(data, stratify_var,
                                        max_levels = MAX_STRATIFICATION_LEVELS,
                                        session = shiny::getDefaultReactiveDomain(),
                                        notify = TRUE) {
  if (is.null(stratify_var) || identical(stratify_var, "None") || identical(stratify_var, "")) {
    return(TRUE)
  }

  df <- .resolve_data(data)
  if (is.null(df) || !is.data.frame(df) || !(stratify_var %in% names(df))) {
    return(TRUE)
  }

  values <- df[[stratify_var]]
  values <- values[!is.na(values)]
  n_levels <- length(unique(as.character(values)))

  if (n_levels <= max_levels) {
    return(TRUE)
  }

  message <- paste0(
    "❌ Stratification variable '", stratify_var,
    "' has ", n_levels,
    " levels — please select a variable with at most ", max_levels, "."
  )

  if (isTRUE(notify) && !is.null(session)) {
    shiny::showNotification(message, type = "error", duration = 8)
  }

  FALSE
}

# ---------------------------------------------------------------
# Stratification UI module (select + order controls)
# ---------------------------------------------------------------
STRAT_SECTION_TITLE <- "Advanced options"
STRAT_CHOOSE_LABEL <- "Stratify by:"
STRAT_NONE_LABEL <- "None"
STRAT_ORDER_LABEL <- "Order of levels (first = reference):"

stratification_ui <- function(id, ns = NULL) {
  ns_fn <- if (is.null(ns)) {
    NS(id)
  } else if (is.function(ns)) {
    function(x) ns(paste(id, x, sep = "-"))
  } else {
    NS(id)
  }

  shiny::renderUI({
    shiny::tags$details(
      shiny::tags$summary(shiny::strong(STRAT_SECTION_TITLE)),
      shiny::uiOutput(ns_fn("stratify_var_ui")),
      shiny::uiOutput(ns_fn("strata_order_ui"))
    )
  })
}

stratification_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    resolved_data <- reactive({
      .resolve_data(data)
    })

    output$stratify_var_ui <- shiny::renderUI({
      df <- resolved_data()

      choices <- STRAT_NONE_LABEL
      if (is.data.frame(df) && ncol(df) > 0) {
        cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
        cat_cols <- setdiff(unique(cat_cols), STRAT_NONE_LABEL)
        if (length(cat_cols) > 0) {
          choices <- c(STRAT_NONE_LABEL, cat_cols)
        }
      }

      current <- isolate(input$stratify_var)
      if (is.null(current) || !(current %in% choices)) {
        current <- STRAT_NONE_LABEL
      }

      shiny::selectInput(
        session$ns("stratify_var"),
        STRAT_CHOOSE_LABEL,
        choices = choices,
        selected = current
      )
    })

    strat_details <- reactive({
      df <- resolved_data()
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        return(list(var = NULL, levels = NULL, available_levels = NULL))
      }

      strat_var <- input$stratify_var
      if (is.null(strat_var) || identical(strat_var, STRAT_NONE_LABEL) ||
          !nzchar(strat_var) || !(strat_var %in% names(df))) {
        return(list(var = NULL, levels = NULL, available_levels = NULL))
      }

      if (!guard_stratification_levels(df, strat_var, session = session)) {
        shiny::updateSelectInput(session, "stratify_var", selected = STRAT_NONE_LABEL)
        return(list(var = NULL, levels = NULL, available_levels = NULL))
      }

      values <- df[[strat_var]]
      if (is.factor(values)) {
        available_levels <- levels(values)
      } else {
        values <- values[!is.na(values)]
        available_levels <- unique(as.character(values))
      }

      available_levels <- available_levels[!is.na(available_levels)]

      if (length(available_levels) == 0) {
        return(list(var = strat_var, levels = character(0), available_levels = character(0)))
      }

      selected_levels <- input$strata_order
      if (!is.null(selected_levels) && length(selected_levels) > 0) {
        selected_levels <- selected_levels[selected_levels %in% available_levels]
      }

      if (is.null(selected_levels) || length(selected_levels) == 0) {
        selected_levels <- available_levels
      }

      list(
        var = strat_var,
        levels = selected_levels,
        available_levels = available_levels
      )
    })

    output$strata_order_ui <- shiny::renderUI({
      details <- strat_details()
      strat_var <- details$var
      if (is.null(strat_var)) return(NULL)

      available_levels <- details$available_levels
      if (is.null(available_levels) || length(available_levels) == 0) return(NULL)

      selected_levels <- details$levels
      if (is.null(selected_levels) || length(selected_levels) == 0) {
        selected_levels <- available_levels
      }

      shiny::selectInput(
        session$ns("strata_order"),
        STRAT_ORDER_LABEL,
        choices = available_levels,
        selected = selected_levels,
        multiple = TRUE
      )
    })

    reactive({
      details <- strat_details()
      list(var = details$var, levels = details$levels)
    })
  })
}

