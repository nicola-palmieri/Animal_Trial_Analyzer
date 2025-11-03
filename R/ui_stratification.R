# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

# -- Utility to resolve either a reactive expression or raw data frame
.resolve_data <- function(data) {
  if (is.function(data)) data() else data
}

MAX_STRATIFICATION_LEVELS <- 10

normalize_single_choice <- function(value) {
  if (is.null(value)) return(NULL)
  value <- value[!is.na(value)]
  if (length(value) == 0) return(NULL)
  value <- as.character(value[[1]])
  if (!nzchar(value)) return(NULL)
  value
}

guard_stratification_levels <- function(data, stratify_var,
                                        max_levels = MAX_STRATIFICATION_LEVELS,
                                        session = shiny::getDefaultReactiveDomain(),
                                        notify = TRUE) {
  stratify_var <- normalize_single_choice(stratify_var)
  if (is.null(stratify_var) || identical(stratify_var, "None")) {
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
# Stratification options panel (select strat variable + placeholder for order)
# ---------------------------------------------------------------
render_stratification_controls <- function(ns, data, input,
                                           section_title = "Advanced options",
                                           stratify_label = "Stratify by:",
                                           none_label = "None") {
  df <- .resolve_data(data)
  req(df)
  
  cat_cols <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
  choices <- c(none_label, setdiff(unique(cat_cols), none_label))
  
  tags$details(
    tags$summary(strong(section_title)),
    selectInput(
      ns("stratify_var"),
      stratify_label,
      choices = choices,
      selected = none_label
    ),
    uiOutput(ns("strata_order_ui"))
  )
}

# Backwards compat alias for ANOVA modules (legacy name)
render_advanced_options <- render_stratification_controls

# ---------------------------------------------------------------
# Stratification order selector (shared across modules)
# ---------------------------------------------------------------
render_strata_order_input <- function(ns, data, strat_var,
                                      input_id = "strata_order",
                                      order_label = NULL) {
  strat_var <- normalize_single_choice(strat_var)
  if (is.null(strat_var) || identical(strat_var, "None")) return(NULL)
  
  df <- .resolve_data(data)
  if (is.null(df)) return(NULL)
  if (nrow(df) == 0) return(NULL)
  
  values <- df[[strat_var]]
  if (is.null(values)) return(NULL)
  
  if (is.factor(values)) {
    strata_levels <- levels(values)
  } else {
    values <- values[!is.na(values)]
    strata_levels <- unique(as.character(values))
  }
  
  if (length(strata_levels) == 0) return(NULL)
  
  if (is.null(order_label)) {
    order_label <- "Order of levels (first = reference):"
  }
  
  selectInput(
    ns(input_id),
    order_label,
    choices = strata_levels,
    selected = strata_levels,
    multiple = TRUE
  )
}


