# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

# -- Utility to resolve either a reactive expression or raw data frame
.resolve_data <- function(data) {
  if (is.function(data)) data() else data
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
    order_label <- paste("Order of levels for", strat_var, "(strata):")
  }
  
  selectInput(
    ns(input_id),
    order_label,
    choices = strata_levels,
    selected = strata_levels,
    multiple = TRUE
  )
}

prepare_stratified_models <- function(
    df,
    responses,
    model,
    factor1_var = NULL,
    factor1_order = NULL,
    factor2_var = NULL,
    factor2_order = NULL,
    stratify_var = NULL,
    strata_order = NULL,
    random_effect = NULL
) {
  req(df, responses, model)
  
  if (!is.null(factor1_var) && !is.null(factor1_order)) {
    df[[factor1_var]] <- factor(as.character(df[[factor1_var]]), levels = factor1_order)
  }
  
  if (!is.null(factor2_var) && !is.null(factor2_order)) {
    df[[factor2_var]] <- factor(as.character(df[[factor2_var]]), levels = factor2_order)
  }
  
  if (!is.null(stratify_var) && !is.null(strata_order)) {
    df[[stratify_var]] <- factor(as.character(df[[stratify_var]]), levels = strata_order)
  }
  
  strata <- if (!is.null(stratify_var) && stratify_var %in% names(df)) levels(df[[stratify_var]]) else NULL
  
  build_rhs <- function() {
    if (model %in% c("oneway_anova")) {
      factor1_var
    } else if (model %in% c("twoway_anova")) {
      if (!is.null(factor1_var) && !is.null(factor2_var)) paste(factor1_var, factor2_var, sep = " *") else factor1_var
    } else if (model %in% c("lm", "lmm")) {
      if (!is.null(factor1_var) && !is.null(factor2_var)) paste(factor1_var, factor2_var, sep = " +") else factor1_var
    } else {
      factor1_var
    }
  }
  
  build_formula <- function(resp) {
    rhs <- build_rhs()
    if (!is.null(random_effect) && model == "lmm") {
      rhs <- paste(rhs, sprintf("(1|%s)", random_effect), sep = " + ")
    }
    as.formula(paste(resp, "~", rhs))
  }
  
  fit_fn <- function(fml, data) {
    if (model == "lmm") lmerTest::lmer(fml, data = data) else stats::lm(fml, data = data)
  }
  
  if (is.null(strata)) {
    out <- list()
    for (resp in responses) {
      out[[resp]] <- fit_fn(build_formula(resp), df)
    }
    return(list(
      type = model,
      models = out,
      responses = responses,
      strata = NULL,
      factors = list(factor1 = factor1_var, factor2 = factor2_var),
      orders = list(order1 = factor1_order, order2 = factor2_order)
    ))
  }
  
  out <- list()
  for (s in strata) {
    sub <- df[df[[stratify_var]] == s, , drop = FALSE]
    sub_models <- list()
    for (resp in responses) {
      sub_models[[resp]] <- fit_fn(build_formula(resp), sub)
    }
    out[[s]] <- sub_models
  }
  
  list(
    type = model,
    models = out,
    responses = responses,
    strata = list(var = stratify_var, levels = strata),
    factors = list(factor1 = factor1_var, factor2 = factor2_var),
    orders = list(order1 = factor1_order, order2 = factor2_order)
  )
}
