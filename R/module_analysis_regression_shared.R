# ===============================================================
# 🔧 Shared helpers for LM/LMM (UI + server utilities)
# ===============================================================

# --- Type detection (robust for Excel imports) ---
reg_detect_types <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  fac_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  list(num = num_vars, fac = fac_vars)
}

# --- Variable selectors UI (optionally expose a random effect) ---
reg_variable_selectors_ui <- function(ns, types, allow_random = FALSE) {
  out <- list(
    selectInput(ns("dep"), "Response variable (numeric):", choices = types$num),
    selectInput(ns("fixed"), "Categorical predictors:", choices = types$fac, multiple = TRUE),
    selectInput(ns("covar"), "Numeric predictors:", choices = types$num, multiple = TRUE)
  )
  if (allow_random) {
    out <- c(out, list(
      selectInput(ns("random"), "Random effect (categorical):", choices = types$fac, selected = NULL)
    ))
  }
  do.call(tagList, out)
}

# --- 2-way interaction checkbox list (categorical × categorical only) ---
reg_interactions_ui <- function(ns, fixed, fac_vars) {
  if (is.null(fixed) || length(fixed) < 2) return(NULL)
  cats_only <- intersect(fixed, fac_vars)
  if (length(cats_only) < 2) return(NULL)
  pairs <- combn(cats_only, 2, simplify = FALSE)
  pair_labels <- vapply(pairs, function(p) paste(p, collapse = " × "), character(1))
  pair_values <- vapply(pairs, function(p) paste(p, collapse = ":"), character(1))
  checkboxGroupInput(
    ns("interactions"),
    label = "Add 2-way interactions (optional):",
    choices = stats::setNames(pair_values, pair_labels)
  )
}

# --- Compose RHS terms (fixed + covar + interactions + optional random) ---
reg_compose_rhs <- function(fixed, covar, interactions, random = NULL, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  rhs <- character(0)
  if (!is.null(fixed) && length(fixed) > 0) rhs <- c(rhs, fixed)
  if (!is.null(covar) && length(covar) > 0) rhs <- c(rhs, covar)
  if (!is.null(interactions) && length(interactions) > 0) rhs <- c(rhs, interactions)
  if (engine == "lmm" && !is.null(random) && nzchar(random)) {
    rhs <- c(rhs, paste0("(1|", random, ")"))
  }
  rhs
}

# --- Formula preview UI ---
reg_formula_preview_ui <- function(ns, dep, rhs) {
  if (is.null(dep) || !nzchar(dep)) return(NULL)
  form_txt <- if (length(rhs) == 0) paste(dep, "~ 1") else paste(dep, "~", paste(rhs, collapse = " + "))
  wellPanel(
    strong("Model formula: "),
    code(form_txt)
  )
}

# --- Fit model (LM vs LMM) ---
reg_fit_model <- function(dep, rhs, data, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  form <- as.formula(if (length(rhs) == 0) paste(dep, "~ 1") else paste(dep, "~", paste(rhs, collapse = " + ")))
  if (engine == "lm") {
    lm(form, data = data)
  } else {
    # LMM: lme4 + lmerTest for p-values
    lmerTest::lmer(form, data = data)
  }
}
