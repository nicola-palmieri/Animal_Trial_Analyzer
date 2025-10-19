# ===============================================================
# 🧠 Animal Trial Analyzer — Shared ANOVA Module Helpers
# ===============================================================

# ---------------------------------------------------------------
# 1️⃣ Response selector UI (single / multi)
# ---------------------------------------------------------------
render_response_selector <- function(ns, df, input) {
  req(df())
  data <- df()
  num_cols <- names(data)[sapply(data, is.numeric)]
  
  if (isTRUE(input$multi_resp)) {
    selectizeInput(
      ns("response"),
      "Response variables (numeric):",
      choices = num_cols,
      selected = head(num_cols, 1),
      multiple = TRUE,
      options = list(maxItems = 10)
    )
  } else {
    selectInput(
      ns("response"),
      "Response variable (numeric):",
      choices = num_cols,
      selected = if (length(num_cols) > 0) num_cols[1] else NULL
    )
  }
}

# ---------------------------------------------------------------
# 2️⃣ Advanced options for stratification
# ---------------------------------------------------------------
render_advanced_options <- function(ns, df, input) {
  req(df())
  data <- df()
  cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
  choices <- c("None", setdiff(unique(cat_cols), "None"))
  
  tags$details(
    tags$summary(strong("Advanced options")),
    selectInput(
      ns("stratify_var"),
      "Stratify analysis by:",
      choices = choices,
      selected = "None"
    ),
    uiOutput(ns("strata_order_ui"))
  )
}

# ---------------------------------------------------------------
# 3️⃣ Helper to fit ANOVA models (handles stratification)
# ---------------------------------------------------------------
prepare_stratified_models <- function(df, responses, strat_var, factor1, factor2, orders, formula_builder) {
  req(df, responses)
  responses <- unique(responses)
  
  if (length(responses) > 10)
    validate(need(FALSE, "Please select at most 10 response variables."))
  
  # Apply factor ordering
  if (!is.null(factor1)) df[[factor1]] <- factor(df[[factor1]], levels = orders$order1)
  if (!is.null(factor2)) df[[factor2]] <- factor(df[[factor2]], levels = orders$order2)
  
  # Handle stratification
  if (!is.null(strat_var) && !identical(strat_var, "None")) {
    if (!is.null(df[[strat_var]])) {
      strata_counts <- table(df[[strat_var]])
      strata <- names(strata_counts)[strata_counts > 0]
    } else strata <- NULL
  } else strata <- NULL
  
  # Case 1 — no stratification
  if (is.null(strata)) {
    model_list <- list()
    for (resp in responses) {
      model_formula <- formula_builder(resp, factor1, factor2)
      model_list[[resp]] <- lm(model_formula, data = df)
    }
    return(list(
      type = "anova",
      models = model_list,
      responses = responses,
      strata = NULL,
      factors = list(factor1 = factor1, factor2 = factor2),
      orders = orders
    ))
  }
  
  # Case 2 — stratified
  if (length(strata) > 10)
    validate(need(FALSE, "Stratified analysis supports up to 10 strata."))
  
  model_list <- list()
  for (stratum in strata) {
    subset_data <- df[df[[strat_var]] == stratum, , drop = FALSE]
    if (!is.null(factor1)) subset_data[[factor1]] <- factor(subset_data[[factor1]], levels = orders$order1)
    if (!is.null(factor2)) subset_data[[factor2]] <- factor(subset_data[[factor2]], levels = orders$order2)
    
    model_list[[stratum]] <- list()
    for (resp in responses) {
      model_formula <- formula_builder(resp, factor1, factor2)
      model_list[[stratum]][[resp]] <- lm(model_formula, data = subset_data)
    }
  }
  
  list(
    type = "anova",
    models = model_list,
    responses = responses,
    strata = list(var = strat_var, levels = strata),
    factors = list(factor1 = factor1, factor2 = factor2),
    orders = orders
  )
}

# ---------------------------------------------------------------
# 4️⃣ Safe name sanitization for filenames
# ---------------------------------------------------------------
sanitize_name <- function(name) {
  safe <- gsub("[^A-Za-z0-9]+", "_", name)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

# ---------------------------------------------------------------
# 5️⃣ Shared results renderer (tab panels + download buttons)
# ---------------------------------------------------------------
render_anova_results <- function(ns, model_info, module_label = "ANOVA") {
  if (is.null(model_info)) return(NULL)
  
  responses <- model_info$responses
  strata_info <- model_info$strata
  
  # No stratification
  if (is.null(strata_info)) {
    tabs <- lapply(seq_along(responses), function(i) {
      tabPanel(
        title = responses[i],
        tags$div(
          verbatimTextOutput(ns(paste0("summary_", i))),
          downloadButton(ns(paste0("download_", i)), "Download Results")
        )
      )
    })
    return(do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs)))
  }
  
  # Stratified
  strata_levels <- strata_info$levels
  tabs <- lapply(seq_along(responses), function(i) {
    response_name <- responses[i]
    stratum_tabs <- lapply(seq_along(strata_levels), function(j) {
      stratum_name <- strata_levels[j]
      tabPanel(
        title = stratum_name,
        tags$div(
          tags$details(
            open = FALSE,
            tags$summary(strong("R Output")),
            verbatimTextOutput(ns(paste0("summary_", i, "_", j)))
          ),
          h4("Download Results"),
          downloadButton(
            ns(paste0("download_", i, "_", j)),
            "Download Results"
          )
        )
      )
    })
    tabPanel(
      title = response_name,
      do.call(tabsetPanel, c(list(id = ns(paste0("strata_tabs_", i))), stratum_tabs))
    )
  })
  do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs))
}

# ===============================================================
# 🔽 Helper — Combine multiple ANOVA results into one DOCX
# ===============================================================
download_all_anova_results <- function(models_info, file) {
  if (is.null(models_info) || is.null(models_info$models)) {
    stop("No models found to export.")
  }
  
  main_doc <- officer::read_docx()
  
  add_result_to_doc <- function(model_obj, response, stratum_label = NULL, model_info) {
    # Prepare and write a temporary file for each ANOVA result
    tmpfile <- tempfile(fileext = ".docx")
    results <- prepare_anova_outputs(model_obj, unlist(model_info$factors, use.names = FALSE))
    write_anova_docx(tmpfile, results, model_obj, response, stratum_label)
    # Append it to the main document
    main_doc <<- officer::body_add_docx(main_doc, src = tmpfile)
    main_doc <<- officer::body_add_par(main_doc, "", style = "Normal")
    main_doc <<- officer::body_add_par(main_doc, "", style = "Normal")
    
  }
  
  # --- Case 1: no stratification
  if (is.null(models_info$strata)) {
    for (resp in models_info$responses) {
      model_obj <- models_info$models[[resp]]
      add_result_to_doc(model_obj, resp, NULL, models_info)
    }
  } else {
    # --- Case 2: stratified
    for (stratum in models_info$strata$levels) {
      for (resp in models_info$responses) {
        model_obj <- models_info$models[[stratum]][[resp]]
        add_result_to_doc(model_obj, resp, stratum, models_info)
      }
    }
  }
  
  print(main_doc, target = file)
}
