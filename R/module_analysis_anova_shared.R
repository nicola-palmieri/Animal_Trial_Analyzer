# ===============================================================
# 🧠 Animal Trial Analyzer — Shared ANOVA Module Helpers
# ===============================================================

source("R/module_analysis_stratification.R")

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
# 2️⃣ Helper to fit ANOVA models (handles stratification)
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
          verbatimTextOutput(ns(paste0("summary_", i, "_", j))),
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

# ---------------------------------------------------------------
# 6  Bind result renderers (summaries + downloads)
#     Shared server-side wiring for both ANOVA modules
# ---------------------------------------------------------------
bind_anova_outputs <- function(ns, output, models_reactive) {
  observeEvent(models_reactive(), {
    model_info <- models_reactive()
    if (is.null(model_info)) return()

    responses <- model_info$responses
    model_list <- model_info$models
    strata_info <- model_info$strata
    factors <- unlist(model_info$factors, use.names = FALSE)

    # --- Non-stratified case ---
    if (is.null(strata_info)) {
      for (i in seq_along(responses)) {
        local({
          idx <- i
          response_name <- responses[i]
          model_obj <- model_list[[response_name]]

          output[[paste0("summary_", idx)]] <- renderPrint({
            results <- prepare_anova_outputs(model_obj, factors)
            print(results$anova_object)

            if (length(results$posthoc_details) == 0) {
              cat("\nNo post-hoc Tukey comparisons were generated.\n")
            } else {
              for (factor_nm in names(results$posthoc_details)) {
                details <- results$posthoc_details[[factor_nm]]
                if (!is.null(details$error)) {
                  cat("\nPost-hoc Tukey comparisons for", factor_nm, "failed:", details$error, "\n")
                } else if (!is.null(details$table)) {
                  cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
                  print(details$table)
                }
              }
            }
          })

          output[[paste0("download_", idx)]] <- downloadHandler(
            filename = function() {
              safe_resp <- sanitize_name(response_name)
              paste0("anova_results_", safe_resp, "_", Sys.Date(), ".docx")
            },
            content = function(file) {
              results <- prepare_anova_outputs(model_obj, factors)
              write_anova_docx(file, results, model_obj, response_name)
            }
          )
        })
      }
      return()
    }

    # --- Stratified case ---
    strata_levels <- strata_info$levels
    for (i in seq_along(responses)) {
      for (j in seq_along(strata_levels)) {
        local({
          idx <- i
          stratum_idx <- j
          response_name <- responses[i]
          stratum_label <- strata_levels[j]
          model_obj <- model_list[[stratum_label]][[response_name]]

          output[[paste0("summary_", idx, "_", stratum_idx)]] <- renderPrint({
            results <- prepare_anova_outputs(model_obj, factors)
            print(results$anova_object)

            if (length(results$posthoc_details) == 0) {
              cat("\nNo post-hoc Tukey comparisons were generated.\n")
            } else {
              for (factor_nm in names(results$posthoc_details)) {
                details <- results$posthoc_details[[factor_nm]]
                if (!is.null(details$error)) {
                  cat("\nPost-hoc Tukey comparisons for", factor_nm, "failed:", details$error, "\n")
                } else if (!is.null(details$table)) {
                  cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
                  print(details$table)
                }
              }
            }
          })

          output[[paste0("download_", idx, "_", stratum_idx)]] <- downloadHandler(
            filename = function() {
              paste0(
                "anova_results_",
                sanitize_name(response_name),
                "_stratum_",
                sanitize_name(stratum_label),
                "_",
                Sys.Date(),
                ".docx"
              )
            },
            content = function(file) {
              results <- prepare_anova_outputs(model_obj, factors)
              write_anova_docx(file, results, model_obj, response_name, stratum_label)
            }
          )
        })
      }
    }
  })
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
