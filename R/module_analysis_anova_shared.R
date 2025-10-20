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
#     Refactored into smaller helpers for clarity
# ---------------------------------------------------------------

# Print ANOVA summary and post-hoc results to the output
print_anova_summary_and_posthoc <- function(model_obj, factors) {
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
  invisible(results)
}

# Helper to bind a single model's outputs (summary + download)
bind_single_model_outputs <- function(output, summary_id, download_id,
                                      model_obj, response_name, factors,
                                      stratum_label = NULL) {
  output[[summary_id]] <- renderPrint({
    print_anova_summary_and_posthoc(model_obj, factors)
  })

  output[[download_id]] <- downloadHandler(
    filename = function() {
      base <- paste0("anova_results_", sanitize_name(response_name))
      if (!is.null(stratum_label)) {
        base <- paste0(base, "_stratum_", sanitize_name(stratum_label))
      }
      paste0(base, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      results <- prepare_anova_outputs(model_obj, factors)
      write_anova_docx(file, results, model_obj, response_name, stratum_label)
    }
  )
}

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
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx),
            download_id = paste0("download_", idx),
            model_obj = model_obj,
            response_name = response_name,
            factors = factors
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
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx, "_", stratum_idx),
            download_id = paste0("download_", idx, "_", stratum_idx),
            model_obj = model_obj,
            response_name = response_name,
            factors = factors,
            stratum_label = stratum_label
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

# ---------------------------------------------------------------
# 7  Low-level utilities used by ANOVA modules
#     Order: p-value formatting -> significance -> table prep -> doc export
# ---------------------------------------------------------------

# ---- p-value formatting ----
format_p_value <- function(p_values) {
  vapply(
    p_values,
    function(p) {
      if (is.na(p)) {
        return(NA_character_)
      }
      if (p < 0.001) {
        "<0.001"
      } else {
        sprintf("%.2f", round(p, 2))
      }
    },
    character(1)
  )
}

# ---- significance markers ----
add_significance_marker <- function(formatted_p, raw_p) {
  mapply(
    function(fp, rp) {
      if (is.na(rp)) {
        return(fp)
      }
      if (rp < 0.05) {
        paste0(fp, "*")
      } else {
        fp
      }
    },
    formatted_p,
    raw_p,
    USE.NAMES = FALSE
  )
}

# ---- prepare ANOVA + posthoc results ----
prepare_anova_outputs <- function(model_obj, factor_names) {
  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  anova_obj <- car::Anova(model_obj, type = 3)
  anova_df <- as.data.frame(anova_obj)
  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]

  # --- format p-values and round numeric columns ---
  p_col <- grep("^Pr", names(anova_df), value = TRUE)
  p_col <- if (length(p_col) > 0) p_col[1] else NULL
  raw_p <- if (!is.null(p_col)) anova_df[[p_col]] else rep(NA_real_, nrow(anova_df))

  for (col in names(anova_df)) {
    if (is.numeric(anova_df[[col]])) {
      anova_df[[col]] <- round(anova_df[[col]], 2)
    }
  }

  anova_significant <- !is.na(raw_p) & raw_p < 0.05
  if (!is.null(p_col)) {
    formatted_p <- format_p_value(raw_p)
    anova_df[[p_col]] <- add_significance_marker(formatted_p, raw_p)
    names(anova_df)[names(anova_df) == p_col] <- "p.value"
  } else {
    anova_df$p.value <- NA_character_
  }

  # --- Post-hoc Tukey for each factor ---
  factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
  posthoc_details <- list()
  posthoc_combined <- NULL
  posthoc_significant <- numeric(0)

  for (factor_nm in factor_names) {
    if (!factor_nm %in% names(model_obj$model)) next

    res <- tryCatch({
      emm <- emmeans::emmeans(model_obj, specs = factor_nm)
      contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
      as.data.frame(summary(contrasts))
    }, error = function(e) list(error = e$message))

    if (is.data.frame(res)) {
      res$Factor <- factor_nm
      posthoc_details[[factor_nm]] <- list(table = res, error = NULL)
      posthoc_combined <- rbind(posthoc_combined, res)
    } else {
      posthoc_details[[factor_nm]] <- list(table = NULL, error = res$error)
    }
  }

  if (!is.null(posthoc_combined)) {
    posthoc_combined <- posthoc_combined[, c("Factor", setdiff(names(posthoc_combined), "Factor"))]
    numeric_cols <- names(posthoc_combined)[sapply(posthoc_combined, is.numeric)]
    if (length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        posthoc_combined[[col]] <- round(posthoc_combined[[col]], 2)
      }
    }

    if ("p.value" %in% names(posthoc_combined)) {
      raw_posthoc_p <- posthoc_combined$p.value
      posthoc_significant <- !is.na(raw_posthoc_p) & raw_posthoc_p < 0.05
      formatted_posthoc_p <- format_p_value(raw_posthoc_p)
      posthoc_combined$p.value <- add_significance_marker(formatted_posthoc_p, raw_posthoc_p)
    } else {
      posthoc_significant <- rep(FALSE, nrow(posthoc_combined))
    }
  }

  list(
    anova_object = anova_obj,
    anova_table = anova_df,
    anova_significant = anova_significant,
    posthoc_details = posthoc_details,
    posthoc_table = posthoc_combined,
    posthoc_significant = posthoc_significant
  )
}

write_anova_docx <- function(doc_or_file, results, model_obj, response_name, stratum_label = NULL) {
  if (inherits(doc_or_file, "rdocx")) {
    doc <- doc_or_file
    file <- tempfile(fileext = ".docx")
  } else {
    doc <- officer::read_docx()
    file <- doc_or_file
  }
  
  doc <- officer::read_docx()
  
  # ---- Title ----
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(paste("ANOVA results —", response_name),
                                 prop = officer::fp_text(bold = TRUE, font.size = 12)))
  )
  if (!is.null(stratum_label)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(paste("Subset:", stratum_label),
                                   prop = officer::fp_text(bold = TRUE)))
    )
  }
  
  # Add some spacing before the ANOVA section
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # ---- ANOVA table ----
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext("Summary of effects",
                                 prop = officer::fp_text(bold = TRUE)))
  )
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  anova_tbl <- results$anova_table
  keep_cols <- c("Effect", "Sum Sq", "Df", "F value", "p.value")
  keep_cols <- intersect(keep_cols, names(anova_tbl))
  anova_tbl <- anova_tbl[, keep_cols, drop = FALSE]
  
  for (col in names(anova_tbl)) {
    if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 3)
  }
  
  ft_anova <- flextable::flextable(anova_tbl)
  ft_anova <- flextable::bold(ft_anova, part = "header")
  if (nrow(anova_tbl) > 0 && "p.value" %in% names(anova_tbl)) {
    ft_anova <- flextable::bold(ft_anova, i = which(results$anova_significant), j = "p.value", bold = TRUE)
  }
  ft_anova <- flextable::set_table_properties(ft_anova, width = .9, layout = "autofit")
  ft_anova <- flextable::theme_vanilla(ft_anova)
  ft_anova <- flextable::fontsize(ft_anova, size = 10, part = "all")
  doc <- flextable::body_add_flextable(doc, ft_anova)
  
  # Add spacing before next section
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # ---- Post-hoc results ----
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext("Pairwise Tukey comparisons",
                                 prop = officer::fp_text(bold = TRUE)))
  )
  
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  if (is.null(results$posthoc_table) || nrow(results$posthoc_table) == 0) {
    doc <- officer::body_add_par(doc, "No significant pairwise differences detected.", style = "Normal")
  } else {
    post_tbl <- results$posthoc_table
    keep_cols <- c("Factor", "contrast", "estimate", "SE", "df", "t.ratio", "p.value")
    keep_cols <- intersect(keep_cols, names(post_tbl))
    post_tbl <- post_tbl[, keep_cols, drop = FALSE]
    for (col in names(post_tbl)) {
      if (is.numeric(post_tbl[[col]])) post_tbl[[col]] <- round(post_tbl[[col]], 3)
    }
    
    ft_post <- flextable::flextable(post_tbl)
    ft_post <- flextable::bold(ft_post, part = "header")
    if ("p.value" %in% names(post_tbl)) {
      ft_post <- flextable::bold(ft_post, i = which(results$posthoc_significant), j = "p.value", bold = TRUE)
    }
    ft_post <- flextable::set_table_properties(ft_post, width = .9, layout = "autofit")
    ft_post <- flextable::theme_vanilla(ft_post)
    ft_post <- flextable::fontsize(ft_post, size = 10, part = "all")
    doc <- flextable::body_add_flextable(doc, ft_post)
  }
  
  # Add spacing before footer
  doc <- officer::body_add_par(doc, "", style = "Normal")
  
  # ---- Footer ----
  doc <- officer::body_add_par(doc, "Significance level: p < 0.05 (bold values).", style = "Normal")
  
  print(doc, target = file)
}
