# ===============================================================
# 🧠 Table Analyzer — Shared ANOVA Module Helpers
# ===============================================================

build_anova_layout_controls <- function(ns, input, info) {
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  n_responses <- if (!is.null(info$responses)) length(info$responses) else 0

  strata_inputs <- if (has_strata) {
    tagList(
      h5("Across strata:"),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("strata_rows"),
            "Grid rows",
            value = isolate(if (is.null(input$strata_rows)) NA else input$strata_rows),
            min = 1,
            max = 10,
            step = 1
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("strata_cols"),
            "Grid columns",
            value = isolate(if (is.null(input$strata_cols)) NA else input$strata_cols),
            min = 1,
            max = 10,
            step = 1
          )
        )
      )
    )
  } else {
    NULL
  }
  
  response_inputs <- if (!is.null(n_responses) && n_responses > 1) {
    tagList(
      h5("Across responses:"),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("resp_rows"),
            "Grid rows",
            value = isolate(if (is.null(input$resp_rows)) NA else input$resp_rows),
            min = 1,
            max = 10,
            step = 1
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("resp_cols"),
            "Grid columns",
            value = isolate(if (is.null(input$resp_cols)) NA else input$resp_cols),
            min = 1,
            max = 10,
            step = 1
          )
        )
      )
    )
  } else {
    NULL
  }
  
  tagList(
    strata_inputs,
    response_inputs
  )
}


# ===============================================================
# 📊 Prepare stratified models for ANOVA (one-way / two-way)
# ===============================================================

prepare_stratified_anova <- function(
    df,
    responses,
    model,
    factor1_var = NULL,
    factor1_order = NULL,
    factor2_var = NULL,
    factor2_order = NULL,
    stratification = NULL,
    stratify_var = NULL,
    strata_order = NULL
) {
  req(df, responses, model)

  if (!is.null(stratification)) {
    if (!is.null(stratification$var)) {
      stratify_var <- stratification$var
    }
    if (!is.null(stratification$levels)) {
      strata_order <- stratification$levels
    }
  }
  
  if (!is.null(factor1_var) && !is.null(factor1_order)) {
    df[[factor1_var]] <- factor(as.character(df[[factor1_var]]), levels = factor1_order)
  }
  
  if (!is.null(factor2_var) && !is.null(factor2_order)) {
    df[[factor2_var]] <- factor(as.character(df[[factor2_var]]), levels = factor2_order)
  }
  
  if (!is.null(stratify_var) && stratify_var %in% names(df)) {
    if (!is.null(strata_order) && length(strata_order) > 0) {
      df[[stratify_var]] <- factor(as.character(df[[stratify_var]]), levels = strata_order)
    } else {
      df[[stratify_var]] <- as.factor(as.character(df[[stratify_var]]))
    }
  }
  
  strata <- if (!is.null(stratify_var) && stratify_var %in% names(df)) {
    levels(df[[stratify_var]])
  } else {
    NULL
  }
  
  build_rhs <- function() {
    if (model == "oneway_anova") {
      factor1_var
    } else if (model == "twoway_anova") {
      if (!is.null(factor1_var) && !is.null(factor2_var)) {
        paste(factor1_var, factor2_var, sep = " *")
      } else {
        factor1_var
      }
    } else {
      factor1_var
    }
  }
  
  build_formula <- function(resp) {
    rhs <- build_rhs()
    if (is.null(rhs) || rhs == "") rhs <- "1"
    as.formula(paste(resp, "~", rhs))
  }
  
  fit_fn <- function(fml, data) {
    stats::aov(fml, data = data)
  }
  safe_fit <- purrr::safely(fit_fn)
  
  if (is.null(strata)) {
    out <- list()
    for (resp in responses) {
      fit_result <- safe_fit(build_formula(resp), df)
      out[[resp]] <- list(
        model = fit_result$result,
        error = if (!is.null(fit_result$error)) conditionMessage(fit_result$error) else NULL
      )
    }
    return(list(
      type = model,
      models = out,
      responses = responses,
      strata = NULL,
      factors = list(factor1 = factor1_var, factor2 = factor2_var),
      orders = list(order1 = factor1_order, order2 = factor2_order),
      data_used = df
    ))
  }
  
  out <- list()
  for (s in strata) {
    sub <- df[df[[stratify_var]] == s, , drop = FALSE]
    sub_models <- list()
    for (resp in responses) {
      fit_result <- safe_fit(build_formula(resp), sub)
      sub_models[[resp]] <- list(
        model = fit_result$result,
        error = if (!is.null(fit_result$error)) conditionMessage(fit_result$error) else NULL
      )
    }
    out[[s]] <- sub_models
  }
  
  list(
    type = model,
    models = out,
    responses = responses,
    strata = list(var = stratify_var, levels = strata),
    factors = list(factor1 = factor1_var, factor2 = factor2_var),
    orders = list(order1 = factor1_order, order2 = factor2_order),
    data_used = df
  )
}


prepare_anova_outputs <- function(model_obj, factor_names) {
  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))

  safe_anova <- purrr::safely(function(mod) {
    car::Anova(mod, type = 3)
  })

  anova_result <- safe_anova(model_obj)
  if (!is.null(anova_result$error)) {
    return(list(
      error = conditionMessage(anova_result$error),
      anova_object = NULL,
      anova_table = NULL,
      anova_significant = NULL,
      posthoc_details = list(),
      posthoc_table = NULL,
      posthoc_significant = NULL
    ))
  }

  anova_obj <- anova_result$result
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
      contrasts <- emmeans::contrast(emm, method = "revpairwise", adjust = "tukey")
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
    error = NULL,
    anova_object = anova_obj,
    anova_table = anova_df,
    anova_significant = anova_significant,
    posthoc_details = posthoc_details,
    posthoc_table = posthoc_combined,
    posthoc_significant = posthoc_significant
  )
}

# ---------------------------------------------------------------
# Collate tidy summaries from ANOVA models
# ---------------------------------------------------------------

compile_anova_results <- function(model_info) {
  if (is.null(model_info) || is.null(model_info$models)) return(NULL)

  factor_names <- unlist(model_info$factors)
  factor_names <- factor_names[!is.na(factor_names) & nzchar(factor_names)]

  build_effects <- function(outputs) {
    if (is.null(outputs) || is.null(outputs$anova_table)) return(NULL)
    effects <- data.frame(
      Effect = outputs$anova_table$Effect,
      significant = outputs$anova_significant,
      stringsAsFactors = FALSE
    )
    if ("p.value" %in% names(outputs$anova_table)) {
      effects$p.value <- outputs$anova_table$p.value
    }
    effects
  }

  if (is.null(model_info$strata)) {
    summary_list <- list()
    posthoc_list <- list()
    effects_list <- list()
    errors_list <- list()

    for (resp in names(model_info$models)) {
      entry <- model_info$models[[resp]]
      entry_errors <- character(0)
      if (!is.null(entry$model)) {
        outputs <- prepare_anova_outputs(entry$model, factor_names)
        if (!is.null(outputs$error)) {
          entry_errors <- c(entry_errors, outputs$error)
          summary_list[[resp]] <- NULL
          posthoc_list[[resp]] <- NULL
          effects_list[[resp]] <- NULL
        } else {
          summary_list[[resp]] <- outputs$anova_table
          posthoc_list[[resp]] <- outputs$posthoc_table
          effects_list[[resp]] <- build_effects(outputs)
        }
      } else {
        summary_list[[resp]] <- NULL
        posthoc_list[[resp]] <- NULL
        effects_list[[resp]] <- NULL
      }
      if (!is.null(entry$error)) {
        entry_errors <- c(entry_errors, entry$error)
      }
      if (length(entry_errors) > 0) {
        errors_list[[resp]] <- paste(unique(entry_errors), collapse = "\n")
      }
    }

    return(list(
      summary = summary_list,
      posthoc = posthoc_list,
      effects = effects_list,
      errors = errors_list
    ))
  }

  summary_list <- list()
  posthoc_list <- list()
  effects_list <- list()
  errors_list <- list()

  for (stratum_name in names(model_info$models)) {
    stratum_models <- model_info$models[[stratum_name]]
    if (is.null(stratum_models)) next

    for (resp in names(stratum_models)) {
      entry <- stratum_models[[resp]]
      outputs <- NULL
      entry_error <- NULL
      if (!is.null(entry$model)) {
        outputs <- prepare_anova_outputs(entry$model, factor_names)
        if (!is.null(outputs$error)) {
          entry_error <- outputs$error
          outputs <- NULL
        }
      }

      if (is.null(summary_list[[resp]])) summary_list[[resp]] <- list()
      if (is.null(posthoc_list[[resp]])) posthoc_list[[resp]] <- list()
      if (is.null(effects_list[[resp]])) effects_list[[resp]] <- list()
      if (is.null(errors_list[[resp]])) errors_list[[resp]] <- list()

      summary_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) outputs$anova_table else NULL
      posthoc_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) outputs$posthoc_table else NULL
      effects_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) build_effects(outputs) else NULL

      if (!is.null(entry$error)) {
        entry_error <- c(entry_error, entry$error)
      }

      if (!is.null(entry_error)) {
        errors_list[[resp]][[stratum_name]] <- paste(unique(entry_error), collapse = "\n")
      }
    }
  }

  list(
    summary = summary_list,
    posthoc = posthoc_list,
    effects = effects_list,
    errors = errors_list
  )
}

# ---------------------------------------------------------------
# Output composition
# ---------------------------------------------------------------
print_anova_summary_and_posthoc <- function(model_entry, factors) {
  if (is.null(model_entry) || (is.list(model_entry) && is.null(model_entry$model))) {
    cat("Model is not available.\n")
    return(invisible(NULL))
  }

  if (!is.null(model_entry$error)) {
    cat("Model fitting failed:\n", model_entry$error, "\n", sep = "")
    return(invisible(NULL))
  }

  model_obj <- model_entry$model
  results <- prepare_anova_outputs(model_obj, factors)
  if (!is.null(results$error)) {
    cat("ANOVA computation failed:\n", results$error, "\n", sep = "")
    return(invisible(NULL))
  }
  if (is.null(results$anova_object)) {
    cat("ANOVA results are unavailable.\n")
    return(invisible(NULL))
  }
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

bind_single_model_outputs <- function(output, summary_id, download_id,
                                      model_entry, response_name, factors,
                                      stratum_label = NULL) {
  output[[summary_id]] <- renderPrint({
    print_anova_summary_and_posthoc(model_entry, factors)
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
      if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
        stop("Model not available for download due to fitting error.")
      }
      results <- prepare_anova_outputs(model_entry$model, factors)
      if (!is.null(results$error)) {
        stop(paste0("ANOVA results unavailable: ", results$error))
      }
      if (is.null(results$anova_table)) {
        stop("ANOVA results are unavailable for export.")
      }
      write_anova_docx(file, results, model_entry$model, response_name, stratum_label)
    }
  )
}

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
          verbatimTextOutput(ns(paste0("summary_", i)))
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
          verbatimTextOutput(ns(paste0("summary_", i, "_", j)))
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
          model_entry <- model_list[[response_name]]
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx),
            download_id = paste0("download_", idx),
            model_entry = model_entry,
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
          model_entry <- model_list[[stratum_label]][[response_name]]
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx, "_", stratum_idx),
            download_id = paste0("download_", idx, "_", stratum_idx),
            model_entry = model_entry,
            response_name = response_name,
            factors = factors,
            stratum_label = stratum_label
          )
        })
      }
    }
  })
}

# ---------------------------------------------------------------
# Results export
# ---------------------------------------------------------------

download_all_anova_results <- function(models_info, file) {
  if (is.null(models_info) || is.null(models_info$models)) {
    stop("No models found to export.")
  }

  combined_results <- list()
  factor_names <- unique(unlist(models_info$factors))
  factor_names <- factor_names[!is.na(factor_names) & nzchar(factor_names)]
  errors <- character(0)

  # --- Case 1: no stratification
  if (is.null(models_info$strata)) {
    for (resp in models_info$responses) {
      model_entry <- models_info$models[[resp]]
      if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
        if (!is.null(model_entry$error)) {
          errors <- c(errors, paste0(resp, ": ", model_entry$error))
        }
        next
      }
      outputs <- prepare_anova_outputs(model_entry$model, factor_names)
      if (!is.null(outputs$error)) {
        errors <- c(errors, paste0(resp, ": ", outputs$error))
        next
      }
      anova_obj <- outputs$anova_object
      if (is.null(anova_obj)) {
        errors <- c(errors, paste0(resp, ": ANOVA results are unavailable."))
        next
      }
      tbl <- as.data.frame(anova_obj)
      tbl$Response <- resp
      tbl$Stratum <- "None"
      tbl$Term <- rownames(tbl)
      rownames(tbl) <- NULL
      names(tbl) <- sub(" ", "", names(tbl))
      tbl$PrF <- tbl[, grep("^Pr", names(tbl))[1]]
      combined_results[[length(combined_results) + 1]] <- tbl
    }
  } else {
    # --- Case 2: stratified
    for (stratum in models_info$strata$levels) {
      for (resp in models_info$responses) {
        model_entry <- models_info$models[[stratum]][[resp]]
        if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
          if (!is.null(model_entry$error)) {
            errors <- c(errors, paste0(resp, " (", stratum, "): ", model_entry$error))
          }
          next
        }
        outputs <- prepare_anova_outputs(model_entry$model, factor_names)
        if (!is.null(outputs$error)) {
          errors <- c(errors, paste0(resp, " (", stratum, "): ", outputs$error))
          next
        }
        anova_obj <- outputs$anova_object
        if (is.null(anova_obj)) {
          errors <- c(errors, paste0(resp, " (", stratum, "): ANOVA results are unavailable."))
          next
        }
        tbl <- as.data.frame(anova_obj)
        tbl$Response <- resp
        tbl$Stratum <- stratum
        tbl$Term <- rownames(tbl)
        rownames(tbl) <- NULL
        names(tbl) <- sub(" ", "", names(tbl))
        tbl$PrF <- tbl[, grep("^Pr", names(tbl))[1]]
        combined_results[[length(combined_results) + 1]] <- tbl
      }
    }
  }

  if (length(combined_results) == 0) {
    msg <- "No ANOVA models available to export."
    if (length(errors) > 0) {
      msg <- paste0(
        msg,
        " The following issues were reported:\n",
        paste(sprintf("- %s", unique(errors)), collapse = "\n")
      )
    }
    stop(msg)
  }

  write_anova_docx(combined_results, file)
}

write_anova_docx <- function(results, file) {

  if (is.null(results) || length(results) == 0) stop("No ANOVA results available to export.")
  combined <- bind_rows(results)
  
  required_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF")
  if (!all(required_cols %in% names(combined))) stop("Missing required columns in ANOVA results.")
  
  # Format and sort
  combined <- combined %>%
    mutate(
      SumSq = round(SumSq, 3),
      Fvalue = round(Fvalue, 3),
      PrF_label = ifelse(PrF < 0.001, "<0.001", sprintf("%.3f", PrF)),
      sig = PrF < 0.05
    ) %>%
    arrange(Response, Stratum, Term)
  
  # Hide Stratum column if it's all "None"
  if (length(unique(combined$Stratum)) == 1 && unique(combined$Stratum) == "None") {
    combined$Stratum <- NULL
    visible_cols <- c("Response", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response")
  } else {
    visible_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response", "Stratum")
  }
  
  # Build flextable
  ft <- flextable(combined[, visible_cols])
  
  # Clean header names
  ft <- set_header_labels(
    ft,
    Response = "Response",
    Stratum = if ("Stratum" %in% visible_cols) "Stratum" else NULL,
    Term = "Term",
    SumSq = "Sum Sq",
    Df = "Df",
    Fvalue = "F value",
    PrF_label = "Pr(>F)"
  )
  
  # Merge identical group labels
  ft <- merge_v(ft, j = intersect(merge_cols, ft$col_keys))
  
  # Styling
  ft <- fontsize(ft, part = "all", size = 10)
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- color(ft, part = "header", color = "black")
  ft <- align(ft, align = "center", part = "all")
  
  # Bold significant p-values (< 0.05)
  if ("sig" %in% names(combined)) {
    sig_rows <- which(combined$sig)
    if (length(sig_rows) > 0 && "PrF_label" %in% ft$col_keys) {
      ft <- bold(ft, i = sig_rows, j = "PrF_label", bold = TRUE)
    }
  }
  
  # ===== Journal-style borders =====
  ft <- border_remove(ft)
  black <- fp_border(color = "black", width = 1)
  thin <- fp_border(color = "black", width = 0.5)
  
  # 1) Top line above header
  ft <- border(ft, part = "header", border.top = black)
  # 2) Line below header
  ft <- border(ft, part = "header", border.bottom = black)
  
  # 3) Thin horizontal lines between different responses
  if ("Response" %in% names(combined)) {
    resp_index <- which(diff(as.numeric(factor(combined$Response))) != 0)
    if (length(resp_index) > 0) {
      ft <- border(ft, i = resp_index, part = "body", border.bottom = thin)
    }
  }
  
  # 4) Final bottom border (last line)
  if (nrow(combined) > 0) {
    ft <- border(ft, i = nrow(combined), part = "body", border.bottom = black)
  }
  
  
  # No side or inner borders
  ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
  
  # Write to DOCX
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "")
  doc <- body_add_par(doc, sprintf("Generated by Table Analyzer on %s", Sys.Date()))
  doc <- body_add_par(doc, "Significant p-values (< 0.05) in bold.", style = "Normal")
  print(doc, target = file)
}


# ---------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------

parse_anova_layout_inputs <- function(layout_values) {
  list(
    strata_rows = suppressWarnings(as.numeric(layout_values$strata_rows)),
    strata_cols = suppressWarnings(as.numeric(layout_values$strata_cols)),
    resp_rows   = suppressWarnings(as.numeric(layout_values$resp_rows)),
    resp_cols   = suppressWarnings(as.numeric(layout_values$resp_cols))
  )
}

initialize_anova_plot_context <- function(data, info, layout_values) {
  factor1 <- info$factors$factor1
  factor2 <- info$factors$factor2
  order1 <- info$orders$order1
  order2 <- info$orders$order2

  if (!is.null(factor1) && !is.null(order1) && factor1 %in% names(data)) {
    data[[factor1]] <- factor(data[[factor1]], levels = order1)
  }
  if (!is.null(factor2) && !is.null(order2) && factor2 %in% names(data)) {
    data[[factor2]] <- factor(data[[factor2]], levels = order2)
  }

  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else character(0)

  if (has_strata && (is.null(strata_levels) || length(strata_levels) == 0) &&
      !is.null(strat_var) && strat_var %in% names(data)) {
    strata_levels <- unique(as.character(stats::na.omit(data[[strat_var]])))
  }

  layout_input <- parse_anova_layout_inputs(layout_values)

  n_expected_strata <- if (has_strata) max(1L, length(strata_levels)) else 1L
  strata_defaults <- if (has_strata) {
    compute_default_grid(n_expected_strata)
  } else {
    list(rows = 1L, cols = 1L)
  }
  strata_layout <- basic_grid_layout(
    rows = layout_input$strata_rows,
    cols = layout_input$strata_cols,
    default_rows = strata_defaults$rows,
    default_cols = strata_defaults$cols
  )

  list(
    data = data,
    responses = info$responses,
    factor1 = factor1,
    factor2 = factor2,
    order1 = order1,
    order2 = order2,
    has_strata = has_strata,
    strat_var = strat_var,
    strata_levels = strata_levels,
    n_expected_strata = n_expected_strata,
    strata_defaults = strata_defaults,
    strata_layout = strata_layout,
    layout_input = layout_input,
    initial_strata_panels = if (has_strata) 0L else 1L
  )
}

anova_summarise_stats <- function(df_subset, resp_name, factor1, factor2) {
  if (is.null(factor1) || !factor1 %in% names(df_subset)) {
    return(tibble::tibble())
  }

  if (is.null(factor2) || !factor2 %in% names(df_subset)) {
    df_subset |>
      dplyr::group_by(.data[[factor1]]) |>
      dplyr::summarise(
        mean = mean(.data[[resp_name]], na.rm = TRUE),
        se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
        .groups = "drop"
      )
  } else {
    df_subset |>
      dplyr::group_by(.data[[factor1]], .data[[factor2]]) |>
      dplyr::summarise(
        mean = mean(.data[[resp_name]], na.rm = TRUE),
        se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
        .groups = "drop"
      )
  }
}

apply_anova_factor_levels <- function(stats_df, factor1, factor2, order1, order2) {
  if (!is.null(factor1) && factor1 %in% names(stats_df)) {
    if (!is.null(order1)) {
      stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]), levels = order1)
    } else {
      stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]))
    }
  }

  if (!is.null(factor2) && factor2 %in% names(stats_df)) {
    levels2 <- if (!is.null(order2)) {
      order2
    } else {
      unique(as.character(stats_df[[factor2]]))
    }
    stats_df[[factor2]] <- factor(as.character(stats_df[[factor2]]), levels = levels2)
  }

  stats_df
}

finalize_anova_plot_result <- function(response_plots,
                                       context,
                                       strata_panel_count,
                                       collect_guides = FALSE) {
  if (length(response_plots) == 0) {
    return(NULL)
  }

  has_strata <- context$has_strata
  strata_layout <- context$strata_layout

  if (has_strata && strata_panel_count == 0L) {
    strata_panel_count <- context$n_expected_strata
  }

  if (has_strata) {
    strata_layout <- adjust_grid_layout(max(1L, strata_panel_count), strata_layout)
  }

  response_defaults <- compute_default_grid(length(response_plots))
  response_layout <- basic_grid_layout(
    rows = context$layout_input$resp_rows,
    cols = context$layout_input$resp_cols,
    default_rows = response_defaults$rows,
    default_cols = response_defaults$cols
  )
  response_layout <- adjust_grid_layout(length(response_plots), response_layout)

  strata_validation <- if (has_strata) {
    validate_grid(max(1L, strata_panel_count), strata_layout$nrow, strata_layout$ncol)
  } else {
    list(valid = TRUE, message = NULL)
  }

  response_validation <- validate_grid(
    length(response_plots),
    response_layout$nrow,
    response_layout$ncol
  )

  warnings <- c()
  if (has_strata && !strata_validation$valid && !is.null(strata_validation$message)) {
    warnings <- c(warnings, strata_validation$message)
  }
  if (!response_validation$valid && !is.null(response_validation$message)) {
    warnings <- c(warnings, response_validation$message)
  }
  warning_text <- if (length(warnings) > 0) paste(warnings, collapse = "<br/>") else NULL

  final_plot <- NULL
  if (is.null(warning_text)) {
    if (length(response_plots) == 1) {
      final_plot <- response_plots[[1]]
    } else {
      combined <- patchwork::wrap_plots(
        plotlist = response_plots,
        nrow = response_layout$nrow,
        ncol = response_layout$ncol
      )
      final_plot <- if (collect_guides) {
        combined & patchwork::plot_layout(guides = "collect")
      } else {
        combined
      }
    }
  }

  list(
    plot = final_plot,
    layout = list(
      strata = list(
        rows = if (has_strata) strata_layout$nrow else 1L,
        cols = if (has_strata) strata_layout$ncol else 1L
      ),
      responses = list(
        rows = response_layout$nrow,
        cols = response_layout$ncol
      )
    ),
    warning = warning_text,
    defaults = list(
      strata = context$strata_defaults,
      responses = response_defaults
    )
  )
}

build_line_plot_panel <- function(stats_df, title_text, y_limits, factor1, factor2, line_colors) {
  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    color_value <- if (!is.null(line_colors) && length(line_colors) > 0) {
      unname(line_colors)[1]
    } else {
      resolve_single_color()
    }
    p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
      geom_line(aes(group = 1), color = color_value, linewidth = 1) +
      geom_point(size = 3, color = color_value) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        color = color_value
      ) +
      theme_minimal(base_size = 14) +
      labs(x = factor1, y = "Mean ± SE") +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      )
  } else {
    group_levels <- if (is.factor(stats_df[[factor2]])) {
      levels(stats_df[[factor2]])
    } else {
      unique(as.character(stats_df[[factor2]]))
    }
    group_levels <- group_levels[!is.na(group_levels)]
    palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
    stats_df[[factor2]] <- factor(as.character(stats_df[[factor2]]), levels = group_levels)
    p <- ggplot(stats_df, aes(
      x = !!sym(factor1),
      y = mean,
      color = !!sym(factor2),
      group = !!sym(factor2)
    )) +
      geom_line(linewidth = 1) +
      geom_point(size = 3) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15
      ) +
      theme_minimal(base_size = 14) +
      labs(
        x = factor1,
        y = "Mean ± SE",
        color = factor2
      ) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
      ) +
      scale_color_manual(values = palette)
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  p + ggtitle(title_text) +
    theme(plot.title = element_text(size = 12, face = "bold"))
}

plot_anova_lineplot_meanse <- function(data, info, layout_values, line_colors = NULL) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels

  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_stats <- list()
      y_values <- c()

      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) {
          next
        }

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) {
          next
        }

        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        y_values <- c(y_values, stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
        stratum_stats[[stratum]] <- stats_df
      }

      if (length(stratum_stats) == 0) {
        next
      }

      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }

      strata_panel_count <- max(strata_panel_count, length(stratum_stats))

      strata_plot_list <- lapply(names(stratum_stats), function(stratum_name) {
        build_line_plot_panel(
          stats_df = stratum_stats[[stratum_name]],
          title_text = stratum_name,
          y_limits = y_limits,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors
        )
      })

      current_layout <- adjust_grid_layout(length(stratum_stats), context$strata_layout)

      combined <- patchwork::wrap_plots(
        plotlist = strata_plot_list,
        nrow = current_layout$nrow,
        ncol = current_layout$ncol
      )

      title_plot <- ggplot() +
        theme_void() +
        ggtitle(resp) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.margin = margin(t = 0, r = 0, b = 6, l = 0)
        )

      response_plots[[resp]] <- title_plot / combined + plot_layout(heights = c(0.08, 1))
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) {
        next
      }

      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }

      response_plots[[resp]] <- build_line_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        y_limits = y_limits,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors
      )
    }
  }

  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = TRUE
  )
}

build_bar_plot_panel <- function(stats_df,
                                 title_text,
                                 factor1,
                                 factor2,
                                 line_colors,
                                 base_fill,
                                 signif_df = NULL,
                                 show_value_labels = FALSE) {
  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
      geom_col(fill = base_fill, width = 0.6, alpha = 0.8) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        color = "gray40",
        linewidth = 0.5
      ) +
      theme_minimal(base_size = 14) +
      labs(
        x = factor1,
        y = "Mean ± SE",
        title = title_text
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 6)),
        axis.title.y = element_text(margin = margin(r = 6)),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),
        axis.text.x = element_text(angle = 30, hjust = 1)
      )

    if (isTRUE(show_value_labels)) {
      label_df <- stats_df |>
        dplyr::mutate(
          .se = dplyr::coalesce(se, 0),
          label_text = format_numeric_labels(mean),
          label_y = ifelse(mean >= 0, mean + .se, mean - .se),
          label_vjust = ifelse(mean >= 0, -0.4, 1.2)
        )

      plot_obj <- plot_obj +
        geom_text(
          data = label_df,
          aes(x = !!sym(factor1), y = label_y, label = label_text, vjust = label_vjust),
          color = "gray20",
          size = 3.8,
          fontface = "bold",
          inherit.aes = FALSE
        ) +
        scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
    }

    if (!is.null(signif_df) && nrow(signif_df) > 0) {
      signif_df <- signif_df |>
        dplyr::filter(p.value < 0.05)
      if (nrow(signif_df) > 0) {
        max_y <- suppressWarnings(max(stats_df$mean + stats_df$se, na.rm = TRUE))
        if (!is.finite(max_y)) {
          max_y <- 0
        }
        step <- if (isTRUE(all.equal(max_y, 0))) 0.1 else abs(max_y) * 0.08
        start <- max_y + step
        signif_df <- signif_df |>
          dplyr::mutate(
            y_position = seq(
              from = start,
              by = step,
              length.out = dplyr::n()
            ),
            label = dplyr::case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01 ~ "**",
              p.value < 0.05 ~ "*",
              TRUE ~ sprintf("p=%.3f", p.value)
            ),
            xmin = group1,
            xmax = group2,
            .group_id = seq_len(dplyr::n())
          )

        plot_obj <- plot_obj + ggsignif::geom_signif(
          data = signif_df,
          aes(
            xmin = xmin,
            xmax = xmax,
            annotations = label,
            y_position = y_position,
            group = .group_id
          ),
          manual = TRUE,
          inherit.aes = FALSE,
          tip_length = 0.01,
          textsize = 3.8,
          vjust = 0.5,
          color = "gray30"
        )
      }
    }

    return(plot_obj)
  }

  group_levels <- if (is.factor(stats_df[[factor2]])) {
    levels(stats_df[[factor2]])
  } else {
    unique(as.character(stats_df[[factor2]]))
  }
  group_levels <- group_levels[!is.na(group_levels)]
  palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
  dodge <- position_dodge(width = 0.7)

  plot_obj <- ggplot(stats_df, aes(
    x = !!sym(factor1),
    y = mean,
    fill = !!sym(factor2)
  )) +
    geom_col(position = dodge, width = 0.6, alpha = 0.85) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.2,
      color = "gray40",
      linewidth = 0.5
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = factor1,
      y = "Mean ± SE",
      fill = factor2,
      title = title_text
    ) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray90"),
      axis.text.x = element_text(angle = 30, hjust = 1)
    ) +
    scale_fill_manual(values = palette)

  if (isTRUE(show_value_labels)) {
    label_df <- stats_df |>
      dplyr::mutate(
        .se = dplyr::coalesce(se, 0),
        label_text = format_numeric_labels(mean),
        label_y = ifelse(mean >= 0, mean + .se, mean - .se),
        label_vjust = ifelse(mean >= 0, -0.4, 1.2)
      )

    plot_obj <- plot_obj +
      geom_text(
        data = label_df,
        aes(
          x = !!sym(factor1),
          y = label_y,
          label = label_text,
          vjust = label_vjust,
          fill = NULL,
          group = !!sym(factor2)
        ),
        position = dodge,
        color = "gray20",
        size = 3.6,
        fontface = "bold",
        inherit.aes = FALSE
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
  }

  if (!is.null(signif_df) && nrow(signif_df) > 0) {
    signif_df <- signif_df |>
      dplyr::filter(p.value < 0.05)

    if (nrow(signif_df) > 0) {
      if (!factor1 %in% names(signif_df)) {
        signif_df[[factor1]] <- "__overall__"
      }

      lookup <- stats_df |>
        dplyr::mutate(
          .factor1 = as.character(.data[[factor1]]),
          .factor2 = as.character(.data[[factor2]]),
          .x_index = as.numeric(.data[[factor1]]),
          .group_index = as.numeric(.data[[factor2]]),
          .ymax = mean + se
        )

      n_groups <- length(group_levels)
      lookup <- lookup |>
        dplyr::mutate(
          .x_offset = dodge$width * ((.group_index - 0.5) / max(1, n_groups) - 0.5),
          .xpos = .x_index + .x_offset
        )

      signif_split <- split(signif_df, signif_df[[factor1]])
      annotations <- lapply(names(signif_split), function(level_name) {
        subset_df <- signif_split[[level_name]]
        if (nrow(subset_df) == 0) {
          return(NULL)
        }

        level_lookup <- lookup
        if (!identical(level_name, "__overall__") && !is.na(level_name)) {
          level_lookup <- dplyr::filter(level_lookup, .factor1 == level_name)
        }

        if (nrow(level_lookup) == 0) {
          return(NULL)
        }

        base_max <- suppressWarnings(max(level_lookup$.ymax, na.rm = TRUE))
        if (!is.finite(base_max)) {
          base_max <- suppressWarnings(max(lookup$.ymax, na.rm = TRUE))
        }
        if (!is.finite(base_max)) {
          base_max <- 0
        }

        step <- if (isTRUE(all.equal(base_max, 0))) 0.1 else abs(base_max) * 0.08
        start <- base_max + step

        subset_df <- subset_df |>
          dplyr::mutate(
            label = dplyr::case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01 ~ "**",
              p.value < 0.05 ~ "*",
              TRUE ~ sprintf("p=%.3f", p.value)
            ),
            xmin = vapply(group1, function(g) {
              vals <- level_lookup$.xpos[level_lookup$.factor2 == g]
              if (length(vals) == 0) NA_real_ else vals[1]
            }, numeric(1)),
            xmax = vapply(group2, function(g) {
              vals <- level_lookup$.xpos[level_lookup$.factor2 == g]
              if (length(vals) == 0) NA_real_ else vals[1]
            }, numeric(1)),
            y_position = seq(
              from = start,
              by = step,
              length.out = dplyr::n()
            ),
            .group_id = seq_len(dplyr::n())
          )

        subset_df
      })

      annotations <- annotations[!vapply(annotations, is.null, logical(1))]

      if (length(annotations) > 0) {
        annotations <- dplyr::bind_rows(annotations)
        annotations <- annotations |>
          dplyr::filter(!is.na(xmin), !is.na(xmax))
        if (nrow(annotations) > 0) {
          annotations <- annotations |>
            dplyr::mutate(.group_id = seq_len(dplyr::n()))
        }

        if (nrow(annotations) > 0) {
          plot_obj <- plot_obj + ggsignif::geom_signif(
            data = annotations,
            aes(
              xmin = xmin,
              xmax = xmax,
              annotations = label,
              y_position = y_position,
              group = .group_id
            ),
            manual = TRUE,
            inherit.aes = FALSE,
            tip_length = 0.01,
            textsize = 3.8,
            vjust = 0.5,
            color = "gray30"
          )
        }
      }
    }
  }

  plot_obj
}

prepare_barplot_significance <- function(posthoc_entry, factor1, factor2, stats_df) {
  if (is.null(posthoc_entry)) {
    return(NULL)
  }

  if (is.list(posthoc_entry) && !is.data.frame(posthoc_entry)) {
    if (!is.null(posthoc_entry$table)) {
      return(prepare_barplot_significance(posthoc_entry$table, factor1, factor2, stats_df))
    }

    if (!is.null(factor1) && !is.null(posthoc_entry[[factor1]])) {
      return(prepare_barplot_significance(posthoc_entry[[factor1]], factor1, factor2, stats_df))
    }

    nested <- lapply(posthoc_entry, function(x) {
      prepare_barplot_significance(x, factor1, factor2, stats_df)
    })
    nested <- nested[!vapply(nested, is.null, logical(1))]
    if (length(nested) == 0) {
      return(NULL)
    }

    combined <- dplyr::bind_rows(nested)
    if (nrow(combined) == 0) {
      return(NULL)
    }
    return(combined)
  }

  if (!is.data.frame(posthoc_entry)) {
    return(NULL)
  }

  df <- posthoc_entry
  if ("Factor" %in% names(df) && !is.null(factor2)) {
    df <- df[df$Factor %in% c(
      factor2,
      paste(factor1, factor2, sep = ":"),
      paste(factor2, factor1, sep = ":")
    ), , drop = FALSE]
  }

  if (nrow(df) == 0) {
    return(NULL)
  }

  factor1_levels <- NULL
  if (!is.null(factor1) && factor1 %in% names(stats_df)) {
    factor1_levels <- levels(stats_df[[factor1]])
    if (is.null(factor1_levels)) {
      factor1_levels <- unique(as.character(stats_df[[factor1]]))
    }
  }

  if (!is.null(factor1) && factor1 %in% names(df)) {
    split_df <- split(df, df[[factor1]])
    annotations <- lapply(names(split_df), function(level_name) {
      subset_df <- split_df[[level_name]]
      signif_tbl <- extract_tukey_for_signif(subset_df)
      if (is.null(signif_tbl) || nrow(signif_tbl) == 0) {
        return(NULL)
      }
      signif_tbl[[factor1]] <- level_name
      signif_tbl
    })
    annotations <- annotations[!vapply(annotations, is.null, logical(1))]
    if (length(annotations) == 0) {
      return(NULL)
    }
    combined <- dplyr::bind_rows(annotations)
    if (!is.null(factor1_levels) && factor1 %in% names(combined)) {
      combined[[factor1]] <- factor(combined[[factor1]], levels = factor1_levels)
    }
    return(combined)
  }

  signif_df <- extract_tukey_for_signif(df)
  if (!is.null(signif_df) && nrow(signif_df) > 0 && !is.null(factor1)) {
    signif_df[[factor1]] <- "__overall__"
  }
  signif_df
}

plot_anova_barplot_meanse <- function(data,
                                      info,
                                      layout_values = list(),
                                      line_colors = NULL,
                                      posthoc_all = NULL,
                                      show_value_labels = FALSE) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  if (is.null(factor1) || length(context$responses) == 0) {
    return(NULL)
  }

  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }

  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels

  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_plots <- list()

      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) {
          next
        }

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) {
          next
        }

        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        posthoc_entry <- NULL
        if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
          resp_posthoc <- posthoc_all[[resp]]
          if (is.list(resp_posthoc) && !is.data.frame(resp_posthoc)) {
            posthoc_entry <- resp_posthoc[[stratum]]
          } else {
            posthoc_entry <- resp_posthoc
          }
        }
        signif_df <- prepare_barplot_significance(posthoc_entry, factor1, factor2, stats_df)

        stratum_plots[[stratum]] <- build_bar_plot_panel(
          stats_df = stats_df,
          title_text = stratum,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_fill = base_fill,
          signif_df = signif_df,
          show_value_labels = show_value_labels
        )
      }

      if (length(stratum_plots) > 0) {
        strata_panel_count <- max(strata_panel_count, length(stratum_plots))
        current_layout <- adjust_grid_layout(length(stratum_plots), context$strata_layout)
        combined <- patchwork::wrap_plots(
          plotlist = stratum_plots,
          nrow = current_layout$nrow,
          ncol = current_layout$ncol
        )

        title_plot <- ggplot() +
          theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) {
        next
      }

      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      posthoc_entry <- NULL
      if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
        posthoc_entry <- posthoc_all[[resp]]
      }
      signif_df <- prepare_barplot_significance(posthoc_entry, factor1, factor2, stats_df)

      response_plots[[resp]] <- build_bar_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        signif_df = signif_df,
        show_value_labels = show_value_labels
      )
    }
  }

  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = FALSE
  )
}

# ---------------------------------------------------------------
# Low-level utilities
# ---------------------------------------------------------------
sanitize_name <- function(name) {
  safe <- gsub("[^A-Za-z0-9]+", "_", name)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

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


