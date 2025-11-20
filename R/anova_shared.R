#### Table Analyzer — Shared ANOVA Module  ####
#### Section: UI & Output Binding ####

build_anova_layout_controls <- function(ns, input, info) {
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  n_responses <- if (!is.null(info$responses)) length(info$responses) else 0

  build_grid_section <- function(title, grid_id, row_help, col_help) {
    tagList(
      h5(title),
      plot_grid_ui(
        id = ns(grid_id),
        rows_help = row_help,
        cols_help = col_help
      )
    )
  }

  strata_inputs <- if (has_strata) {
    build_grid_section(
      title = "Across strata:",
      grid_id = "strata_grid",
      row_help = "Set how many rows of plots to use when displaying different strata.",
      col_help = "Set how many columns of plots to use when displaying different strata."
    )
  } else {
    NULL
  }

  response_inputs <- if (!is.null(n_responses) && n_responses > 1) {
    build_grid_section(
      title = "Across responses:",
      grid_id = "response_grid",
      row_help = "Set the number of plot rows when multiple responses are shown together.",
      col_help = "Set the number of plot columns when multiple responses are shown together."
    )
  } else {
    NULL
  }

  tagList(strata_inputs, response_inputs)
}


# ---------------------------------------------------------------
# Formula utilities
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

print_anova_summary_and_posthoc <- function(model_entry, factors) {
  if (is.null(model_entry) || (is.list(model_entry) && is.null(model_entry$model))) {
    cat("Model is not available.\n")
    return(invisible(NULL))
  }

  if (!is.null(model_entry$error)) {
    cat(format_safe_error_message("Model fitting failed", model_entry$error), "\n", sep = "")
    return(invisible(NULL))
  }

  model_obj <- model_entry$model
  results <- prepare_anova_outputs(model_obj, factors)
  if (!is.null(results$error)) {
    cat(format_safe_error_message("ANOVA computation failed", results$error), "\n", sep = "")
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
        cat(
          "\n",
          format_safe_error_message(
            paste("Post-hoc Tukey comparisons for", factor_nm, "failed"),
            details$error
          ),
          "\n",
          sep = ""
        )
      } else if (!is.null(details$table)) {
        cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
        print(details$table)
      }
    }
  }
  invisible(results)
}

#### Section: Model Fitting & Preparation ####

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

  set_factor_levels <- function(data, var, levels = NULL, default_factor = FALSE) {
    if (is.null(var) || !var %in% names(data)) return(data)
    if (!is.null(levels)) {
      data[[var]] <- factor(as.character(data[[var]]), levels = levels)
    } else if (default_factor) {
      data[[var]] <- as.factor(as.character(data[[var]]))
    }
    data
  }

  df <- df |>
    set_factor_levels(factor1_var, factor1_order) |>
    set_factor_levels(factor2_var, factor2_order) |>
    set_factor_levels(stratify_var, strata_order, default_factor = TRUE)

  strata <- if (!is.null(stratify_var) && stratify_var %in% names(df)) {
    levels(df[[stratify_var]])
  } else {
    NULL
  }

  factor1_rhs <- anova_protect_vars(factor1_var)
  factor2_rhs <- anova_protect_vars(factor2_var)

  rhs <- switch(
    model,
    oneway_anova = factor1_rhs,
    twoway_anova = if (length(factor1_rhs) > 0 && length(factor2_rhs) > 0) {
      paste(factor1_rhs, factor2_rhs, sep = " *")
    } else {
      factor1_rhs
    },
    factor1_rhs
  )
  rhs <- if (is.null(rhs) || rhs == "") "1" else rhs

  build_formula <- function(resp) stats::as.formula(paste(anova_protect_vars(resp), "~", rhs))
  safe_fit <- purrr::safely(function(fml, data) stats::aov(fml, data = data))

  fit_models_for_data <- function(data) {
    lapply(responses, function(resp) {
      fit_result <- safe_fit(build_formula(resp), data)
      list(
        model = fit_result$result,
        error = if (!is.null(fit_result$error)) conditionMessage(fit_result$error) else NULL
      )
    }) |>
      stats::setNames(responses)
  }

  base_info <- list(
    type = model,
    responses = responses,
    factors = list(factor1 = factor1_var, factor2 = factor2_var),
    orders = list(order1 = factor1_order, order2 = factor2_order),
    data_used = df
  )

  if (is.null(strata)) {
    return(c(base_info, list(models = fit_models_for_data(df), strata = NULL)))
  }

  models <- lapply(strata, function(s) {
    subset_rows <- df[[stratify_var]] == s & !is.na(df[[stratify_var]])
    fit_models_for_data(df[subset_rows, , drop = FALSE])
  })
  names(models) <- strata

  c(base_info, list(models = models, strata = list(var = stratify_var, levels = strata)))
}

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

#### Section: ANOVA Output Processing ####

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
  
  # --- round numeric columns and keep raw p-values ---
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
    names(anova_df)[names(anova_df) == p_col] <- "p.value"
  } else {
    anova_df$p.value <- NA_real_
  }
  
  # --- Post-hoc Tukey for each factor ---
  factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
  posthoc_details <- list()
  posthoc_combined <- NULL
  posthoc_significant <- numeric(0)
  
  # --- Post-hoc Tukey (one-way or two-way specific) ---
    if (length(factor_names) == 1) {
      f1 <- factor_names[1]
      f1_spec <- anova_protect_vars(f1)
      if (f1 %in% names(model_obj$model)) {
        res <- tryCatch({
          emm <- emmeans::emmeans(model_obj, specs = as.formula(paste("~", f1_spec)))
          contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
          as.data.frame(summary(contrasts))
        }, error = function(e) list(error = e$message))
      
      if (is.data.frame(res)) {
        res$Factor <- f1
        posthoc_details[[f1]] <- list(table = res, error = NULL)
        posthoc_combined <- res
      } else {
        posthoc_details[[f1]] <- list(table = NULL, error = res$error)
      }
    }
    
  } else if (length(factor_names) == 2) {
    f1 <- factor_names[1]
    f2 <- factor_names[2]

    f1_spec <- anova_protect_vars(f1)
    f2_spec <- anova_protect_vars(f2)
    
    # --- main-effect Tukey for both factors (averaged) ---
    for (ff in c(f1, f2)) {
      if (ff %in% names(model_obj$model)) {
        ff_spec <- anova_protect_vars(ff)
        res_main <- tryCatch({
          emm_main <- emmeans::emmeans(model_obj, specs = as.formula(paste("~", ff_spec)))
          contrasts_main <- emmeans::contrast(emm_main, method = "pairwise", adjust = "tukey")
          as.data.frame(summary(contrasts_main))
        }, error = function(e) list(error = e$message))
        
        if (is.data.frame(res_main)) {
          res_main$Factor <- ff
          posthoc_details[[ff]] <- list(table = res_main, error = NULL)
          posthoc_combined <- dplyr::bind_rows(posthoc_combined, res_main)
        } else {
          posthoc_details[[ff]] <- list(table = NULL, error = res_main$error)
        }
      }
    }

    # --- nested contrasts of factor2 within each level of factor1 ---
    res_nested <- tryCatch({
      formula_nested <- as.formula(paste("pairwise ~", f2_spec, "|", f1_spec))
      emm_nested <- emmeans::emmeans(model_obj, specs = formula_nested, adjust = "tukey")
      contrasts_df <- as.data.frame(summary(emm_nested$contrasts))
      contrasts_df$Factor <- paste0(f2, "_within_", f1)
      contrasts_df[[f1]] <- as.character(contrasts_df[[f1]])
      contrasts_df
    }, error = function(e) list(error = e$message))
    
    if (is.data.frame(res_nested)) {
      posthoc_details[[paste0(f2, "_within_", f1)]] <- list(table = res_nested, error = NULL)
      posthoc_combined <- dplyr::bind_rows(posthoc_combined, res_nested)
    } else {
      posthoc_details[[paste0(f2, "_within_", f1)]] <- list(table = NULL, error = res_nested$error)
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

#### Section: Plot Context Initialization ####

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

parse_anova_layout_inputs <- function(layout_values) {
  list(
    strata_rows = suppressWarnings(as.numeric(layout_values$strata_rows)),
    strata_cols = suppressWarnings(as.numeric(layout_values$strata_cols)),
    resp_rows   = suppressWarnings(as.numeric(layout_values$resp_rows)),
    resp_cols   = suppressWarnings(as.numeric(layout_values$resp_cols))
  )
}

finalize_anova_plot_result <- function(response_plots,
                                       context,
                                       strata_panel_count,
                                       collect_guides = FALSE,
                                       legend_position = NULL) {
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

  panel_counts <- list(
    strata = if (has_strata) max(1L, strata_panel_count) else 1L,
    responses = length(response_plots)
  )

  final_plot <- NULL
  if (is.null(warning_text)) {
    if (length(response_plots) == 1) {
      final_plot <- response_plots[[1]]
      if (collect_guides && !is.null(legend_position)) {
        final_plot <- final_plot & theme(legend.position = legend_position)
      }
    } else {
      final_plot <- patchwork::wrap_plots(
        plotlist = response_plots,
        nrow = response_layout$nrow,
        ncol = response_layout$ncol
      )
    }
    if (collect_guides || !is.null(legend_position)) {
      final_plot <- apply_common_legend_layout(
        final_plot,
        legend_position = legend_position,
        collect_guides = collect_guides
      )
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
    panel_counts = panel_counts,
    defaults = list(
      strata = context$strata_defaults,
      responses = response_defaults
    )
  )
}

apply_common_legend_layout <- function(plot_obj,
                                       legend_position = NULL,
                                       collect_guides = FALSE) {
  if (is.null(plot_obj)) return(plot_obj)

  updated <- plot_obj
  if (collect_guides) {
    updated <- collect_guides_safe(updated)
  }

  if (!is.null(legend_position)) {
    updated <- add_theme_to_plot(updated, theme(legend.position = legend_position))
  }

  updated
}

collect_guides_safe <- function(plot_obj) {
  if (is.null(plot_obj) || !requireNamespace("patchwork", quietly = TRUE)) {
    return(plot_obj)
  }

  is_patchwork <- inherits(plot_obj, "patchwork")
  if (!is_patchwork) {
    return(plot_obj)
  }

  exports <- tryCatch(getNamespaceExports("patchwork"), error = function(...) character())
  collected <- if ("collect_guides" %in% exports) {
    patchwork::collect_guides(plot_obj)
  } else {
    plot_obj + patchwork::plot_layout(guides = "collect")
  }

  collected + patchwork::plot_layout(guides = "collect")
}

add_theme_to_plot <- function(plot_obj, theme_obj) {
  if (inherits(plot_obj, "patchwork")) {
    plot_obj & theme_obj
  } else {
    plot_obj + theme_obj
  }
}

#### Section: Statistics Summarisation ####

compute_lineplot_shared_limits <- function(context, data, factor1, factor2) {
  combined <- NULL

  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next

        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
        combined <- update_numeric_range(combined, y_values)
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
      combined <- update_numeric_range(combined, y_values)
    }
  }

  if (is.null(combined) || any(!is.finite(combined))) return(NULL)
  combined
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

#### Section: Barplot Construction ####

plot_anova_barplot_meanse <- function(data,
                                      info,
                                      layout_values = list(),
                                      line_colors = NULL,
                                      show_value_labels = FALSE,
                                      base_size = 14,
                                      posthoc_all = NULL,
                                      share_y_axis = FALSE,
                                      common_legend = FALSE,
                                      legend_position = NULL) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  allowed_positions <- c("bottom", "top", "left", "right")
  legend_position_value <- if (!is.null(legend_position) && legend_position %in% allowed_positions) {
    legend_position
  } else {
    "bottom"
  }

  if (is.null(factor1) || length(context$responses) == 0) {
    return(NULL)
  }

  shared_y_limits <- if (isTRUE(share_y_axis)) {
    compute_barplot_shared_limits(
      context,
      data,
      factor1,
      factor2,
      posthoc_all,
      show_value_labels
    )
  } else {
    NULL
  }

  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }
  
  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels
  
  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }
    
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_plots <- list()
      
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next
        
        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        
        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }
        
        stratum_plots[[stratum]] <- build_bar_plot_panel(
          stats_df = stats_df,
          title_text = stratum,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_fill = base_fill,
          show_value_labels = show_value_labels,
          base_size = base_size,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc,
          y_limits = shared_y_limits
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
          ta_plot_theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = base_size, face = "bold", hjust = 0.5))

        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      
      response_plots[[resp]] <- build_bar_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        show_value_labels = show_value_labels,
        base_size = base_size,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry,
        y_limits = shared_y_limits
      )
    }
  }
  
  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = isTRUE(common_legend),
    legend_position = if (isTRUE(common_legend)) legend_position_value else NULL
  )
}


# ---------------------------------------------------------------
# Low-level utilities
# ---------------------------------------------------------------

compute_barplot_shared_limits <- function(context,
                                          data,
                                          factor1,
                                          factor2,
                                          posthoc_all = NULL,
                                          show_value_labels = FALSE) {
  combined <- NULL

  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }

    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }

        rng <- compute_barplot_panel_range(
          stats_df, factor1, factor2,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc
        )
        combined <- update_numeric_range(combined, rng)
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

      rng <- compute_barplot_panel_range(
        stats_df, factor1, factor2,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry
      )
      combined <- update_numeric_range(combined, rng)
    }
  }

  if (is.null(combined)) return(NULL)
  upper_padding <- if (isTRUE(show_value_labels)) 0.18 else 0.12

  limits <- expand_axis_limits(combined, lower_mult = 0.05, upper_mult = upper_padding)
  ensure_barplot_zero_baseline(limits)
}

compute_barplot_panel_range <- function(stats_df,
                                        factor1,
                                        factor2,
                                        posthoc_entry = NULL,
                                        nested_posthoc = NULL) {
  if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)
  values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NULL)
  rng <- range(values)
  max_val <- rng[2]

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    prep <- prepare_significance_annotations_data(stats_df, factor1, posthoc_entry)
  } else {
    prep <- prepare_nested_significance_annotations_data(stats_df, factor1, factor2, nested_posthoc)
  }

  if (!is.null(prep) && !is.null(prep$max_y) && is.finite(prep$max_y)) {
    max_val <- max(max_val, prep$max_y)
  }

  c(rng[1], max_val)
}

build_bar_plot_panel <- function(stats_df,
                                 title_text,
                                 factor1,
                                 factor2,
                                 line_colors,
                                 base_fill,
                                 show_value_labels = FALSE,
                                 base_size = 14,
                                 posthoc_entry = NULL,
                                 nested_posthoc = NULL,
                                 y_limits = NULL) {

  # Compute per-panel limits when not sharing axes so we can always anchor at zero
  if (is.null(y_limits)) {
    panel_range <- compute_barplot_panel_range(
      stats_df,
      factor1,
      factor2,
      posthoc_entry = posthoc_entry,
      nested_posthoc = nested_posthoc
    )

    if (!is.null(panel_range)) {
      y_limits <- expand_axis_limits(panel_range, lower_mult = 0, upper_mult = 0.12)
      y_limits <- ensure_barplot_zero_baseline(y_limits)
    }
  }

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    return(
      build_single_factor_barplot(
        stats_df,
        title_text,
        factor1,
        base_fill,
        show_value_labels,
        base_size,
        posthoc_entry,
        y_limits = y_limits
      )
    )
  }

  build_two_factor_barplot(
    stats_df,
    title_text,
    factor1,
    factor2,
    line_colors,
    base_fill,
    show_value_labels,
    base_size,
    nested_posthoc,
    y_limits = y_limits
  )
}

# ===============================================================
# 🔹 Helper: Single-factor barplot (one-way ANOVA)
# ===============================================================

build_single_factor_barplot <- function(stats_df,
                                        title_text,
                                        factor1,
                                        base_fill,
                                        show_value_labels,
                                        base_size,
                                        posthoc_entry,
                                        y_limits = NULL) {
  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)

  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
    geom_col(fill = base_fill, width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.15,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    )

  expand_scale <- is.null(y_limits)

  if (isTRUE(show_value_labels)) {
    plot_obj <- add_bar_value_labels(
      plot_obj, stats_df, factor1, format_numeric_labels, base_size,
      expand_scale = expand_scale
    )
  }

  if (!is.null(posthoc_entry)) {
    plot_obj <- add_significance_annotations(
      plot_obj, stats_df, factor1, posthoc_entry,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Two-factor grouped barplot (two-way ANOVA)
# ===============================================================

add_bar_value_labels <- function(plot_obj,
                                stats_df,
                                factor1,
                                format_numeric_labels,
                                base_size,
                                expand_scale = TRUE) {
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
      size = compute_label_text_size(base_size),
      fontface = "bold",
      inherit.aes = FALSE
    )

  if (isTRUE(expand_scale)) {
    plot_obj <- plot_obj + scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Add value labels to grouped (two-factor) barplots
# ===============================================================

build_two_factor_barplot <- function(stats_df,
                                     title_text,
                                     factor1,
                                     factor2,
                                     line_colors,
                                     base_fill,
                                     show_value_labels,
                                     base_size,
                                     nested_posthoc = NULL,
                                     y_limits = NULL) {

  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)
  
  group_levels <- if (is.factor(stats_df[[factor2]])) {
    levels(stats_df[[factor2]])
  } else {
    unique(as.character(stats_df[[factor2]]))
  }
  group_levels <- group_levels[!is.na(group_levels)]
  palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
  dodge <- position_dodge(width = 0.7)
  
  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean, fill = !!sym(factor2))) +
    geom_col(position = dodge, width = 0.6, alpha = 0.85) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.2,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", fill = factor2, title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    ) +
    scale_fill_manual(values = palette)

  expand_scale <- is.null(y_limits)

  if (isTRUE(show_value_labels)) {
    plot_obj <- add_grouped_bar_value_labels(
      plot_obj, stats_df, factor1, factor2,
      format_numeric_labels, dodge, base_size,
      expand_scale = expand_scale
    )
  }

  if (!is.null(nested_posthoc)) {
    plot_obj <- add_nested_significance_annotations(
      plot_obj, stats_df, factor1, factor2, nested_posthoc,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Add value labels to single-factor barplots
# ===============================================================

add_grouped_bar_value_labels <- function(plot_obj,
                                         stats_df,
                                         factor1,
                                         factor2,
                                         format_numeric_labels,
                                         dodge,
                                         base_size,
                                         expand_scale = TRUE) {
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
      size = compute_label_text_size(base_size),
      fontface = "bold",
      inherit.aes = FALSE
    )

  if (isTRUE(expand_scale)) {
    plot_obj <- plot_obj + scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Significance annotation preparation & drawing
# ===============================================================

#### Section: Lineplot Construction ####

plot_anova_lineplot_meanse <- function(data,
                                       info,
                                       layout_values,
                                       line_colors = NULL,
                                       base_size = 14,
                                       show_lines = FALSE,
                                       show_jitter = FALSE,
                                       use_dodge = FALSE,
                                       share_y_axis = FALSE,
                                       common_legend = FALSE,
                                       legend_position = NULL) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  allowed_positions <- c("bottom", "top", "left", "right")
  legend_position_value <- if (!is.null(legend_position) && legend_position %in% allowed_positions) {
    legend_position
  } else {
    "bottom"
  }

  shared_y_limits <- if (isTRUE(share_y_axis)) {
    compute_lineplot_shared_limits(context, data, factor1, factor2)
  } else {
    NULL
  }

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
        stratum_stats[[stratum]] <- list(
          stats = stats_df,
          raw = prepare_lineplot_raw_data(subset_data, resp, factor1, factor2)
        )
      }

      if (length(stratum_stats) == 0) {
        next
      }

      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }
      y_limits_to_use <- if (!is.null(shared_y_limits)) shared_y_limits else y_limits

      strata_panel_count <- max(strata_panel_count, length(stratum_stats))

      strata_plot_list <- lapply(names(stratum_stats), function(stratum_name) {
        entry <- stratum_stats[[stratum_name]]
        build_line_plot_panel(
          stats_df = entry$stats,
          title_text = stratum_name,
          y_limits = y_limits_to_use,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_size = base_size,
          raw_data = entry$raw,
          response_var = resp,
          show_lines = show_lines,
          show_jitter = show_jitter,
          use_dodge = use_dodge
        )
      })

      current_layout <- adjust_grid_layout(length(stratum_stats), context$strata_layout)

      combined <- patchwork::wrap_plots(
        plotlist = strata_plot_list,
        nrow = current_layout$nrow,
        ncol = current_layout$ncol
      )

      if (isTRUE(common_legend)) {
        combined <- collect_guides_safe(combined)
      }

      title_plot <- ggplot() +
        ta_plot_theme_void() +
        ggtitle(resp) +
        theme(
          plot.title = element_text(
            size = base_size,
            face = "bold",
            hjust = 0.5
          ),
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

      y_limits_to_use <- if (!is.null(shared_y_limits)) shared_y_limits else y_limits

      response_plots[[resp]] <- build_line_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        y_limits = y_limits_to_use,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_size = base_size,
        raw_data = prepare_lineplot_raw_data(data, resp, factor1, factor2),
        response_var = resp,
        show_lines = show_lines,
        show_jitter = show_jitter,
        use_dodge = use_dodge
      )
    }
  }

  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = isTRUE(common_legend),
    legend_position = if (isTRUE(common_legend)) legend_position_value else NULL
  )
}

build_line_plot_panel <- function(stats_df,
                                  title_text,
                                  y_limits,
                                  factor1,
                                  factor2,
                                  line_colors,
                                  base_size = 13,
                                  raw_data = NULL,
                                  response_var = NULL,
                                  show_lines = FALSE,
                                  show_jitter = FALSE,
                                  use_dodge = FALSE) {
  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    color_value <- if (!is.null(line_colors) && length(line_colors) > 0) {
      unname(line_colors)[1]
    } else {
      resolve_single_color()
    }
    p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean))

    if (isTRUE(show_jitter) && !is.null(raw_data) &&
        !is.null(response_var) && response_var %in% names(raw_data) &&
        factor1 %in% names(raw_data)) {
      jitter_df <- raw_data[!is.na(raw_data[[response_var]]), , drop = FALSE]
      if (nrow(jitter_df) > 0) {
        p <- p + geom_jitter(
          data = jitter_df,
          aes(x = !!sym(factor1), y = !!sym(response_var)),
          width = 0.12,
          alpha = 0.35,
          size = 1.7,
          color = color_value,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
      }
    }

    if (isTRUE(show_lines)) {
      p <- p + geom_line(aes(group = 1), color = color_value, linewidth = 1)
    }

    p <- p +
      geom_point(size = 3, color = color_value) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        color = color_value
      ) +
      ta_plot_theme(base_size = base_size) +
      labs(x = factor1, y = "Mean ± SE") +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
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
    dodge_width <- if (isTRUE(use_dodge)) 0.4 else NULL
    dodge <- if (!is.null(dodge_width)) position_dodge(width = dodge_width) else NULL
    jitter_dodge_width <- if (is.null(dodge_width)) 0 else dodge_width
    p <- ggplot(stats_df, aes(
      x = !!sym(factor1),
      y = mean,
      color = !!sym(factor2),
      group = !!sym(factor2)
    ))

    if (isTRUE(show_jitter) && !is.null(raw_data) && !is.null(response_var) &&
        all(c(factor1, factor2) %in% names(raw_data)) &&
        response_var %in% names(raw_data)) {
      jitter_df <- raw_data[!is.na(raw_data[[response_var]]), , drop = FALSE]
      if (nrow(jitter_df) > 0) {
        jitter_df[[factor2]] <- factor(as.character(jitter_df[[factor2]]), levels = group_levels)
        p <- p + geom_jitter(
          data = jitter_df,
          aes(x = !!sym(factor1), y = !!sym(response_var), color = !!sym(factor2)),
          position = position_jitterdodge(jitter.width = 0.15, dodge.width = jitter_dodge_width),
          size = 1.6,
          alpha = 0.4,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
      }
    }

    if (isTRUE(show_lines)) {
      if (is.null(dodge)) {
        p <- p + geom_line(linewidth = 1)
      } else {
        p <- p + geom_line(linewidth = 1, position = dodge)
      }
    }

    point_layer <- if (is.null(dodge)) {
      geom_point(size = 3)
    } else {
      geom_point(size = 3, position = dodge)
    }

    errorbar_layer <- if (is.null(dodge)) {
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15
      )
    } else {
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        position = dodge
      )
    }

    p <- p +
      point_layer +
      errorbar_layer +
      ta_plot_theme(base_size = base_size) +
      labs(
        x = factor1,
        y = "Mean ± SE",
        color = factor2
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
      ) +
      scale_color_manual(values = palette)
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  p + ggtitle(title_text) +
    theme(
      plot.title = element_text(
        size = base_size,
        face = "bold",
        hjust = 0.5
      )
    )
}

prepare_lineplot_raw_data <- function(df, response_var, factor1, factor2 = NULL) {
  if (is.null(df) || is.null(response_var) || is.null(factor1)) return(NULL)
  if (!response_var %in% names(df) || !factor1 %in% names(df)) return(NULL)

  cols <- c(factor1, factor2, response_var)
  cols <- cols[!vapply(cols, is.null, FUN.VALUE = logical(1), USE.NAMES = FALSE)]
  cols <- unique(cols)
  cols <- cols[cols %in% names(df)]
  if (!response_var %in% cols || !factor1 %in% cols) return(NULL)

  raw_subset <- df[, cols, drop = FALSE]
  raw_subset <- raw_subset[!is.na(raw_subset[[response_var]]), , drop = FALSE]
  if (nrow(raw_subset) == 0) return(NULL)
  raw_subset
}

#### Section: Significance Annotation System ####

prepare_significance_annotations_data <- function(stats_df, factor1, posthoc_entry) {
  if (is.null(posthoc_entry) || !is.data.frame(posthoc_entry)) return(NULL)

  signif_df <- posthoc_entry

  signif_df$p.value <- as.character(signif_df$p.value)
  signif_df$p.value <- gsub("[[:space:]]", "", signif_df$p.value)
  signif_df$p.value <- gsub("^<\\.?0*", "0.", signif_df$p.value)
  signif_df$p.value <- suppressWarnings(as.numeric(signif_df$p.value))

  signif_df <- signif_df |> dplyr::filter(!is.na(p.value) & p.value < 0.05)
  if (nrow(signif_df) == 0) return(NULL)

  signif_df <- signif_df |> dplyr::mutate(
    xmin = gsub(" - .*", "", contrast),
    xmax = gsub(".*- ", "", contrast),
    annotations = dplyr::case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE ~ ""
    )
  )

  max_y <- max(stats_df$mean + stats_df$se, na.rm = TRUE)
  step <- abs(max_y) * 0.15
  signif_df$y_position <- seq(from = max_y + step, by = step, length.out = nrow(signif_df))
  signif_df$.group_id <- seq_len(nrow(signif_df))

  list(
    data = signif_df,
    max_y = max(signif_df$y_position, na.rm = TRUE) * 1.1
  )
}

add_significance_annotations <- function(plot_obj,
                                         stats_df,
                                         factor1,
                                         posthoc_entry,
                                         allow_scale_expansion = TRUE) {
  prep <- prepare_significance_annotations_data(stats_df, factor1, posthoc_entry)
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj + ggsignif::geom_signif(
    data = prep$data,
    aes(
      xmin = xmin,
      xmax = xmax,
      annotations = annotations,
      y_position = y_position,
      group = .group_id
    ),
    manual = TRUE,
    inherit.aes = FALSE,
    textsize = 3.5,
    tip_length = 0.01,
    color = "gray30"
  )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.10)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}

prepare_nested_significance_annotations_data <- function(stats_df,
                                                         factor1,
                                                         factor2,
                                                         nested_posthoc,
                                                         dodge_width = 0.7) {
  nested_name <- paste0(factor2, "_within_", factor1)

  # Accept both a flat data.frame or a list entry
  df <- NULL
  if (is.data.frame(nested_posthoc)) {
    if (!"Factor" %in% names(nested_posthoc)) return(NULL)
    df <- dplyr::filter(nested_posthoc, .data$Factor == nested_name)
  } else if (is.list(nested_posthoc) && nested_name %in% names(nested_posthoc)) {
    df <- nested_posthoc[[nested_name]]
  } else {
    return(NULL)
  }
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!all(c("contrast","p.value", factor1) %in% names(df))) return(NULL)
  
  # Clean p-values, keep only significant
  df$p.value <- as.character(df$p.value)
  df$p.value <- gsub("[[:space:]]", "", df$p.value)
  df$p.value <- gsub("^<\\.?0*", "0.", df$p.value)  # <.0001, <0.001 -> numeric-ish
  df$p.value <- suppressWarnings(as.numeric(df$p.value))
  df <- dplyr::filter(df, !is.na(.data$p.value) & .data$p.value < 0.05)
  if (nrow(df) == 0) return(NULL)
  
  # Parse pair labels from "A - B"
  df$g1 <- sub(" - .*", "", df$contrast)
  df$g2 <- sub(".*- ",  "", df$contrast)
  
  # Factor levels on the plot
  lev1 <- levels(stats_df[[factor1]])
  lev2 <- levels(stats_df[[factor2]])
  if (is.null(lev1)) lev1 <- unique(as.character(stats_df[[factor1]]))
  if (is.null(lev2)) lev2 <- unique(as.character(stats_df[[factor2]]))
  
  # Compute dodge offsets for factor2 groups (centred around x)
  k <- length(lev2)
  # offsets range roughly within [-dodge_width/2, +dodge_width/2]
  offsets <- seq_len(k)
  offsets <- (offsets - (k + 1)/2) * (dodge_width / max(1, k))
  
  # Helper: numeric x for a (factor1 level, factor2 level) bar center
  idx2 <- function(g) match(g, lev2)
  x_center <- function(xlvl, glvl) {
    as.numeric(match(xlvl, lev1)) + offsets[idx2(glvl)]
  }
  
  # Build numeric xmin/xmax for each contrast row
  df$x_base  <- as.numeric(match(df[[factor1]], lev1))
  df$xmin    <- mapply(x_center, df[[factor1]], df$g1)
  df$xmax    <- mapply(x_center, df[[factor1]], df$g2)
  
  # Per-treatment y position just above its local bars
  local_max <- dplyr::summarise(
    dplyr::group_by(stats_df, .data[[factor1]]),
    ymax = max(mean + se, na.rm = TRUE),
    .groups = "drop"
  )
  y_lookup <- setNames(local_max$ymax, as.character(local_max[[factor1]]))
  df$y0 <- unname(y_lookup[as.character(df[[factor1]])])
  
  # Stack multiple brackets within the same treatment a bit
  step <- diff(range(stats_df$mean + stats_df$se, na.rm = TRUE))
  if (!is.finite(step) || step == 0) step <- max(stats_df$mean + stats_df$se, na.rm = TRUE) * 0.05
  step <- step * 0.12
  base_offset <- step * 0.6
  df <- dplyr::group_by(df, .data[[factor1]])
  df <- dplyr::mutate(
    df,
    row_id = dplyr::row_number(),
    y_position = y0 + base_offset + row_id * step
  )
  df <- dplyr::ungroup(df)
  
  # Stars
  df$annotations <- dplyr::case_when(
    df$p.value < 0.001 ~ "***",
    df$p.value < 0.01  ~ "**",
    df$p.value < 0.05  ~ "*",
    TRUE ~ ""
  )
  df$.group_id <- seq_len(nrow(df))
  
  # One layer total (manual=TRUE expects numeric x’s on the data)
  list(
    data = df,
    max_y = max(df$y_position, na.rm = TRUE) * 1.1
  )
}

add_nested_significance_annotations <- function(plot_obj,
                                                 stats_df,
                                                 factor1,
                                                 factor2,
                                                 nested_posthoc,
                                                 dodge_width = 0.7,
                                                 allow_scale_expansion = TRUE) {
  prep <- prepare_nested_significance_annotations_data(
    stats_df, factor1, factor2, nested_posthoc, dodge_width
  )
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj + ggsignif::geom_signif(
    data        = prep$data,
    aes(xmin = xmin,
        xmax = xmax,
        annotations = annotations,
        y_position = y_position,
        group = .group_id),
    manual      = TRUE,
    inherit.aes = FALSE,
    textsize    = 3.5,
    tip_length  = 0.01,
    color       = "gray30"
  )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.10)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}

#### Section: Utility Helpers ####

update_numeric_range <- function(current_range, values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(current_range)
  new_range <- range(values)
  if (any(!is.finite(new_range))) return(current_range)
  if (is.null(current_range)) {
    new_range
  } else {
    c(min(current_range[1], new_range[1]), max(current_range[2], new_range[2]))
  }
}

expand_axis_limits <- function(range_vals, lower_mult = 0.05, upper_mult = 0.12) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) return(range_vals)
  span <- diff(range_vals)
  if (!is.finite(span) || span == 0) {
    span <- max(1, abs(range_vals[2]))
  }
  c(range_vals[1] - span * lower_mult, range_vals[2] + span * upper_mult)
}

ensure_barplot_zero_baseline <- function(range_vals) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) {
    return(range_vals)
  }

  lower <- range_vals[1]
  if (is.na(lower)) return(range_vals)

  # Keep barplots anchored at zero to avoid negative baselines when no annotations
  range_vals[1] <- 0
  range_vals
}

sanitize_name <- function(name) {
  safe <- gsub("[^A-Za-z0-9]+", "_", name)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

anova_protect_vars <- function(vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)

  vals <- vapply(vars, function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return("")
    if (grepl("^`.*`$", v)) v else paste0("`", v, "`")
  }, character(1))

  vals[nzchar(vals)]
}
