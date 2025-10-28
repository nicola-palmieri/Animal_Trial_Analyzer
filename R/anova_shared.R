# ===============================================================
# 🧠 Animal Trial Analyzer — Shared ANOVA Module Helpers
# ===============================================================

build_anova_layout_controls <- function(ns, input, info, default_ui_value) {
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
            value = isolate(default_ui_value(input$strata_rows)),
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
            value = isolate(default_ui_value(input$strata_cols)),
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
            value = isolate(default_ui_value(input$resp_rows)),
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
            value = isolate(default_ui_value(input$resp_cols)),
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
    h4("Layout controls"),
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
    stratify_var = NULL,
    strata_order = NULL
) {
  req(df, responses, model)
  
  if (!is.null(factor1_var) && !is.null(factor1_order)) {
    df[[factor1_var]] <- factor(as.character(df[[factor1_var]]), levels = factor1_order)
  }
  
  if (!is.null(factor2_var) && !is.null(factor2_order)) {
    df[[factor2_var]] <- factor(as.character(df[[factor2_var]]), levels = factor2_order)
  }
  
  if (!is.null(stratify_var) && stratify_var %in% names(df)) {
    if (!guard_stratification_levels(df, stratify_var)) {
      return(NULL)
    }

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
    anova_object = anova_obj,
    anova_table = anova_df,
    anova_significant = anova_significant,
    posthoc_details = posthoc_details,
    posthoc_table = posthoc_combined,
    posthoc_significant = posthoc_significant
  )
}

# ---------------------------------------------------------------
# Output composition
# ---------------------------------------------------------------
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

# ---------------------------------------------------------------
# Results export
# ---------------------------------------------------------------

download_all_anova_results <- function(models_info, file) {
  if (is.null(models_info) || is.null(models_info$models)) {
    stop("No models found to export.")
  }
  
  combined_results <- list()
  
  # --- Case 1: no stratification
  if (is.null(models_info$strata)) {
    for (resp in models_info$responses) {
      model_obj <- models_info$models[[resp]]
      anova_obj <- car::Anova(model_obj, type = 3)
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
        model_obj <- models_info$models[[stratum]][[resp]]
        anova_obj <- car::Anova(model_obj, type = 3)
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
  doc <- body_add_par(doc, sprintf("Generated by Animal Trial Analyzer on %s", Sys.Date()))
  doc <- body_add_par(doc, "Significant p-values (< 0.05) in bold.", style = "Normal")
  print(doc, target = file)
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


