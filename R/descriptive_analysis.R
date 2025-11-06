# ===============================================================
# 🧾 Table Analyzer — Descriptive Statistics Modules
# ===============================================================

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      tags$details(
        tags$summary(strong("Advanced options")),
        br(),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Show summary", width = "100%")),
        column(6, downloadButton(ns("download_summary"), "Download summary", style = "width: 100%;"))
      ),
      hr()
    ),
    results = tagList(
      verbatimTextOutput(ns("summary_text"))
    )
  )
}

descriptive_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- filtered_data

    # ------------------------------------------------------------
    # Dynamic inputs
    # ------------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      
      tagList(
        selectInput(ns("cat_vars"), label = "Categorical variables:", choices = cat_cols, selected = cat_cols, multiple = TRUE),
        br(),
        selectInput(ns("num_vars"), label = "Numeric variables:", choices = num_cols, selected = num_cols, multiple = TRUE)
      )
    })
    
    strat_info <- stratification_server("strat", df)
    
    # ------------------------------------------------------------
    # Summary computation
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      req(df())
      
      local_data <- df()
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      
      validate(
        need(length(selected_vars) > 0, "Please select at least one variable.")
      )
      
      strat_details <- strat_info()
      group_var <- strat_details$var
      data_columns <- selected_vars
      
      # --- Handle stratification if present ---
      if (!is.null(group_var)) {
        sel <- strat_details$levels
        if (!is.null(sel) && length(sel) > 0) {
          local_data <- dplyr::filter(local_data, .data[[group_var]] %in% sel)
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]), levels = sel)
        } else {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]))
        }
        local_data <- droplevels(local_data)
        data_columns <- unique(c(data_columns, group_var))
      }
      
      # --- Keep only selected valid columns ---
      data_columns <- data_columns[!is.na(data_columns) & nzchar(data_columns)]
      data_columns <- intersect(data_columns, names(local_data))
      local_data <- local_data[, data_columns, drop = FALSE]
      
      # --- Identify strata levels (if any) ---
      strata_levels <- if (!is.null(group_var) && group_var %in% names(local_data)) {
        levels(local_data[[group_var]])
      } else {
        NULL
      }
      
      # --- Compute summary ---
      list(
        summary = compute_descriptive_summary(local_data, group_var),
        selected_vars = selected_vars,
        group_var = group_var,
        processed_data = local_data,
        strata_levels = strata_levels
      )
    })
    
    
    
    
    # ------------------------------------------------------------
    # Print summary
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      print_summary_sections(summary_data()$summary)
    })
    
    # ------------------------------------------------------------
    # Download
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        results <- summary_data()
        req(results)
        sink(file)
        on.exit(sink(), add = TRUE)
        print_summary_sections(results$summary)
      }
    )
    
    # ------------------------------------------------------------
    # Return full model info
    # ------------------------------------------------------------
    df_final <- reactive({
      details <- summary_data()
      req(details)
      details$processed_data
    })

    model_fit <- reactive(NULL)

    summary_table <- reactive({
      details <- summary_data()
      req(details)
      details$summary
    })

    posthoc_results <- reactive(NULL)

    effect_table <- reactive(NULL)

    selected_vars_reactive <- reactive({
      details <- summary_data()
      req(details)
      details$selected_vars
    })

    group_var_reactive <- reactive({
      details <- summary_data()
      req(details)
      details$group_var
    })

    strata_levels_reactive <- reactive({
      details <- summary_data()
      req(details)
      details$strata_levels
    })

    reactive({
      details <- summary_data()
      req(details)

      data_used <- df_final()

      list(
        analysis_type = "DESCRIPTIVE",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = posthoc_results(),
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        metadata = list(
          selected_vars = details$selected_vars,
          group_var = details$group_var,
          strata_levels = details$strata_levels
        ),
        type = "descriptive",
        data = df,
        processed_data = df_final,
        selected_vars = selected_vars_reactive,
        group_var = group_var_reactive,
        strata_levels = strata_levels_reactive
      )
    })

  })
}

compute_descriptive_summary <- function(data, group_var = NULL) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  
  group_data <- if (!is.null(group_var)) group_by(data, .data[[group_var]], .drop = TRUE) else data

  skim_out <- if (!is.null(group_var)) {
    group_data %>% skim()
  } else {
    skim(data)
  }

  # Custom formatter for categorical top counts (show top 10, rest as "Others")
  format_top_counts <- function(values, top_n = 10) {
    if (is.null(values)) {
      return(NA_character_)
    }

    values_chr <- as.character(values)
    values_chr[is.na(values_chr)] <- "<NA>"

    if (!length(values_chr)) {
      return(NA_character_)
    }

    counts <- sort(table(values_chr), decreasing = TRUE)
    if (!length(counts)) {
      return(NA_character_)
    }

    top_counts <- head(counts, top_n)
    remainder <- sum(counts) - sum(top_counts)

    parts <- paste0(names(top_counts), ": ", as.integer(top_counts))
    if (remainder > 0) {
      parts <- c(parts, paste0("Others:", remainder))
    }

    paste(parts, collapse = ", ")
  }

  factor_vars <- names(data)[vapply(data, function(x) is.factor(x) || is.character(x) || is.logical(x), logical(1))]

  if (length(factor_vars) > 0 && "top_counts" %in% names(skim_out)) {
    custom_counts <- NULL

    if (!is.null(group_var) && group_var %in% names(data)) {
      group_keys <- dplyr::group_keys(group_data)
      group_splits <- dplyr::group_split(group_data)

      custom_counts_list <- vector("list", length(group_splits))
      for (i in seq_along(group_splits)) {
        group_df <- group_splits[[i]]
        per_var <- lapply(factor_vars, function(var) {
          tibble::tibble(
            skim_variable = var,
            top_counts_custom = format_top_counts(group_df[[var]])
          )
        })
        per_var <- dplyr::bind_rows(per_var)

        if (nrow(per_var) == 0) {
          next
        }

        key_vals <- group_keys[i, , drop = FALSE]
        if (ncol(key_vals) > 0) {
          key_vals <- key_vals[rep(1, nrow(per_var)), , drop = FALSE]
          per_var <- dplyr::bind_cols(key_vals, per_var)
        }

        custom_counts_list[[i]] <- per_var
      }

      if (length(custom_counts_list) > 0) {
        custom_counts <- dplyr::bind_rows(custom_counts_list)
      }

      join_cols <- c(names(group_keys), "skim_variable")
    } else {
      custom_counts <- dplyr::bind_rows(lapply(factor_vars, function(var) {
        tibble::tibble(
          skim_variable = var,
          top_counts_custom = format_top_counts(data[[var]])
        )
      }))
      join_cols <- "skim_variable"
    }

    if (!is.null(custom_counts) && nrow(custom_counts) > 0) {
      skim_out <- dplyr::left_join(skim_out, custom_counts, by = join_cols)

      if ("top_counts_custom" %in% names(skim_out)) {
        skim_out$top_counts <- as.character(skim_out$top_counts)
        idx <- which(skim_out$skim_type == "factor" & !is.na(skim_out$top_counts_custom))
        if (length(idx) > 0) {
          skim_out$top_counts[idx] <- skim_out$top_counts_custom[idx]
        }
        skim_out$top_counts_custom <- NULL
      }
    }
  }

  cv_out <- group_data %>%
    summarise(across(
      where(is.numeric),
      ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
      .names = "cv_{.col}"
    ), .groups = "drop")
  
  outlier_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ {
        q <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
      },
      .names = "outliers_{.col}"
    ), .groups = "drop")
  
  missing_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ 100 * mean(is.na(.x)),
      .names = "missing_{.col}"
    ), .groups = "drop")
  
  shapiro_out <- group_data %>%
    summarise(across(
      all_of(numeric_vars),
      ~ tryCatch(shapiro.test(.x)$p.value, error = function(e) NA_real_),
      .names = "shapiro_{.col}"
    ), .groups = "drop")
  
  list(
    skim = skim_out,
    cv = cv_out,
    outliers = outlier_out,
    missing = missing_out,
    shapiro = shapiro_out
  )
}

# ---- Shared printing ----
print_summary_sections <- function(results) {
  # 1) Print skim AS-IS (unchanged)
  cat(paste(capture.output(print(results$skim)), collapse = "\n"), "\n\n", sep = "")
  
  # 2) Helper to detect if a grouping column exists and what it's called
  metric_prefix <- "^(cv_|outliers_|missing_|shapiro_)"
  first_col <- if (!is.null(results$cv) && ncol(results$cv) > 0) names(results$cv)[1] else NULL
  group_col <- if (!is.null(first_col) && !grepl(metric_prefix, first_col)) first_col else NULL
  
  # 3) Robust long conversion that preserves the real group column name (if any)
  to_long <- function(df, value_name, group_col) {
    if (is.null(df) || ncol(df) == 0) {
      if (is.null(group_col)) {
        return(tibble::tibble(variable = character(), !!value_name := numeric()))
      } else {
        return(tibble::tibble(!!group_col := character(), variable = character(), !!value_name := numeric()))
      }
    }
    if (is.null(group_col)) {
      out <- tidyr::pivot_longer(df, tidyselect::everything(),
                                 names_to = "variable", values_to = value_name)
    } else {
      out <- tidyr::pivot_longer(df, -dplyr::all_of(group_col),
                                 names_to = "variable", values_to = value_name)
    }
    out$variable <- sub("^(cv_|outliers_|missing_|shapiro_)", "", out$variable)
    out
  }
  
  # 4) Build pieces (no "missing" here)
  cv_long   <- to_long(results$cv,       "cv",        group_col)
  out_long  <- to_long(results$outliers, "outliers",  group_col)
  shap_long <- to_long(results$shapiro,  "shapiro_p", group_col)
  
  # 5) Join by the right keys
  if (is.null(group_col)) {
    merged <- dplyr::full_join(cv_long,  out_long,  by = "variable") |>
      dplyr::full_join(shap_long, by = "variable")
  } else {
    merged <- dplyr::full_join(cv_long,  out_long,  by = c(group_col, "variable")) |>
      dplyr::full_join(shap_long, by = c(group_col, "variable"))
  }
  
  # 6) Round / order by numeric skim order
  merged <- merged |>
    dplyr::mutate(
      cv = round(cv, 2),
      shapiro_p = signif(shapiro_p, 3)
    )
  
  numeric_order <- NULL
  if (is.data.frame(results$skim) &&
      all(c("skim_type", "skim_variable") %in% names(results$skim))) {
    numeric_order <- results$skim |>
      dplyr::filter(.data$skim_type == "numeric") |>
      dplyr::pull(.data$skim_variable) |>
      unique()
  }
  if (!is.null(numeric_order) && length(numeric_order) > 0) {
    merged$variable <- factor(merged$variable, levels = numeric_order)
    if (is.null(group_col)) {
      merged <- dplyr::arrange(merged, .data$variable)
    } else {
      merged <- dplyr::arrange(merged, .data[[group_col]], .data$variable)
    }
    merged$variable <- as.character(merged$variable)
  } else {
    if (is.null(group_col)) {
      merged <- dplyr::arrange(merged, .data$variable)
    } else {
      merged <- dplyr::arrange(merged, .data[[group_col]], .data$variable)
    }
  }
  
  # 7) Print with/without group column
  cat("── Numeric variables summary ──\n")
  if (is.null(group_col)) {
    final_df <- merged[, c("variable","cv","outliers","shapiro_p"), drop = FALSE]
  } else {
    final_df <- merged[, c("variable", group_col, "cv","outliers","shapiro_p"), drop = FALSE]
  }
  print(as.data.frame(final_df), row.names = FALSE)
  
  cat("\nInterpretation:\n")
  cat("  • outliers = # beyond 1.5×IQR\n")
  cat("  • shapiro_p < 0.05 → non-normal distribution\n")
  
  invisible(NULL)
}
