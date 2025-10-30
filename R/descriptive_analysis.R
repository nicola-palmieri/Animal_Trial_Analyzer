# ===============================================================
# 🧾 Animal Trial Analyzer — Descriptive Statistics Module (Aligned Layout)
# ===============================================================

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("advanced_options")),
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
    
    output$advanced_options <- renderUI({
      tagList(
        render_stratification_controls(ns, df, input),
        uiOutput(ns("strata_order_ui"))
      )
    })
    
    output$strata_order_ui <- renderUI({
      render_strata_order_input(ns, df, input$stratify_var)
    })
    
    # ------------------------------------------------------------
    # Summary computation
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      req(df())
      raw_data <- df()
      local_data <- raw_data  # create a copy to avoid modifying shared reactive
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      validate(need(length(selected_vars) > 0, "Please select at least one variable."))

      group_var <- if (is.null(input$stratify_var) || input$stratify_var == "None") NULL else input$stratify_var
      if (!guard_stratification_levels(raw_data, group_var)) {
        return(NULL)
      }

      data_columns <- selected_vars

      if (!is.null(group_var)) {
        # keep ONLY selected levels, in the exact order; drop NA and unused levels
        sel <- input$strata_order
        if (!is.null(sel) && length(sel) > 0) {
          local_data <- dplyr::filter(local_data, .data[[group_var]] %in% sel)
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]), levels = sel)
        } else {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]))
        }
        local_data <- droplevels(local_data)

        data_columns <- unique(c(data_columns, group_var))
      }

      data_columns <- data_columns[!is.na(data_columns) & nzchar(data_columns)]
      data_columns <- intersect(data_columns, names(local_data))
      local_data <- local_data[, data_columns, drop = FALSE]

      if (!is.null(group_var) && !is.null(input$strata_order)) {
        if (group_var %in% names(local_data)) {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]),
                                            levels = input$strata_order)
        }
      }

      strata_levels <- if (!is.null(group_var) && group_var %in% names(local_data)) {
        levels(local_data[[group_var]])
      } else {
        NULL
      }

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
    return(reactive({
      details <- summary_data()
      if (is.null(details)) {
        return(NULL)
      }

      list(
        type = "descriptive",
        data = df,
        summary = reactive({ details <- summary_data(); req(details); details$summary }),
        selected_vars = reactive({ details <- summary_data(); req(details); details$selected_vars }),
        group_var = reactive({ details <- summary_data(); req(details); details$group_var }),
        processed_data = reactive({ details <- summary_data(); req(details); details$processed_data }),
        strata_levels = reactive({ details <- summary_data(); req(details); details$strata_levels })
      )
    }))
    
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
  # --- Reformat skim() headers ---
  lines <- capture.output(print(results$skim))
  
  # Replace the "Variable type: character/numeric" banner lines
  lines <- stringr::str_replace(
    lines,
    "^\\s*──\\s*Variable type:\\s*factor\\s*─+\\s*$",
    "── Categorical variables ──"
  )
  lines <- stringr::str_replace(
    lines,
    "^\\s*──\\s*Variable type:\\s*numeric\\s*─+\\s*$",
    "── Numeric variables ──"
  )
  
  cat(paste(lines, collapse = "\n"), "\n")
  
  cat("\n── Coefficient of Variation (CV%) ──\n")
  print(results$cv)
  cat("\n── Outlier Counts (IQR rule) ──\n")
  print(results$outliers)
  cat("\n── Missingness Summary (% Missing) ──\n")
  print(results$missing)
  cat("\n── Shapiro–Wilk Normality Test (p-values) ──\n")
  print(results$shapiro)
  cat("\nInterpretation:\n")
  cat("  • CV% > 20 may indicate high variability.\n")
  cat("  • Outliers = # of animals beyond 1.5×IQR.\n")
  cat("  • Missing% > 5 may indicate measurement gaps.\n")
  cat("  • Shapiro p < 0.05 → non-normal distribution.\n")
}

