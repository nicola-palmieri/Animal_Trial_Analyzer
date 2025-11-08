# ===============================================================
# ⏱️ Longitudinal analysis module (LMM + GEE)
# ===============================================================

# Helper: safely pull first non-empty element
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

longitudinal_score_time <- function(x, name = "") {
  score <- 0
  if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) score <- score + 3
  if (is.numeric(x)) score <- score + 1
  if (is.ordered(x)) score <- score + 2
  if (is.factor(x)) {
    levs <- levels(x)
    if (length(levs) >= 3 && all(!is.na(suppressWarnings(as.numeric(levs))))) score <- score + 1
  }
  uniq <- unique(stats::na.omit(x))
  if (length(uniq) >= 3) score <- score + 1
  if (nzchar(name) && grepl("(time|day|visit|week|month|year|cycle|hour)", name, ignore.case = TRUE)) score <- score + 2
  if (length(uniq) <= 1) score <- 0
  score
}

longitudinal_score_subject <- function(x, name = "") {
  vals <- unique(stats::na.omit(as.character(x)))
  n_vals <- length(vals)
  score <- 0
  if (n_vals >= 5) score <- score + 2
  if (n_vals >= 10) score <- score + 1
  if (nzchar(name) && grepl("(id|subject|patient|participant|unit)", name, ignore.case = TRUE)) score <- score + 3
  if (!is.factor(x) && !is.character(x)) score <- 0
  score
}

longitudinal_score_treatment <- function(x, name = "") {
  vals <- unique(stats::na.omit(as.character(x)))
  n_vals <- length(vals)
  score <- 0
  if (n_vals >= 2 && n_vals <= 6) score <- score + 2
  if (n_vals == 2) score <- score + 1
  if (nzchar(name) && grepl("(treat|arm|group|dose)", name, ignore.case = TRUE)) score <- score + 2
  if (!is.factor(x) && !is.character(x)) score <- 0
  score
}

#' Detect candidate variables for longitudinal modelling
#'
#' @param df data.frame
#' @return list with default selections and choices
longitudinal_detect_variables <- function(df) {
  if (!is.data.frame(df)) return(list())
  col_names <- names(df)
  numeric_cols <- col_names[vapply(df, is.numeric, logical(1))]
  categorical_cols <- col_names[vapply(df, function(x) is.factor(x) || is.character(x), logical(1))]

  time_scores <- vapply(col_names, function(col) longitudinal_score_time(df[[col]], col), numeric(1))
  subject_scores <- vapply(col_names, function(col) longitudinal_score_subject(df[[col]], col), numeric(1))
  treatment_scores <- vapply(col_names, function(col) longitudinal_score_treatment(df[[col]], col), numeric(1))

  time_candidates <- col_names[time_scores > 0]
  if (length(time_candidates) == 0) time_candidates <- numeric_cols
  default_time <- if (length(time_candidates) > 0) {
    time_candidates[which.max(time_scores[time_candidates] %||% 0)]
  } else {
    NULL
  }

  subject_candidates <- categorical_cols
  if (length(subject_candidates) == 0) {
    subject_candidates <- col_names
  }
  default_subject <- if (length(subject_candidates) > 0) {
    subject_candidates[which.max(subject_scores[subject_candidates] %||% 0)]
  } else {
    NULL
  }

  treatment_candidates <- categorical_cols
  default_treatment <- NULL
  if (length(treatment_candidates) > 0) {
    best_treatment <- treatment_candidates[which.max(treatment_scores[treatment_candidates] %||% 0)]
    if (!is.na(best_treatment) && (treatment_scores[best_treatment] > 0)) {
      default_treatment <- best_treatment
    } else {
      default_treatment <- treatment_candidates[1]
    }
  }

  default_response <- if (length(numeric_cols) > 0) numeric_cols[1] else col_names[1]

  list(
    response_choices = numeric_cols,
    time_choices = unique(c(time_candidates, numeric_cols)),
    subject_choices = subject_candidates,
    treatment_choices = treatment_candidates,
    default_response = default_response,
    default_time = default_time,
    default_subject = default_subject,
    default_treatment = default_treatment
  )
}

longitudinal_detection_summary <- function(det) {
  if (is.null(det) || length(det) == 0) return(NULL)
  items <- list()
  if (!is.null(det$default_response)) items[[length(items) + 1]] <- sprintf("Response: %s", det$default_response)
  if (!is.null(det$default_time)) items[[length(items) + 1]] <- sprintf("Time: %s", det$default_time)
  if (!is.null(det$default_treatment)) items[[length(items) + 1]] <- sprintf("Treatment: %s", det$default_treatment)
  if (!is.null(det$default_subject)) items[[length(items) + 1]] <- sprintf("Subject: %s", det$default_subject)
  if (length(items) == 0) return(NULL)
  paste(items, collapse = " \u2022 ")
}

longitudinal_build_rhs <- function(time_var, treatment_var, include_interaction = TRUE,
                                   method = c("lmm", "gee"), subject_var = NULL,
                                   random_slope = FALSE) {
  method <- match.arg(method)
  terms <- character(0)
  if (!is.null(time_var) && nzchar(time_var)) terms <- c(terms, time_var)
  if (!is.null(treatment_var) && nzchar(treatment_var)) terms <- c(terms, treatment_var)
  if (isTRUE(include_interaction) && all(nzchar(c(time_var, treatment_var)))) {
    terms <- c(terms, paste(time_var, treatment_var, sep = ":"))
  }
  if (method == "lmm" && !is.null(subject_var) && nzchar(subject_var)) {
    random_term <- if (isTRUE(random_slope) && nzchar(time_var)) {
      paste0("(1 + ", time_var, "|", subject_var, ")")
    } else {
      paste0("(1|", subject_var, ")")
    }
    terms <- c(terms, random_term)
  }
  unique(terms)
}

longitudinal_build_formula <- function(response, rhs_terms) {
  if (is.null(response) || !nzchar(response)) return(NULL)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  if (length(rhs_terms) == 0) {
    paste(response, "~ 1")
  } else {
    paste(response, "~", paste(rhs_terms, collapse = " + "))
  }
}

longitudinal_prepare_data <- function(df, vars) {
  if (!is.data.frame(df)) return(df)
  vars <- unique(vars[nzchar(vars)])
  vars <- intersect(vars, names(df))
  df <- df[, vars, drop = FALSE]
  stats::na.omit(df)
}

longitudinal_resolve_stratification <- function(df, strat_var, strata_levels = NULL) {
  if (is.null(strat_var) || !nzchar(strat_var)) {
    return(list(var = NULL, levels = NULL, warning = NULL))
  }

  if (!strat_var %in% names(df)) {
    msg <- sprintf(
      "Stratification variable '%s' was not found after filtering missing values; showing overall results only.",
      strat_var
    )
    return(list(var = NULL, levels = NULL, warning = msg))
  }

  list(var = strat_var, levels = strata_levels, warning = NULL)
}

longitudinal_fit_model <- function(df, response, rhs_terms, method = c("lmm", "gee"),
                                   subject_var = NULL, family = "gaussian",
                                   corstr = "exchangeable") {
  method <- match.arg(method)
  rhs_terms <- rhs_terms[nzchar(rhs_terms)]
  form_txt <- longitudinal_build_formula(response, rhs_terms)
  validate <- function(cond, msg) {
    if (!isTRUE(cond)) stop(msg, call. = FALSE)
  }
  validate(!is.null(form_txt), "Model formula could not be constructed.")
  form <- stats::as.formula(form_txt)
  if (method == "lmm") {
    reg_fit_model(response, rhs_terms, df, engine = "lmm")
  } else {
    validate(!is.null(subject_var) && nzchar(subject_var), "Subject identifier is required for GEE models.")
    family_obj <- switch(family,
      gaussian = stats::gaussian(),
      binomial = stats::binomial(),
      poisson = stats::poisson(),
      stats::gaussian()
    )
    geepack::geeglm(
      formula = form,
      data = df,
      id = df[[subject_var]],
      corstr = corstr,
      family = family_obj
    )
  }
}

longitudinal_tidy_gee <- function(model) {
  if (is.null(model)) {
    return(list(summary = NULL, effects = NULL))
  }
  sm <- summary(model)
  coef_df <- as.data.frame(sm$coefficients)
  coef_df$term <- rownames(coef_df)
  rownames(coef_df) <- NULL
  names(coef_df) <- c("estimate", "std_error", "statistic", "p_value", "term")
  coef_df <- coef_df[, c("term", "estimate", "std_error", "statistic", "p_value")]

  metrics <- data.frame(
    metric = c("QIC", "QICu", "nobs"),
    value = c(
      suppressWarnings(geepack::QIC(model)$QIC),
      suppressWarnings(geepack::QIC(model)$QICu),
      stats::nobs(model)
    ),
    stringsAsFactors = FALSE
  )

  anova_tbl <- tryCatch({
    as.data.frame(anova(model))
  }, error = function(e) NULL)
  if (!is.null(anova_tbl)) {
    anova_tbl$Effect <- rownames(anova_tbl)
    rownames(anova_tbl) <- NULL
    anova_tbl <- anova_tbl[, c("Effect", setdiff(names(anova_tbl), "Effect"))]
  }

  list(
    summary = coef_df,
    effects = list(metrics = metrics, anova = anova_tbl)
  )
}

longitudinal_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("detection_ui")),
      uiOutput(ns("response_ui")),
      uiOutput(ns("time_ui")),
      uiOutput(ns("treatment_ui")),
      uiOutput(ns("subject_ui")),
      selectInput(
        ns("method"),
        "Estimation method:",
        choices = c("Mixed-effects (LMM)" = "lmm", "Generalized Estimating Equations (GEE)" = "gee"),
        selected = "lmm"
      ),
      checkboxInput(ns("include_interaction"), "Include time × treatment interaction", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lmm'", ns("method")),
        checkboxInput(ns("random_slope"), "Allow random slope for time", value = FALSE)
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'gee'", ns("method")),
        selectInput(
          ns("gee_family"),
          "Response distribution:",
          choices = c("Gaussian" = "gaussian", "Binomial" = "binomial", "Poisson" = "poisson"),
          selected = "gaussian"
        ),
        selectInput(
          ns("gee_corstr"),
          "Working correlation:",
          choices = c("Exchangeable" = "exchangeable", "AR(1)" = "ar1", "Independent" = "independence"),
          selected = "exchangeable"
        )
      ),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      hr(),
      uiOutput(ns("formula_preview")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run longitudinal model", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download fitted models", style = "width: 100%;"))
      )
    ),
    results = tagList(
      uiOutput(ns("results_ui"))
    )
  )
}

longitudinal_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    strat_info <- stratification_server("strat", data)

    df <- reactive({
      d <- data()
      req(is.data.frame(d))
      validate(need(nrow(d) > 0, "Upload data to run the longitudinal analysis."))
      d
    })

    detection <- reactive({
      req(df())
      longitudinal_detect_variables(df())
    })

    output$detection_ui <- renderUI({
      det <- detection()
      summary <- longitudinal_detection_summary(det)
      if (is.null(summary)) return(NULL)
      div(
        class = "alert alert-info", role = "alert",
        strong("Detected defaults: "), summary
      )
    })

    output$response_ui <- renderUI({
      det <- detection()
      choices <- det$response_choices %||% names(df())
      selectInput(ns("response"), "Response variable (numeric):", choices = choices, selected = det$default_response)
    })

    output$time_ui <- renderUI({
      det <- detection()
      choices <- det$time_choices %||% names(df())
      selectInput(ns("time_var"), "Time variable:", choices = choices, selected = det$default_time)
    })

    output$treatment_ui <- renderUI({
      det <- detection()
      choices <- det$treatment_choices %||% names(df())
      selectInput(ns("treatment"), "Treatment / group variable:", choices = choices, selected = det$default_treatment)
    })

    output$subject_ui <- renderUI({
      det <- detection()
      choices <- det$subject_choices %||% names(df())
      selectInput(ns("subject"), "Subject identifier:", choices = choices, selected = det$default_subject)
    })

    rhs_terms <- reactive({
      longitudinal_build_rhs(
        input$time_var,
        input$treatment,
        include_interaction = isTRUE(input$include_interaction),
        method = input$method %||% "lmm",
        subject_var = input$subject,
        random_slope = isTRUE(input$random_slope)
      )
    })

    output$formula_preview <- renderUI({
      req(input$response)
      rhs <- rhs_terms()
      form_txt <- longitudinal_build_formula(input$response, rhs)
      if (is.null(form_txt)) return(NULL)
      wellPanel(strong("Model formula: "), code(form_txt))
    })

    model_info <- eventReactive(input$run, {
      d <- df()
      response <- req(input$response)
      time_var <- req(input$time_var)
      subject_var <- req(input$subject)
      treatment_var <- input$treatment
      method <- input$method %||% "lmm"
      gee_family <- input$gee_family %||% "gaussian"
      gee_corstr <- input$gee_corstr %||% "exchangeable"
      include_interaction <- isTRUE(input$include_interaction)
      random_slope <- isTRUE(input$random_slope)

      strat_selection <- strat_info()
      strat_var <- strat_selection$var
      required_vars <- c(response, time_var, subject_var, treatment_var, strat_var)
      clean_df <- longitudinal_prepare_data(d, required_vars)
      validate(need(nrow(clean_df) > 0, "No observations remain after removing missing values."))

      rhs <- longitudinal_build_rhs(
        time_var,
        treatment_var,
        include_interaction = include_interaction,
        method = method,
        subject_var = subject_var,
        random_slope = random_slope
      )

      fits <- list()
      flat_models <- list()
      errors <- list()
      success <- FALSE
      strata_entries <- list()
      strata_models <- list()

      strat_resolution <- longitudinal_resolve_stratification(clean_df, strat_var, strat_selection$levels)
      if (!is.null(strat_resolution$warning)) {
        errors[["Stratification"]] <- strat_resolution$warning
      }
      strat_var <- strat_resolution$var
      strata_levels <- strat_resolution$levels

      strat <- list(
        var = strat_var,
        levels = strata_levels
      )

      build_strata <- function() {
        if (is.null(strat_var)) {
          list(list(label = NULL, display = "Overall", data = clean_df))
        } else {
          levels_to_use <- strata_levels
          if (is.null(levels_to_use) || length(levels_to_use) == 0) {
            levels_to_use <- unique(stats::na.omit(as.character(clean_df[[strat_var]])))
          }
          lapply(levels_to_use, function(level) {
            subset_df <- clean_df[clean_df[[strat_var]] == level, , drop = FALSE]
            list(label = level, display = level, data = subset_df)
          })
        }
      }

      strata <- build_strata()

      for (stratum in strata) {
        subset_df <- stratum$data
        display <- stratum$display %||% "Overall"
        if (nrow(subset_df) == 0) {
          msg <- sprintf("No observations available for %s.", display)
          strata_entries[[length(strata_entries) + 1]] <- list(
            label = stratum$label,
            display = display,
            model = NULL,
            error = msg
          )
          errors[[display]] <- msg
          next
        }

        fit <- tryCatch({
          longitudinal_fit_model(
            subset_df,
            response,
            rhs,
            method = method,
            subject_var = subject_var,
            family = gee_family,
            corstr = gee_corstr
          )
        }, error = function(e) e)

        if (inherits(fit, "error")) {
          msg <- conditionMessage(fit)
          strata_entries[[length(strata_entries) + 1]] <- list(
            label = stratum$label,
            display = display,
            model = NULL,
            error = msg
          )
          errors[[display]] <- msg
        } else {
          success <- TRUE
          tidy <- if (method == "lmm") {
            tidy_regression_model(fit, "lmm")
          } else {
            longitudinal_tidy_gee(fit)
          }
          strata_entries[[length(strata_entries) + 1]] <- list(
            label = stratum$label,
            display = display,
            model = fit,
            error = NULL,
            tidy = tidy
          )
          strata_models[[display]] <- fit
          flat_models[[length(flat_models) + 1]] <- list(
            response = response,
            stratum = display,
            model = fit,
            tidy = tidy
          )
        }
      }

      fits[[response]] <- list(
        stratified = !is.null(strat_var),
        strata = strata_entries
      )

      summary_list <- list()
      effects_list <- list()
      if (length(strata_entries) > 0) {
        if (!is.null(strat_var)) {
          summary_list[[response]] <- list()
          effects_list[[response]] <- list()
          for (entry in strata_entries) {
            display <- entry$display %||% "Stratum"
            if (!is.null(entry$tidy$summary)) summary_list[[response]][[display]] <- entry$tidy$summary
            if (!is.null(entry$tidy$effects)) effects_list[[response]][[display]] <- entry$tidy$effects
          }
        } else {
          tidy <- strata_entries[[1]]$tidy
          if (!is.null(tidy$summary)) summary_list[[response]] <- tidy$summary
          if (!is.null(tidy$effects)) effects_list[[response]] <- tidy$effects
        }
      }

      metadata <- list(
        response = response,
        time_var = time_var,
        treatment_var = treatment_var,
        subject_var = subject_var,
        include_interaction = include_interaction,
        random_slope = random_slope,
        method = method,
        family = if (method == "gee") gee_family else NULL,
        corstr = if (method == "gee") gee_corstr else NULL,
        rhs = rhs,
        formula = longitudinal_build_formula(response, rhs),
        stratification = strat,
        models = strata_models,
        detection = detection()
      )

      list(
        fits = fits,
        flat_models = flat_models,
        summary = summary_list,
        effects = effects_list,
        success = success,
        response = response,
        errors = errors,
        stratification = strat,
        data_used = clean_df,
        metadata = metadata
      )
    })

    output$results_ui <- renderUI({
      mod <- model_info()
      req(mod)
      warnings_ui <- NULL
      if (length(mod$errors) > 0) {
        items <- lapply(names(mod$errors), function(name) {
          tags$li(tags$strong(name), ": ", mod$errors[[name]])
        })
        warnings_ui <- div(class = "alert alert-warning", tags$ul(items))
      }
      if (!isTRUE(mod$success)) {
        return(warnings_ui %||% div(class = "alert alert-warning", "Model fitting failed."))
      }

      response <- mod$response
      fits <- mod$fits[[response]]
      strata <- fits$strata

      panels <- lapply(seq_along(strata), function(idx) {
        entry <- strata[[idx]]
        display <- entry$display %||% "Overall"
        panel_id <- paste0("summary_", idx)
        table_id <- paste0("table_", idx)
        diag_resid_id <- paste0("resid_", idx)
        diag_qq_id <- paste0("qq_", idx)

        if (!is.null(entry$model)) {
          local({
            local_entry <- entry
            local_panel_id <- panel_id
            local_table_id <- table_id
            local_diag_resid <- diag_resid_id
            local_diag_qq <- diag_qq_id
            output[[local_panel_id]] <- renderPrint({
              if ((input$method %||% "lmm") == "lmm") {
                reg_display_lmm_summary(local_entry$model)
              } else {
                print(summary(local_entry$model))
              }
            })
            output[[local_table_id]] <- renderTable({
              local_entry$tidy$summary
            }, striped = TRUE, bordered = TRUE, spacing = "s")
            if ((input$method %||% "lmm") == "lmm") {
              output[[local_diag_resid]] <- renderPlot({
                plot(stats::fitted(local_entry$model), stats::resid(local_entry$model),
                     xlab = "Fitted values", ylab = "Residuals")
                abline(h = 0, lty = 2)
              })
              output[[local_diag_qq]] <- renderPlot({
                stats::qqnorm(stats::resid(local_entry$model))
                stats::qqline(stats::resid(local_entry$model))
              })
            }
          })
        }

        diag_block <- NULL
        if ((input$method %||% "lmm") == "lmm" && !is.null(entry$model)) {
          diag_block <- tagList(
            h5("Diagnostics"),
            fluidRow(
              column(6, plotOutput(ns(diag_resid_id))),
              column(6, plotOutput(ns(diag_qq_id)))
            ),
            br()
          )
        }

        if (is.null(entry$model)) {
          div(
            class = "alert alert-warning",
            strong(display), ": ", entry$error
          )
        } else {
          tagList(
            h4(display),
            verbatimTextOutput(ns(panel_id)),
            br(),
            tableOutput(ns(table_id)),
            br(),
            diag_block
          )
        }
      })

      content <- do.call(tagList, panels)
      if (!is.null(warnings_ui)) {
        content <- tagList(warnings_ui, content)
      }
      content
    })

    output$download_model <- downloadHandler(
      filename = function() {
        paste0("longitudinal_models_", Sys.Date(), ".rds")
      },
      content = function(file) {
        mod <- model_info()
        req(mod)
        if (!isTRUE(mod$success)) stop("No fitted models available.")
        export <- lapply(mod$flat_models, function(entry) {
          list(
            response = entry$response,
            stratum = entry$stratum,
            model = entry$model,
            tidy = entry$tidy,
            metadata = mod$metadata
          )
        })
        saveRDS(export, file)
      }
    )

    reactive({
      mod <- model_info()
      req(mod)
      list(
        analysis_type = "LONGITUDINAL",
        type = "longitudinal",
        data_used = mod$data_used,
        model = mod$metadata$models,
        summary = mod$summary,
        posthoc = NULL,
        effects = mod$effects,
        stats = list(n = nrow(mod$data_used)),
        metadata = mod$metadata,
        fits = mod$fits,
        flat_models = mod$flat_models,
        stratification = mod$stratification,
        responses = mod$response,
        errors = mod$errors
      )
    })
  })
}
