# ===============================================================
# 🧬 Common module for LM and LMM
# ===============================================================

regression_ui <- function(id, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  ns <- NS(id)
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  list(
    config = tagList(
      if (allow_multi_response) multi_response_ui(ns("response")) else uiOutput(ns("response_ui")),
      uiOutput(ns("fixed_selector")),
      uiOutput(ns("level_order")),
      uiOutput(ns("covar_selector")),
      if (engine == "lmm") uiOutput(ns("random_selector")),
      uiOutput(ns("interaction_select")),
      uiOutput(ns("formula_preview")),
      br(),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show results", width = "100%"),
          "Help: Fit the model using the chosen predictors and options."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_model"), "Download all results", style = "width: 100%;"),
          "Help: Export the model outputs, tables, and summaries to your computer."
        ))
      )
    ),
    results = tagList(
      uiOutput(ns("results_ui"))
    )
  )
}

regression_server <- function(id, data, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    strat_info <- stratification_server("strat", data)

    if (allow_multi_response) {
      selected_responses <- multi_response_server("response", data)
    } else {
      output$response_ui <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          selectInput(ns("dep"), "Response variable (numeric)", choices = types$num),
          "Help: Choose the outcome that the model should predict."
        )
      })

      selected_responses <- reactive({
        req(input$dep)
        input$dep
      })
    }

    output$fixed_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("fixed"),
          "Categorical predictors",
          choices = types$fac,
          multiple = TRUE
        ),
        "Help: Pick factor variables that might explain differences in the response."
      )
    })

    output$level_order <- renderUI({
      req(data())
      req(input$fixed)

      df <- data()
      fac_vars <- input$fixed

      if (length(fac_vars) == 0) return(NULL)

      tagList(
        lapply(fac_vars, function(var) {
          values <- df[[var]]
          if (is.factor(values)) lvls <- levels(values)
          else {
            values <- values[!is.na(values)]
            lvls <- unique(as.character(values))
          }
          with_help_tooltip(
            selectInput(
              ns(paste0("order_", var)),
              paste("Order of levels (first = reference)", var),
              choices = lvls,
              selected = lvls,
              multiple = TRUE
            ),
            sprintf("Help: Arrange the levels of %s; the first level becomes the model reference.", var)
          )
        })
      )
    })

    output$covar_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("covar"),
          "Numeric predictors",
          choices = types$num,
          multiple = TRUE
        ),
        "Help: Pick numeric predictors that could help explain the response."
      )
    })

    if (engine == "lmm") {
      output$random_selector <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          selectInput(
            ns("random"),
            "Random effect (categorical)",
            choices = types$fac,
            selected = NULL
          ),
          "Help: Choose a grouping factor for random intercepts in the mixed model."
        )
      })
    }

    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })

    output$formula_preview <- renderUI({
      responses <- selected_responses()
      req(length(responses) > 0)
      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        engine = engine
      )
      reg_formula_preview_ui(ns, responses[1], rhs)
    })

    models <- eventReactive(input$run, {
      req(data())
      df <- data()
      responses <- selected_responses()
      req(length(responses) > 0)

      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        engine = engine
      )

      strat_details <- strat_info()
      safe_fit <- purrr::safely(reg_fit_model)

      fits <- list()
      success_resps <- character(0)
      error_resps <- character(0)
      success_models <- list()
      error_messages <- list()
      flat_models <- list()
      primary_model <- NULL
      primary_error <- NULL

      for (resp in responses) {
        if (is.null(strat_details$var)) {
          result <- safe_fit(resp, rhs, df, engine = engine)
          entry <- list(
            stratified = FALSE,
            strata = list(list(
              label = NULL,
              display = "Overall",
              model = if (is.null(result$error)) result$result else NULL,
              error = if (!is.null(result$error)) result$error$message else NULL
            ))
          )
          fits[[resp]] <- entry

          if (is.null(result$error)) {
            success_resps <- c(success_resps, resp)
            success_models[[resp]] <- list(Overall = result$result)
            flat_models[[length(flat_models) + 1]] <- list(
              response = resp,
              stratum = NULL,
              model = result$result
            )
            if (is.null(primary_model)) primary_model <- result$result
          } else {
            error_resps <- c(error_resps, resp)
            error_messages[[resp]] <- result$error$message
            if (is.null(primary_error)) primary_error <- result$error$message
          }
        } else {
          strata_entries <- list()
          successful_strata <- list()

          for (level in strat_details$levels) {
            subset_data <- df[df[[strat_details$var]] == level, , drop = FALSE]
            if (nrow(subset_data) == 0) {
              msg <- paste0("No observations available for stratum '", level, "'.")
              strata_entries[[length(strata_entries) + 1]] <- list(
                label = level,
                display = level,
                model = NULL,
                error = msg
              )
              next
            }

            result <- safe_fit(resp, rhs, subset_data, engine = engine)
            if (!is.null(result$error)) {
              strata_entries[[length(strata_entries) + 1]] <- list(
                label = level,
                display = level,
                model = NULL,
                error = result$error$message
              )
            } else {
              strata_entries[[length(strata_entries) + 1]] <- list(
                label = level,
                display = level,
                model = result$result,
                error = NULL
              )
              successful_strata[[level]] <- result$result
              flat_models[[length(flat_models) + 1]] <- list(
                response = resp,
                stratum = level,
                model = result$result
              )
              if (is.null(primary_model)) primary_model <- result$result
            }
          }

          fits[[resp]] <- list(
            stratified = TRUE,
            strata = strata_entries
          )

          if (length(successful_strata) > 0) {
            success_resps <- c(success_resps, resp)
            success_models[[resp]] <- successful_strata
          } else {
            error_resps <- c(error_resps, resp)
            errors_vec <- vapply(
              strata_entries,
              function(entry) {
                if (!is.null(entry$error)) {
                  paste0(entry$display, ": ", entry$error)
                } else {
                  NA_character_
                }
              },
              character(1)
            )
            errors_vec <- errors_vec[!is.na(errors_vec)]
            combined_error <- paste(errors_vec, collapse = "\n")
            if (!nzchar(combined_error)) combined_error <- "Model fitting failed."
            error_messages[[resp]] <- combined_error
            if (is.null(primary_error)) primary_error <- combined_error
          }
        }
      }

      list(
        responses = responses,
        success_responses = unique(success_resps),
        error_responses = unique(error_resps),
        fits = fits,
        models = success_models,
        flat_models = flat_models,
        model = primary_model,
        errors = error_messages,
        error = primary_error,
        rhs = rhs,
        allow_multi = allow_multi_response,
        stratification = strat_details
      )
    })

    build_panel_content <- function(idx, response, fit_entry) {
      strata <- fit_entry$strata

      if (!isTRUE(fit_entry$stratified)) {
        stratum <- strata[[1]]
        tagList(
          verbatimTextOutput(ns(paste0("summary_", idx))),
          br(),
          h5("Diagnostics"),
          fluidRow(
            column(6, plotOutput(ns(paste0("resid_", idx)))),
            column(6, plotOutput(ns(paste0("qq_", idx))))
          ),
          br(),
          with_help_tooltip(
            downloadButton(ns(paste0("download_", idx)), "Download results", style = "width: 100%;"),
            "Help: Save the model summary and diagnostics for this response."
          )
        )
      } else {
          stratum_tabs <- lapply(seq_along(strata), function(j) {
            stratum <- strata[[j]]
            label <- if (!is.null(stratum$display)) stratum$display else paste("Stratum", j)

            content <- if (!is.null(stratum$model)) {
              tagList(
                verbatimTextOutput(ns(paste0("summary_", idx, "_", j))),
                br(),
                h5("Diagnostics"),
                fluidRow(
                  column(6, plotOutput(ns(paste0("resid_", idx, "_", j)))),
                  column(6, plotOutput(ns(paste0("qq_", idx, "_", j))))
                ),
                br(),
                with_help_tooltip(
                  downloadButton(ns(paste0("download_", idx, "_", j)), "Download results", style = "width: 100%;"),
                  "Help: Save the model summary and diagnostics for this stratum."
                )
              )
            } else {
              tags$pre(format_safe_error_message("Model fitting failed", stratum$error))
            }

          tabPanel(title = label, content)
        })

        do.call(
          tabsetPanel,
          c(list(id = ns(paste0("strata_tabs_", idx))), stratum_tabs)
        )
      }
    }

    output$results_ui <- renderUI({
      mod <- models()
      req(mod)

      success_resps <- mod$success_responses
      error_resps <- mod$error_responses
      fits <- mod$fits

      error_block <- NULL
      if (!is.null(error_resps) && length(error_resps) > 0) {
        error_block <- lapply(error_resps, function(resp) {
          err <- mod$errors[[resp]]
          tags$pre(
            format_safe_error_message(
              paste("Model fitting failed for", resp),
              if (!is.null(err)) err else ""
            )
          )
        })
      }

      if (is.null(success_resps) || length(success_resps) == 0) {
        if (!is.null(error_block)) return(do.call(tagList, error_block))
        return(NULL)
      }

      panels <- lapply(seq_along(success_resps), function(idx) {
        response <- success_resps[idx]
        fit_entry <- fits[[response]]
        content <- build_panel_content(idx, response, fit_entry)

        if (length(success_resps) > 1) {
          tabPanel(title = response, content)
        } else {
          content
        }
      })

      results_block <- if (length(success_resps) > 1) {
        do.call(tabsetPanel, c(list(id = ns("results_tabs")), panels))
      } else {
        panels[[1]]
      }

      elements <- c(if (!is.null(error_block)) error_block, list(results_block))
      do.call(tagList, elements)
    })

    observeEvent(models(), {
      mod <- models()
      req(mod)

      success_resps <- mod$success_responses
      fits <- mod$fits
      req(success_resps)

      for (idx in seq_along(success_resps)) {
        local({
          local_idx <- idx
          response <- success_resps[local_idx]
          fit_entry <- fits[[response]]

          if (!isTRUE(fit_entry$stratified)) {
            stratum <- fit_entry$strata[[1]]
            model_obj <- stratum$model

            output[[paste0("summary_", local_idx)]] <- renderPrint({
              if (engine == "lm") {
                reg_display_lm_summary(model_obj)
              } else {
                reg_display_lmm_summary(model_obj)
              }
            })

            output[[paste0("resid_", local_idx)]] <- renderPlot({
              plot(fitted(model_obj), resid(model_obj), xlab = "Fitted values", ylab = "Residuals")
              abline(h = 0, lty = 2)
            })

            output[[paste0("qq_", local_idx)]] <- renderPlot({
              qqnorm(resid(model_obj))
              qqline(resid(model_obj))
            })

            output[[paste0("download_", local_idx)]] <- downloadHandler(
              filename = function() {
                paste0(
                  engine,
                  "_results_",
                  response,
                  "_",
                  Sys.Date(),
                  ".docx"
                )
              },
              content = function(file) {
                write_lm_docx(model_obj, file)
              }
            )
          } else {
            strata <- fit_entry$strata
            for (j in seq_along(strata)) {
              local({
                local_j <- j
                stratum <- strata[[local_j]]
                if (is.null(stratum$model)) return()
                model_obj <- stratum$model

                output[[paste0("summary_", local_idx, "_", local_j)]] <- renderPrint({
                  if (engine == "lm") {
                    reg_display_lm_summary(model_obj)
                  } else {
                    reg_display_lmm_summary(model_obj)
                  }
                })

                output[[paste0("resid_", local_idx, "_", local_j)]] <- renderPlot({
                  plot(fitted(model_obj), resid(model_obj), xlab = "Fitted values", ylab = "Residuals")
                  abline(h = 0, lty = 2)
                })

                output[[paste0("qq_", local_idx, "_", local_j)]] <- renderPlot({
                  qqnorm(resid(model_obj))
                  qqline(resid(model_obj))
                })

                output[[paste0("download_", local_idx, "_", local_j)]] <- downloadHandler(
                  filename = function() {
                    paste0(
                      engine,
                      "_results_",
                      response,
                      "_",
                      stratum$display,
                      "_",
                      Sys.Date(),
                      ".docx"
                    )
                  },
                  content = function(file) {
                    write_lm_docx(model_obj, file)
                  }
                )
              })
            }
          }
        })
      }
    }, ignoreNULL = FALSE)

    output$download_model <- downloadHandler(
      filename = function() {
        mod <- models()
        if (is.null(mod) || length(mod$flat_models) == 0) {
          return(paste0(engine, "_results_", Sys.Date(), ".docx"))
        }

        if (length(mod$flat_models) == 1) {
          entry <- mod$flat_models[[1]]
          parts <- c(engine, "results", entry$response)
          if (!is.null(entry$stratum)) parts <- c(parts, entry$stratum)
          paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".docx")
        } else {
          paste0(engine, "_all_results_", Sys.Date(), ".docx")
        }
      },
      content = function(file) {
        mod <- models()
        if (is.null(mod)) stop("No models available. Please run the analysis first.")
        flat_models <- mod$flat_models
        if (length(flat_models) == 0) stop("No models available. Please run the analysis first.")

        if (length(flat_models) == 1) {
          write_lm_docx(flat_models[[1]]$model, file)
        } else {
          doc <- officer::read_docx()
          for (entry in flat_models) {
            tmp <- tempfile(fileext = ".docx")
            sublab <- if (!is.null(entry$stratum)) paste("Stratum:", entry$stratum) else NULL
            
            # pass subtitle so it appears RIGHT under the title
            write_lm_docx(entry$model, tmp, subtitle = sublab)
            
            doc <- officer::body_add_docx(doc, src = tmp)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          print(doc, target = file)
        }
      }
    )

    df_final <- reactive({
      data()
    })

    model_fit <- reactive({
      mod <- models()
      req(mod)
      mod$models
    })

    compiled_results <- reactive({
      mod <- models()
      req(mod)
      compile_regression_results(mod, engine)
    })

    summary_table <- reactive({
      res <- compiled_results()
      req(res)
      res$summary
    })

    effect_table <- reactive({
      res <- compiled_results()
      req(res)
      res$effects
    })

    error_table <- reactive({
      res <- compiled_results()
      req(res)
      res$errors
    })

    reactive({
      mod <- models()
      req(mod)

      data_used <- df_final()

      list(
        analysis_type = if (engine == "lm") "LM" else "LMM",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = NULL,
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        metadata = list(
          responses = mod$responses,
          success_responses = mod$success_responses,
          error_responses = mod$error_responses,
          errors = mod$errors,
          stratification = mod$stratification,
          rhs = mod$rhs,
          allow_multi = mod$allow_multi,
          compiled_errors = error_table(),
          flat_models = mod$flat_models,
          engine = engine
        ),
        type = if (engine == "lm") "lm" else "lmm",
        fits = mod$fits,
        flat_models = mod$flat_models,
        stratification = mod$stratification,
        responses = mod$responses,
        errors = mod$errors
      )
    })
  })
}
