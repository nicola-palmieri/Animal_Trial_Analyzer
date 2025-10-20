# ===============================================================
# 🧬 Common module for LM and LMM  
# ===============================================================

regression_ui <- function(id, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  ns <- NS(id)
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  list(
    config = tagList(
      uiOutput(ns("response_ui")),
      uiOutput(ns("fixed_selector")),
      uiOutput(ns("level_order")),
      uiOutput(ns("covar_selector")),
      if (engine == "lmm") uiOutput(ns("random_selector")),
      uiOutput(ns("interaction_select")),
      hr(),
      uiOutput(ns("formula_preview")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download All Results", width = "100%"))
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

    output$response_ui <- renderUI({
      req(data())
      if (allow_multi_response) {
        render_response_inputs(ns, data, input)
      } else {
        types <- reg_detect_types(data())
        selectInput(ns("dep"), "Response variable (numeric):", choices = types$num)
      }
    })

    output$fixed_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      selectInput(
        ns("fixed"),
        "Categorical predictors:",
        choices = types$fac,
        multiple = TRUE
      )
    })
    
    output$level_order <- renderUI({
      req(data())
      req(input$fixed)
      
      df <- data()
      fac_vars <- input$fixed
      if (engine == "lmm" && !is.null(input$random) && nzchar(input$random)) {
        fac_vars <- unique(c(fac_vars, input$random))
      }
      
      if (length(fac_vars) == 0) return(NULL)
      
      tagList(
        lapply(fac_vars, function(var) {
          values <- df[[var]]
          if (is.factor(values)) lvls <- levels(values)
          else {
            values <- values[!is.na(values)]
            lvls <- unique(as.character(values))
          }
          selectInput(
            ns(paste0("order_", var)),
            paste("Order of levels for", var, "(first = reference):"),
            choices = lvls,
            selected = lvls,
            multiple = TRUE
          )
        })
      )
    })
    
    output$covar_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      selectInput(
        ns("covar"),
        "Numeric predictors:",
        choices = types$num,
        multiple = TRUE
      )
    })
    
    if (engine == "lmm") {
      output$random_selector <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        selectInput(
          ns("random"),
          "Random effect (categorical):",
          choices = types$fac,
          selected = NULL
        )
      })
    }
    
    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })

    selected_responses <- reactive({
      if (allow_multi_response) {
        get_selected_responses(input)
      } else {
        req(input$dep)
        input$dep
      }
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
      responses <- selected_responses()
      req(length(responses) > 0)
      
      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        engine = engine
      )
      
      safe_fit <- purrr::safely(reg_fit_model)
      fits <- lapply(responses, function(resp) {
        result <- safe_fit(resp, rhs, data(), engine = engine)
        if (!is.null(result$error)) {
          list(error = result$error$message, model = NULL)
        } else {
          list(error = NULL, model = result$result)
        }
      })
      names(fits) <- responses
      
      success_resps <- names(fits)[vapply(fits, function(x) !is.null(x$model), logical(1))]
      error_resps <- names(fits)[vapply(fits, function(x) is.null(x$model) && !is.null(x$error), logical(1))]
      
      successful_models <- lapply(fits[success_resps], function(x) x$model)
      if (length(successful_models) > 0) names(successful_models) <- success_resps
      
      error_messages <- lapply(fits[error_resps], function(x) x$error)
      if (length(error_messages) > 0) names(error_messages) <- error_resps
      
      primary_model <- if (length(successful_models) > 0) successful_models[[1]] else NULL
      primary_error <- if (length(successful_models) == 0 && length(error_messages) > 0) error_messages[[1]] else NULL
      
      list(
        responses = responses,
        success_responses = success_resps,
        error_responses = error_resps,
        fits = fits,
        models = successful_models,
        model = primary_model,
        errors = error_messages,
        error = primary_error,
        rhs = rhs,
        allow_multi = allow_multi_response
      )
    })

    build_panel_content <- function(idx, response) {
      tagList(
        verbatimTextOutput(ns(paste0("summary_", idx))),
        br(),
        h5("Diagnostics"),
        fluidRow(
          column(6, plotOutput(ns(paste0("resid_", idx)))),
          column(6, plotOutput(ns(paste0("qq_", idx))))
        ),
        br(),
        downloadButton(ns(paste0("download_", idx)), "Download Results")
      )
    }

    output$results_ui <- renderUI({
      mod <- models()
      if (is.null(mod)) return(NULL)
      
      success_resps <- mod$success_responses
      error_resps <- mod$error_responses
      fits <- mod$fits
      
      error_block <- NULL
      if (!is.null(error_resps) && length(error_resps) > 0) {
        error_items <- lapply(error_resps, function(resp) {
          err <- fits[[resp]]$error
          tags$li(tags$strong(resp), ": ", err)
        })
        error_block <- div(
          class = "alert alert-warning",
          strong("Models with errors:"),
          tags$ul(error_items)
        )
      }
      
      if (is.null(success_resps) || length(success_resps) == 0) {
        if (!is.null(error_block)) return(tagList(error_block))
        return(NULL)
      }
      
      panels <- lapply(seq_along(success_resps), function(idx) {
        response <- success_resps[idx]
        content <- build_panel_content(idx, response)
        if (length(success_resps) > 1) {
          tabPanel(title = response, content)
        } else {
          content
        }
      })
      
      results_block <- if (length(success_resps) > 1) {
        do.call(tabsetPanel, c(list(id = ns("results_tabs")) , panels))
      } else {
        panels[[1]]
      }
      
      tagList(
        if (!is.null(error_block)) error_block,
        results_block
      )
    })
    
    observeEvent(models(), {
      mod <- models()
      if (is.null(mod)) return()
      success_resps <- mod$success_responses
      fits <- mod$fits
      if (is.null(success_resps) || length(success_resps) == 0) return()
      
      for (idx in seq_along(success_resps)) {
        local({
          local_idx <- idx
          response_name <- success_resps[local_idx]
          model_obj <- fits[[response_name]]$model
          
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
                response_name,
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
    }, ignoreNULL = FALSE)
    
    output$download_model <- downloadHandler(
      filename = function() {
        mod <- models()
        if (is.null(mod)) {
          return(paste0(engine, "_results_", Sys.Date(), ".docx"))
        }
        success_models <- mod$models
        if (allow_multi_response && length(success_models) > 1) {
          paste0(engine, "_all_results_", Sys.Date(), ".docx")
        } else if (length(success_models) >= 1) {
          first_name <- names(success_models)[1]
          paste0(engine, "_results_", first_name, "_", Sys.Date(), ".docx")
        } else {
          paste0(engine, "_results_", Sys.Date(), ".docx")
        }
      },
      content = function(file) {
        mod <- models()
        if (is.null(mod)) stop("No models available. Please run the analysis first.")
        success_models <- mod$models
        if (length(success_models) == 0) stop("No models available. Please run the analysis first.")
        
        if (!allow_multi_response || length(success_models) == 1) {
          write_lm_docx(success_models[[1]], file)
        } else {
          doc <- officer::read_docx()
          for (i in seq_along(success_models)) {
            tmp <- tempfile(fileext = ".docx")
            write_lm_docx(success_models[[i]], tmp)
            doc <- officer::body_add_docx(doc, src = tmp)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          print(doc, target = file)
        }
      }
    )
    
    return(reactive({
      mod <- models()
      if (is.null(mod)) return(NULL)
      attr(mod, "engine") <- engine
      mod
    }))
  })
}
