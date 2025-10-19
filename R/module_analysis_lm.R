# ===============================================================
# 🧮 Linear Model (LM) — fixed effects only
# ===============================================================

source("R/module_analysis_lm_helpers.R")
source("R/module_analysis_regression_shared.R")

lm_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("variable_selectors")),
      uiOutput(ns("interaction_select")),
      hr(),
      uiOutput(ns("formula_preview")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run Linear Model", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
      verbatimTextOutput(ns("fit_error")),
      verbatimTextOutput(ns("full_summary")),
      h5("Diagnostics"),
      fluidRow(
        column(6, plotOutput(ns("resid_plot"))),
        column(6, plotOutput(ns("qq_plot")))
      )
    )
  )
}

lm_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Variable selectors
    output$variable_selectors <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_variable_selectors_ui(ns, types, allow_random = FALSE)
    })
    
    # --- Possible interactions
    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })
    
    # --- Formula preview
    output$formula_preview <- renderUI({
      req(input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, random = NULL, engine = "lm")
      reg_formula_preview_ui(ns, input$dep, rhs)
    })
    
    # --- Safe model fitting
    model <- eventReactive(input$run, {
      req(data(), input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, random = NULL, engine = "lm")
      
      safe_fit <- purrr::safely(reg_fit_model)
      result <- safe_fit(input$dep, rhs, data(), engine = "lm")
      
      if (!is.null(result$error)) {
        return(list(error = result$error$message, model = NULL))
      }
      
      list(error = NULL, model = result$result)
    })
    
    # --- Error message
    output$fit_error <- renderPrint({
      req(model())
      if (!is.null(model()$error)) {
        cat("⚠️ Model fitting error:\n", model()$error)
      }
    })
    
    # --- ANOVA + summary (cleaned)
    output$full_summary <- renderPrint({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      
      # --- ANOVA (Type III) ---
      aout <- capture.output(car::Anova(m, type = 3))
      
      # Remove "Signif. codes" and preceding '---' line
      signif_idx <- grep("^Signif\\. codes", aout)
      if (length(signif_idx) > 0) {
        remove_idx <- c(signif_idx - 1, signif_idx)
        remove_idx <- remove_idx[remove_idx > 0]
        aout <- aout[-remove_idx]
      }
      
      cat(paste(aout, collapse = "\n"))
      cat("\n\n")
      
      # --- Model summary (only coefficients section) ---
      sout <- capture.output(summary(m))
      
      # Find start and stop positions
      start <- grep("^Residuals:", sout)[1]
      stop  <- grep("^Signif\\. codes", sout)[1]
      
      # If present, trim section
      if (!is.na(start)) {
        if (!is.na(stop)) {
          sout <- sout[start:(stop - 2)]  # remove the '---' and 'Signif. codes' lines
        } else {
          sout <- sout[start:length(sout)]
        }
      }
      
      # Just in case "Signif. codes" appears unexpectedly
      signif_idx <- grep("^Signif\\. codes", sout)
      if (length(signif_idx) > 0) {
        remove_idx <- c(signif_idx - 1, signif_idx)
        remove_idx <- remove_idx[remove_idx > 0]
        sout <- sout[-remove_idx]
      }
      
      cat(paste(sout, collapse = "\n"))
    })
    
    
    
    # --- Diagnostic plots
    output$resid_plot <- renderPlot({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      
      plot(fitted(m), resid(m),
           xlab = "Fitted values", ylab = "Residuals")
      abline(h = 0, lty = 2)
    })
    
    output$qq_plot <- renderPlot({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      qqnorm(resid(m)); qqline(resid(m))
    })
    
    # --- Download handler
    output$download_model <- downloadHandler(
      filename = function() paste0("lm_results_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        write_lm_docx(model()$model, file)
      }
    )
    
    # --- Return standardized object
    return(reactive({
      m <- model()
      attr(m, "engine") <- "lm"
      m
    }))
  })
}
