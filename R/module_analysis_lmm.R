# ===============================================================
# 🧬 Linear Mixed Model (LMM) — single random intercept
# ===============================================================

source("R/module_analysis_lmm_helpers.R")
source("R/module_analysis_regression_shared.R")

lmm_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("variable_selectors")),   # includes random
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

lmm_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selectors <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_variable_selectors_ui(ns, types, allow_random = TRUE)
    })
    
    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })
    
    output$formula_preview <- renderUI({
      req(input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, input$random, engine = "lmm")
      reg_formula_preview_ui(ns, input$dep, rhs)
    })
    
    model <- eventReactive(input$run, {
      req(data(), input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, input$random, engine = "lmm")
      
      safe_fit <- purrr::safely(reg_fit_model)
      result <- safe_fit(input$dep, rhs, data(), engine = "lmm")
      
      if (!is.null(result$error)) {
        return(list(error = result$error$message, model = NULL))
      }
      
      list(error = NULL, model = result$result)
    })
    
    output$fit_error <- renderPrint({
      req(model())
      if (!is.null(model()$error)) {
        cat("⚠️ Model fitting error:\n", model()$error)
      }
    })
    
    # --- Results (Type III + summary, cleaned) ---
    output$full_summary <- renderPrint({
      req(model())
      if (!is.null(model()$error)) return()  # stop if there was an error
      m <- model()$model
      
      aout <- capture.output(anova(m, type = 3))
      cat(paste(aout, collapse = "\n"))
      
      cat("\n\n")
      
      sout <- capture.output(summary(m))
      start <- grep("^Scaled residuals:", sout)[1]
      stop  <- grep("^Correlation of Fixed Effects:", sout)[1]
      if (!is.na(start)) {
        if (!is.na(stop)) sout <- sout[start:(stop - 1)]
        else sout <- sout[start:length(sout)]
      }
      
      icc_df <- compute_icc(m)
      if (!is.null(icc_df) && nrow(icc_df) > 0) {
        icc_line <- paste(
          paste0("ICC (", icc_df$Group, "): ", icc_df$ICC),
          collapse = "; "
        )
        random_idx <- grep("^Random effects:", sout)[1]
        if (!is.na(random_idx)) {
          sout <- append(sout, paste0("\n", icc_line), after = random_idx + 4)
        } else {
          sout <- c(sout, icc_line)
        }
      }
      
      cat(paste(sout, collapse = "\n"))
    })
    
    
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
    
    
    output$download_model <- downloadHandler(
      filename = function() paste0("lmm_results_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        # You can reuse write_lm_docx if you like the same layout,
        # or make a write_lmm_docx() that calls lmerTest::anova(model, type=3)
        write_lm_docx(model(), file)
      }
    )
    
    return(reactive({
      m <- model()
      attr(m, "engine") <- "lmm"
      m
    }))
  })
}
