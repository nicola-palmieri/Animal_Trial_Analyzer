# ===============================================================
# 🧬 Common module for LM and LMM  
# ===============================================================

regression_ui <- function(id, engine = c("lm", "lmm")) {
  ns <- NS(id)
  engine <- match.arg(engine)
  tagList(
    uiOutput(ns("variable_selectors")),
    uiOutput(ns("interaction_select")),
    hr(),
    uiOutput(ns("formula_preview")),
    br(),
    fluidRow(
      column(6, actionButton(ns("run"), "Run", width = "100%")),
      column(6, downloadButton(ns("download_model"), "Download All Results", width = "100%"))
    ),
    verbatimTextOutput(ns("fit_error")),
    verbatimTextOutput(ns("full_summary")),
    h5("Diagnostics"),
    fluidRow(
      column(6, plotOutput(ns("resid_plot"))),
      column(6, plotOutput(ns("qq_plot")))
    )
  )
}

regression_server <- function(id, data, engine = c("lm", "lmm")) {
  engine <- match.arg(engine)
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$variable_selectors <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_variable_selectors_ui(ns, types, allow_random = (engine == "lmm"))
    })
    
    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })
    
    output$formula_preview <- renderUI({
      req(input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, 
                             if (engine == "lmm") input$random else NULL, engine = engine)
      reg_formula_preview_ui(ns, input$dep, rhs)
    })
    
    model <- eventReactive(input$run, {
      req(data(), input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, 
                             if (engine == "lmm") input$random else NULL, engine = engine)
      safe_fit <- purrr::safely(reg_fit_model)
      result <- safe_fit(input$dep, rhs, data(), engine = engine)
      if (!is.null(result$error)) return(list(error = result$error$message, model = NULL))
      list(error = NULL, model = result$result)
    })
    
    output$fit_error <- renderPrint({
      req(model())
      if (!is.null(model()$error)) cat("⚠️ Model fitting error:\n", model()$error)
    })
    
    output$full_summary <- renderPrint({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      if (engine == "lm") {
        reg_display_lm_summary(m)
      } else {
        reg_display_lmm_summary(m)
      }
    })
    
    output$resid_plot <- renderPlot({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      plot(fitted(m), resid(m), xlab = "Fitted values", ylab = "Residuals")
      abline(h = 0, lty = 2)
    })
    
    output$qq_plot <- renderPlot({
      req(model())
      if (!is.null(model()$error)) return()
      qqnorm(resid(model()$model)); qqline(resid(model()$model))
    })
    
    output$download_model <- downloadHandler(
      filename = function() paste0(engine, "_results_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        write_lm_docx(model()$model, file)
      }
    )
    
    return(reactive({
      m <- model()
      attr(m, "engine") <- engine
      m
    }))
  })
}
