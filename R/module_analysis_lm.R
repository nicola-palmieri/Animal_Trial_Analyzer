# ===============================================================
# 🧮 Animal Trial Analyzer — Linear Model (LM) Module
# ===============================================================

source("R/module_analysis_lm_helpers.R")
source("R/module_analysis_regression_shared.R")

lm_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("variable_selectors")),
      uiOutput(ns("interaction_select")),  # <-- new dynamic checkbox panel
      hr(),
      uiOutput(ns("formula_preview")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run Linear Model", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
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
    
    output$variable_selectors <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_variable_selectors_ui(ns, types, allow_random = FALSE)  # LM has no random
    })
    
    # --- Dynamic list of possible 2-way interactions ---
    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })
    
    # --- Formula preview ---
    output$formula_preview <- renderUI({
      req(input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, random = NULL, engine = "lm")
      reg_formula_preview_ui(ns, input$dep, rhs)
    })
    
    model <- eventReactive(input$run, {
      req(data(), input$dep)
      rhs <- reg_compose_rhs(input$fixed, input$covar, input$interactions, random = NULL, engine = "lm")
      reg_fit_model(input$dep, rhs, data(), engine = "lm")
    })
    
    
    output$full_summary <- renderPrint({
      req(model())
      if (!is.null(model()$error)) return()
      m <- model()$model
      
      # --- ANOVA (Type III) ---
      aout <- capture.output(anova(m, type = 3))
      signif_idx <- grep("^Signif\\. codes", aout)
      if (length(signif_idx) > 0) {
        remove_idx <- c(signif_idx - 1, signif_idx)
        remove_idx <- remove_idx[remove_idx > 0]
        aout <- aout[-remove_idx]
      }
      cat(paste(aout, collapse = "\n"))
      
      cat("\n\n")
      
      # --- Model summary ---
      sout <- capture.output(summary(m))
      start <- grep("^Scaled residuals:", sout)[1]
      stop  <- grep("^Correlation of Fixed Effects:", sout)[1]
      if (!is.na(start)) {
        if (!is.na(stop)) sout <- sout[start:(stop - 1)]
        else sout <- sout[start:length(sout)]
      }
      
      signif_idx <- grep("^Signif\\. codes", sout)
      if (length(signif_idx) > 0) {
        remove_idx <- c(signif_idx - 1, signif_idx)
        remove_idx <- remove_idx[remove_idx > 0]
        sout <- sout[-remove_idx]
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
      plot(model(), which = 1)
    })
    
    output$qq_plot <- renderPlot({
      req(model())
      plot(model(), which = 2)
    })
    
    output$download_model <- downloadHandler(
      filename = function() paste0("lm_summary_", Sys.Date(), ".docx"),
      content = function(file) {
        req(model())
        write_lm_docx(model(), file)
      }
    )
    
    return(model)
  })
}
