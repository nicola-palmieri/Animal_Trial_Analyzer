# ===============================================================
# 🧪 Animal Trial Analyzer — One-way ANOVA Module (Refactored)
# ===============================================================

source("R/module_analysis_anova_shared.R")

one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      uiOutput(ns("advanced_options")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run One-way ANOVA", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download All Results", width = "100%"))
      )
    ),
    results = tagList(
      uiOutput(ns("summary_ui"))
    )
  )
}

one_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # -----------------------------------------------------------
    # Reactive data
    # -----------------------------------------------------------
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    # -----------------------------------------------------------
    # Dynamic inputs
    # -----------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      num_cols <- names(data)[sapply(data, is.numeric)]
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]
      
      tagList(
        checkboxInput(ns("multi_resp"), "Enable multiple response variables", value = FALSE),
        uiOutput(ns("response_selector")),
        selectInput(
          ns("group"),
          "Categorical predictor:",
          choices = cat_cols,
          selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
        )
      )
    })
    
    output$response_selector <- renderUI({
      render_response_selector(ns, df, input)
    })
    
    output$advanced_options <- renderUI({
      render_advanced_options(ns, df, input)
    })
    
    output$strata_order_ui <- renderUI({
      req(df())
      strat_var <- input$stratify_var
      if (is.null(strat_var) || identical(strat_var, "None")) return(NULL)
      
      data <- df()
      values <- data[[strat_var]]
      if (is.null(values)) return(NULL)
      
      if (is.factor(values)) {
        strata_levels <- levels(values)
      } else {
        values <- values[!is.na(values)]
        strata_levels <- unique(as.character(values))
      }
      
      if (length(strata_levels) == 0) return(NULL)
      
      selectInput(
        ns("strata_order"),
        paste("Order of levels for", strat_var, "(strata):"),
        choices = strata_levels,
        selected = strata_levels,
        multiple = TRUE
      )
    })
    
    # -----------------------------------------------------------
    # Level order selection
    # -----------------------------------------------------------
    output$level_order <- renderUI({
      req(df(), input$group)
      levels <- unique(as.character(df()[[input$group]]))
      selectInput(
        ns("order"),
        "Order of levels (first = reference):",
        choices = levels,
        selected = levels,
        multiple = TRUE
      )
    })
    
    # -----------------------------------------------------------
    # Model fitting (via shared helper)
    # -----------------------------------------------------------
    models <- eventReactive(input$run, {
      req(df(), input$response, input$group, input$order)
      responses <- input$response
      if (!isTRUE(input$multi_resp)) responses <- responses[1]
      responses <- unique(responses)
      req(length(responses) > 0)
      
      prepare_stratified_models(
        df = df(),
        responses = responses,
        strat_var = input$stratify_var,
        factor1 = input$group,
        factor2 = NULL,
        orders = list(order1 = input$order, order2 = NULL),
        formula_builder = function(resp, f1, f2) as.formula(paste(resp, "~", f1))
      )
    })
    
    # -----------------------------------------------------------
    # Download all results as one combined DOCX
    # -----------------------------------------------------------
    output$download_all <- downloadHandler(
      
      filename = function() {
        paste0("anova_all_results_", Sys.Date(), ".docx")
      },
      content = function(file) {
        model_info <- models()
        download_all_anova_results(model_info, file)
      }
    )
    
    # -----------------------------------------------------------
    # Render results (shared UI generator)
    # -----------------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "One-way ANOVA")
    })
    
    # -----------------------------------------------------------
    # Render model summaries + download buttons
    # -----------------------------------------------------------
    observeEvent(models(), {
      model_info <- models()
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
            
            output[[paste0("summary_", idx)]] <- renderPrint({
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
            })
            
            output[[paste0("download_", idx)]] <- downloadHandler(
              filename = function() {
                safe_resp <- sanitize_name(response_name)
                paste0("anova_results_", safe_resp, "_", Sys.Date(), ".docx")
              },
              content = function(file) {
                results <- prepare_anova_outputs(model_obj, factors)
                write_anova_docx(file, results, model_obj, response_name)
              }
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
            
            output[[paste0("summary_", idx, "_", stratum_idx)]] <- renderPrint({
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
            })
            
            output[[paste0("download_", idx, "_", stratum_idx)]] <- downloadHandler(
              filename = function() {
                paste0(
                  "anova_results_",
                  sanitize_name(response_name),
                  "_stratum_",
                  sanitize_name(stratum_label),
                  "_",
                  Sys.Date(),
                  ".docx"
                )
              },
              content = function(file) {
                results <- prepare_anova_outputs(model_obj, factors)
                write_anova_docx(file, results, model_obj, response_name, stratum_label)
              }
            )
          })
        }
      }
    })
    
    return(models)
  })
}
