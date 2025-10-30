# ===============================================================
# 🧪 Table Analyzer — PCA Module
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      selectInput(ns("vars"), "Numeric variables:", choices = NULL, multiple = TRUE),
      br(),
      uiOutput(ns("advanced_options")),
      br(),
      fluidRow(
        column(6, actionButton(ns("run_pca"), "Show PCA summary", width = "100%")),
        column(6, downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"))
      )
    ),
    results = tagList(
      h5("PCA summary and loadings"),
      verbatimTextOutput(ns("summary"))
    )
  )
}

pca_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())

    # Dynamically populate numeric variable list
    observe({
      num_vars <- names(df())[sapply(df(), is.numeric)]
      updateSelectInput(session, "vars", choices = num_vars, selected = num_vars)
    })

    output$advanced_options <- renderUI({
      render_stratification_controls(ns, df, input)
    })

    output$strata_order_ui <- renderUI({
      render_strata_order_input(ns, df, input$stratify_var)
    })

    run_pca_on_subset <- function(subset_data, selected_vars) {
      if (is.null(subset_data) || nrow(subset_data) == 0) {
        return(list(model = NULL, data = subset_data, message = "No data available for PCA."))
      }

      numeric_subset <- subset_data[, selected_vars, drop = FALSE]
      complete_idx <- stats::complete.cases(numeric_subset)
      numeric_subset <- numeric_subset[complete_idx, , drop = FALSE]
      plot_data <- subset_data[complete_idx, , drop = FALSE]

      if (nrow(numeric_subset) < 2) {
        return(list(
          model = NULL,
          data = plot_data,
          message = "Not enough complete observations to compute PCA."
        ))
      }

      safe_prcomp <- purrr::safely(function(mat) {
        prcomp(mat, center = TRUE, scale. = TRUE)
      })

      model <- safe_prcomp(numeric_subset)

      if (!is.null(model$error)) {
        return(list(
          model = NULL,
          data = plot_data,
          message = conditionMessage(model$error)
        ))
      }

      list(model = model$result, data = plot_data, message = NULL)
    }

    # Run PCA
    pca_result <- eventReactive(input$run_pca, {
      req(df())

      data <- df()
      validate(need(nrow(data) > 0, "No data available for PCA."))

      numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      selected_vars <- intersect(input$vars, numeric_vars)
      validate(need(length(selected_vars) > 1, "Select at least two numeric variables for PCA."))

      stratify_var <- input$stratify_var
      if (is.null(stratify_var) || identical(stratify_var, "None") ||
          !nzchar(stratify_var) || !(stratify_var %in% names(data))) {
        stratify_var <- NULL
      }

      if (!is.null(stratify_var) && !guard_stratification_levels(data, stratify_var)) {
        return(NULL)
      }

      strata_levels <- NULL
      local_data <- data

      if (!is.null(stratify_var)) {
        values <- local_data[[stratify_var]]
        values <- values[!is.na(values)]
        available_levels <- unique(as.character(values))

        if (!is.null(input$strata_order) && length(input$strata_order) > 0) {
          strata_levels <- input$strata_order[input$strata_order %in% available_levels]
        } else {
          strata_levels <- available_levels
        }

        strata_levels <- strata_levels[nzchar(strata_levels)]

        if (length(strata_levels) == 0) {
          return(list(
            group_var = stratify_var,
            strata_levels = character(0),
            selected_vars = selected_vars,
            results = list()
          ))
        }

        keep_rows <- !is.na(local_data[[stratify_var]]) &
          as.character(local_data[[stratify_var]]) %in% strata_levels

        local_data <- local_data[keep_rows, , drop = FALSE]
        local_data[[stratify_var]] <- factor(
          as.character(local_data[[stratify_var]]),
          levels = strata_levels
        )
      }

      results <- list()

      if (is.null(stratify_var)) {
        results[["Overall"]] <- run_pca_on_subset(local_data, selected_vars)
      } else {
        for (level in strata_levels) {
          subset_data <- local_data[as.character(local_data[[stratify_var]]) == level, , drop = FALSE]
          results[[level]] <- run_pca_on_subset(subset_data, selected_vars)
        }
      }

      list(
        group_var = stratify_var,
        strata_levels = strata_levels,
        selected_vars = selected_vars,
        results = results
      )
    })

    # Verbatim output: summary + loadings
    output$summary <- renderPrint({
      results <- pca_result()
      validate(need(!is.null(results), "Run the PCA analysis to view results."))

      entries <- results$results
      if (is.null(entries) || length(entries) == 0) {
        cat("No PCA results available.")
        return(invisible())
      }

      multiple <- length(entries) > 1

      for (name in names(entries)) {
        entry <- entries[[name]]
        if (multiple) {
          cat(sprintf("===== Stratum: %s =====\n", name))
        }

        if (is.null(entry) || is.null(entry$model)) {
          message <- if (!is.null(entry$message) && nzchar(entry$message)) {
            entry$message
          } else {
            "Not enough data to compute PCA."
          }
          cat(message, "\n\n", sep = "")
          next
        }

        model <- entry$model
        cat("── PCA Summary ──\n")
        print(summary(model))
        cat("\n── PCA Loadings (rotation matrix) ──\n")
        print(round(model$rotation, 3))
        cat("\n── PCA Explained Variance (%) ──\n")
        var_exp <- 100 * model$sdev^2 / sum(model$sdev^2)
        print(round(var_exp, 2))
        cat("\n")
      }

      invisible()
    })

    # Download combined results
    output$download_all <- downloadHandler(
      filename = function() paste0("PCA_results_", Sys.Date(), ".txt"),
      content = function(file) {
        results <- pca_result()
        req(results)

        sink(file)
        on.exit(sink(), add = TRUE)

        entries <- results$results
        if (is.null(entries) || length(entries) == 0) {
          cat("No PCA results available.\n")
          return()
        }

        multiple <- length(entries) > 1

        for (name in names(entries)) {
          entry <- entries[[name]]
          if (multiple) {
            cat(sprintf("===== Stratum: %s =====\n", name))
          }

          if (is.null(entry) || is.null(entry$model)) {
            message <- if (!is.null(entry$message) && nzchar(entry$message)) {
              entry$message
            } else {
              "Not enough data to compute PCA."
            }
            cat(message, "\n\n", sep = "")
            next
          }

          model <- entry$model
          cat("── PCA Summary ──\n")
          print(summary(model))
          cat("\n── PCA Loadings (rotation matrix) ──\n")
          print(round(model$rotation, 3))
          cat("\n── PCA Explained Variance (%) ──\n")
          var_exp <- 100 * model$sdev^2 / sum(model$sdev^2)
          print(round(var_exp, 2))
          cat("\n")
        }
      }
    )

    # Return structured reactive for integration
    reactive({
      details <- pca_result()

      if (is.null(details)) {
        return(list(
          type = "pca",
          model = NULL,
          data = df(),
          vars = input$vars
        ))
      }

      list(
        type = "pca",
        model = details$results,
        data = df(),
        vars = details$selected_vars,
        selected_vars = details$selected_vars,
        group_var = details$group_var,
        strata_levels = details$strata_levels
      )
    })
  })
}
