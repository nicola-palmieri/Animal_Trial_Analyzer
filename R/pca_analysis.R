# ===============================================================
# 🧪 Table Analyzer — PCA Module
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      with_help_tooltip(
        selectInput(ns("vars"), "Numeric variables", choices = NULL, multiple = TRUE),
        "Pick the numeric variables whose combined patterns you want PCA to capture."
      ),
      tags$details(
        tags$summary(strong("Advanced options")),
        helpText(paste(
          "Stratify by is not available for PCA because the principal components are computed on the full numeric matrix.",
          "Splitting the data by groups would produce different coordinate systems, making the loadings and scores incomparable across groups."
        ))
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run_pca"), "Show PCA summary", width = "100%"),
          "Compute the principal components for the selected variables."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"),
          "Export the PCA summaries, loadings, and diagnostics to a text file."
        ))
      )
    ),
    results = tagList(
      h5("PCA summary and loadings"),
      verbatimTextOutput(ns("summary")),
      uiOutput(ns("excluded_rows_section"))
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

    run_pca_on_subset <- function(subset_data, selected_vars) {
      if (is.null(subset_data) || nrow(subset_data) == 0) {
        return(list(model = NULL, data = subset_data, message = "No data available for PCA."))
      }

      numeric_subset <- subset_data[, selected_vars, drop = FALSE]
      complete_idx <- stats::complete.cases(numeric_subset)
      numeric_subset <- numeric_subset[complete_idx, , drop = FALSE]
      plot_data <- subset_data[complete_idx, , drop = FALSE]
      original_n <- nrow(subset_data)
      used_n <- nrow(plot_data)
      excluded_n <- original_n - used_n

      excluded_rows <- subset_data[!complete_idx, , drop = FALSE]
      if (excluded_n > 0) {
        excluded_rows <- cbind(
          data.frame(`Row #` = seq_len(nrow(subset_data))[!complete_idx], check.names = FALSE),
          excluded_rows
        )
        rownames(excluded_rows) <- NULL
      } else {
        excluded_rows <- NULL
      }

      if (nrow(numeric_subset) < 2) {
        return(list(
          model = NULL,
          data = plot_data,
          message = "Not enough complete observations to compute PCA.",
          original_n = original_n,
          used_n = used_n,
          excluded_n = excluded_n,
          excluded_rows = excluded_rows
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
          message = conditionMessage(model$error),
          message_title = "PCA computation failed",
          original_n = original_n,
          used_n = used_n,
          excluded_n = excluded_n,
          excluded_rows = excluded_rows
        ))
      }

      list(
        model = model$result,
        data = plot_data,
        message = NULL,
        original_n = original_n,
        used_n = used_n,
        excluded_n = excluded_n,
        excluded_rows = excluded_rows
      )
    }

    # Run PCA
    pca_result <- eventReactive(input$run_pca, {
      req(df())

      data <- df()
      validate(need(nrow(data) > 0, "No data available for PCA."))

      numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      selected_vars <- intersect(input$vars, numeric_vars)
      validate(need(length(selected_vars) > 1, "Select at least two numeric variables for PCA."))

      result <- run_pca_on_subset(data, selected_vars)

      list(
        selected_vars = selected_vars,
        result = result,
        data_used = result$data
      )
    })

    # Verbatim output: summary + loadings
    output$summary <- renderPrint({
      results <- pca_result()
      validate(need(!is.null(results), "Run the PCA analysis to view results."))

      entry <- results$result
      if (is.null(entry) || is.null(entry$model)) {
        if (!is.null(entry)) {
          used_n <- if (!is.null(entry$used_n)) entry$used_n else 0
          original_n <- if (!is.null(entry$original_n)) entry$original_n else 0
          excluded_n <- if (!is.null(entry$excluded_n)) entry$excluded_n else 0
          cat(sprintf(
            "Rows used for PCA: %d of %d (excluded %d due to missing values in the selected variables).\n\n",
            used_n,
            original_n,
            excluded_n
          ))
        }

        if (!is.null(entry) && !is.null(entry$message) && nzchar(entry$message)) {
          if (!is.null(entry$message_title)) {
            cat(format_safe_error_message(entry$message_title, entry$message))
          } else {
            cat(entry$message)
          }
        } else {
          cat("Not enough data to compute PCA.")
        }
        return(invisible())
      }

      model <- entry$model
      cat(sprintf(
        "Rows used for PCA: %d of %d (excluded %d due to missing values in the selected variables).\n\n",
        entry$used_n,
        entry$original_n,
        entry$excluded_n
      ))
      cat("── PCA Summary ──\n")
      print(summary(model))
      cat("\n── PCA Loadings (rotation matrix) ──\n")
      print(round(model$rotation, 3))
      cat("\n── PCA Explained Variance (%) ──\n")
      var_exp <- 100 * model$sdev^2 / sum(model$sdev^2)
      print(round(var_exp, 2))
      cat("\n")

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

        entry <- results$result
        if (!is.null(entry)) {
          used_n <- if (!is.null(entry$used_n)) entry$used_n else 0
          original_n <- if (!is.null(entry$original_n)) entry$original_n else 0
          excluded_n <- if (!is.null(entry$excluded_n)) entry$excluded_n else 0
          cat(sprintf(
            "Rows used for PCA: %d of %d (excluded %d due to missing values in the selected variables).\n\n",
            used_n,
            original_n,
            excluded_n
          ))
        }

        if (is.null(entry) || is.null(entry$model)) {
          if (!is.null(entry) && !is.null(entry$message) && nzchar(entry$message)) {
            if (!is.null(entry$message_title)) {
              cat(format_safe_error_message(entry$message_title, entry$message), "\n", sep = "")
            } else {
              cat(entry$message, "\n", sep = "")
            }
          } else {
            cat("Not enough data to compute PCA.\n")
          }
          return()
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
    )

    # Return structured reactive for integration
    df_final <- reactive({
      details <- pca_result()
      if (is.null(details)) return(NULL)
      details$data_used
    })

    model_fit <- reactive({
      details <- pca_result()
      if (is.null(details)) return(NULL)
      details$result
    })

    compiled_tables <- reactive({
      details <- pca_result()
      if (is.null(details)) return(NULL)

      entry <- details$result
      if (is.null(entry)) return(NULL)

      messages_list <- if (!is.null(entry$message)) list(PCA = entry$message) else NULL

      model <- entry$model
      if (is.null(model)) {
        return(list(summary = NULL, effects = NULL, messages = messages_list))
      }

      rotation_tbl <- as.data.frame(as.table(model$rotation), stringsAsFactors = FALSE)
      colnames(rotation_tbl) <- c("Variable", "Component", "Loading")

      variance_vals <- 100 * model$sdev^2 / sum(model$sdev^2)
      variance_tbl <- data.frame(
        Component = paste0("PC", seq_along(variance_vals)),
        Variance = variance_vals,
        stringsAsFactors = FALSE
      )

      list(
        summary = list(PCA = rotation_tbl),
        effects = list(PCA = variance_tbl),
        messages = messages_list
      )
    })

    summary_table <- reactive({
      res <- compiled_tables()
      if (is.null(res)) return(NULL)
      res$summary
    })

    effect_table <- reactive({
      res <- compiled_tables()
      if (is.null(res)) return(NULL)
      res$effects
    })

    posthoc_results <- reactive(NULL)

    analysis_result <- reactive({
      details <- pca_result()
      if (is.null(details)) {
        return(list(
          analysis_type = "PCA",
          data_used = df(),
          model = NULL,
          summary = NULL,
          posthoc = NULL,
          effects = NULL,
          stats = if (!is.null(df())) list(n = nrow(df()), vars = names(df())) else NULL,
          metadata = list(
            selected_vars = input$vars,
            group_var = NULL,
            strata_levels = NULL,
            messages = NULL,
            complete_cases = NULL,
            excluded_rows = NULL,
            excluded_n = NULL,
            original_n = NULL
          ),
          type = "pca",
          data = df,
          vars = input$vars,
          selected_vars = input$vars,
          group_var = NULL,
          strata_levels = NULL
        ))
      }

      data_used <- df_final()

      compiled <- compiled_tables()
      messages <- if (!is.null(compiled)) compiled$messages else NULL

      entry <- details$result

      list(
        analysis_type = "PCA",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = posthoc_results(),
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        metadata = list(
          selected_vars = details$selected_vars,
          group_var = NULL,
          strata_levels = NULL,
          complete_cases = if (!is.null(entry)) entry$data else NULL,
          excluded_rows = if (!is.null(entry)) entry$excluded_rows else NULL,
          excluded_n = if (!is.null(entry)) entry$excluded_n else NULL,
          original_n = if (!is.null(entry)) entry$original_n else NULL,
          messages = messages
        ),
        type = "pca",
        data = df,
        vars = details$selected_vars,
        selected_vars = details$selected_vars,
        group_var = NULL,
        strata_levels = NULL
      )
    })

    output$excluded_rows_section <- renderUI({
      results <- pca_result()
      req(results)

      entry <- results$result
      if (is.null(entry) || is.null(entry$excluded_n) || entry$excluded_n == 0) {
        return(tags$p("No rows were excluded when computing the PCA."))
      }

      tagList(
        h5(sprintf("Excluded rows (%d)", entry$excluded_n)),
        DT::DTOutput(ns("excluded_table"))
      )
    })

    output$excluded_table <- DT::renderDT({
      results <- pca_result()
      req(results)

      entry <- results$result
      req(entry$excluded_rows)

      DT::datatable(
        entry$excluded_rows,
        options = list(scrollX = TRUE, pageLength = 5),
        rownames = FALSE,
        class = "compact stripe"
      )
    })

    analysis_result
  })
}
