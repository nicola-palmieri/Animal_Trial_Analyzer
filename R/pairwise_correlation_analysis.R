# ===============================================================
# 🧪 Table  Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      selectInput(ns("vars"), "Numeric variables:", choices = NULL, multiple = TRUE),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Show correlation matrix", width = "100%")),
        column(6, downloadButton(ns("download_model"), "Download all results", style = "width: 100%;"))
      )
    ),
    results = tagList(
      h5("Correlation matrix"),
      verbatimTextOutput(ns("summary"))
    )
  )
}

ggpairs_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(data_reactive())

    strat_info <- stratification_server("strat", df)

    # ---- Update variable selector ----
    selection_initialized <- reactiveVal(FALSE)
    last_selected <- reactiveVal(NULL)

    observeEvent(input$vars, {
      last_selected(input$vars %||% character())
    }, ignoreNULL = FALSE)

    repopulate_numeric_selector <- function() {
      data <- isolate(df())
      if (is.null(data)) return()

      num_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      current_input <- isolate(input$vars)
      previous <- current_input
      if (is.null(previous)) {
        previous <- last_selected()
      }
      previous <- intersect(previous %||% character(), num_vars)

      selected <- previous
      if (!selection_initialized() && length(num_vars) > 0) {
        selected <- if (length(previous) > 0) previous else num_vars
        selection_initialized(TRUE)
      }

      updateSelectInput(session, "vars", choices = num_vars, selected = selected)
    }

    observeEvent(df(), {
      repopulate_numeric_selector()
    }, ignoreNULL = FALSE)

    session$onFlushed(function() {
      repopulate_numeric_selector()
    }, once = FALSE)

    build_ggpairs_object <- function(data) {
      GGally::ggpairs(
        data,
        progress = FALSE,
        upper = list(
          continuous = GGally::wrap("cor", size = 4, color = basic_color_palette[1])
        ),
        lower = list(
          continuous = GGally::wrap("points", alpha = 0.6, color = basic_color_palette[1], size = 1.5)
        ),
        diag = list(
          continuous = GGally::wrap("densityDiag", fill = basic_color_palette[1], alpha = 0.4)
        )
      ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", size = 9),
          panel.grid.minor = ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          plot.title = ggplot2::element_text(size = 12, face = "bold")
        )
    }

    correlation_store <- reactiveVal(NULL)

    # ---- Compute correlation matrix ----
    observeEvent(input$run, {
      req(df())
      data <- df()
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      selected_vars <- if (length(input$vars)) input$vars else numeric_vars
      selected_vars <- intersect(selected_vars, numeric_vars)

      if (length(selected_vars) < 2) {
        correlation_store(list(
          message = "Need at least two numeric columns.",
          matrices = list(),
          plots = list(),
          group_var = NULL,
          selected_vars = selected_vars,
          data_used = NULL,
          strata_levels = NULL
        ))
        return()
      }

      strat_details <- strat_info()
      group_var <- strat_details$var

      strata_levels <- "Overall"
      if (!is.null(group_var) && group_var %in% names(data)) {
        levels <- strat_details$levels
        if (is.null(levels) || length(levels) == 0) {
          values <- data[[group_var]]
          values <- values[!is.na(values)]
          strata_levels <- unique(as.character(values))
        } else {
          strata_levels <- levels
        }
      } else {
        group_var <- NULL
      }

      matrices <- list()
      plots <- list()
      processed_data <- data[, unique(c(selected_vars, group_var)), drop = FALSE]

      if (!is.null(group_var)) {
        keep_rows <- !is.na(processed_data[[group_var]]) &
          as.character(processed_data[[group_var]]) %in% strata_levels
        processed_data <- processed_data[keep_rows, , drop = FALSE]
        processed_data[[group_var]] <- factor(
          as.character(processed_data[[group_var]]),
          levels = strata_levels
        )
      }

      if (is.null(group_var)) {
        dat <- data[, selected_vars, drop = FALSE]
        cor_matrix <- cor(dat, use = "pairwise.complete.obs")
        matrices[["Overall"]] <- cor_matrix
        plots[["Overall"]] <- build_ggpairs_object(dat)
      } else {
        for (level in strata_levels) {
          subset_rows <- !is.na(data[[group_var]]) & as.character(data[[group_var]]) == level
          subset_data <- data[subset_rows, , drop = FALSE]
          if (nrow(subset_data) == 0) {
            matrices[[level]] <- NULL
            plots[[level]] <- NULL
            next
          }
          dat <- subset_data[, selected_vars, drop = FALSE]
          cor_matrix <- suppressWarnings(cor(dat, use = "pairwise.complete.obs"))
          matrices[[level]] <- cor_matrix
          plots[[level]] <- build_ggpairs_object(dat)
        }
      }

      correlation_store(list(
        matrices = matrices,
        plots = plots,
        group_var = group_var,
        selected_vars = selected_vars,
        data_used = processed_data,
        strata_levels = if (!is.null(group_var)) strata_levels else NULL
      ))
    })

    output$summary <- renderPrint({
      results <- correlation_store()
      if (is.null(results)) {
        return(invisible(NULL))
      }

      if (!is.null(results$message)) {
        cat(results$message)
        return(invisible(NULL))
      }

      matrices <- results$matrices
      if (is.null(matrices) || length(matrices) == 0) {
        return(invisible(NULL))
      }

      multiple <- length(matrices) > 1
      for (name in names(matrices)) {
        mat <- matrices[[name]]
        if (multiple) {
          cat(sprintf("=== Stratum: %s ===\n", name))
        }
        if (is.null(mat)) {
          cat("  No data available for this stratum.\n\n")
        } else {
          print(round(mat, 2))
          cat("\n")
        }
      }
    })
    
    # ---- Download results ----
    output$download_model <- downloadHandler(
      filename = function() paste0("Correlation_results_", Sys.Date(), ".txt"),
      content = function(file) {
        res <- correlation_store()
        if (is.null(res)) return()
        sink(file)
        on.exit(sink(), add = TRUE)
        
        if (!is.null(res$message)) {
          cat(res$message, "\n")
          return()
        }
        
        matrices <- res$matrices
        if (is.null(matrices) || length(matrices) == 0) {
          cat("No correlation matrices available.\n")
          return()
        }
        
        multiple <- length(matrices) > 1
        for (nm in names(matrices)) {
          mat <- matrices[[nm]]
          if (multiple) cat(sprintf("=== Stratum: %s ===\n", nm))
          if (is.null(mat)) {
            cat("No data available for this stratum.\n\n")
          } else {
            print(round(mat, 3))
            cat("\n")
          }
        }

      }
    )

    # ---- Return structured output for visualization ----
    df_final <- reactive({
      res <- correlation_store()
      if (is.null(res)) return(NULL)
      res$data_used
    })

    model_fit <- reactive({
      res <- correlation_store()
      if (is.null(res)) return(NULL)
      res$matrices
    })

    summary_table <- reactive({
      res <- correlation_store()
      if (is.null(res)) return(NULL)
      res$matrices
    })

    posthoc_results <- reactive(NULL)

    effect_table <- reactive(NULL)

    reactive({
      res <- correlation_store()
      if (is.null(res)) return(NULL)

      data_used <- df_final()

      list(
        analysis_type = "CORR",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = posthoc_results(),
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        metadata = list(
          selected_vars = res$selected_vars,
          group_var = res$group_var,
          strata_levels = res$strata_levels,
          plots = res$plots,
          message = res$message
        ),
        type = "pairs",
        data = df,
        group_var = reactive({
          det <- correlation_store()
          if (is.null(det)) return(NULL)
          det$group_var
        }),
        strata_order = reactive({
          det <- correlation_store()
          if (is.null(det)) return(NULL)
          det$strata_levels
        }),
        results = reactive(correlation_store())
      )
    })
  })
}
