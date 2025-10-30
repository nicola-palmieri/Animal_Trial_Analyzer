# ===============================================================
# 🧪 Animal Trial Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      selectInput(ns("vars"), "Numeric variables:", choices = NULL, multiple = TRUE),
      br(),
      uiOutput(ns("advanced_options")),
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

    output$advanced_options <- renderUI({
      render_stratification_controls(ns, df, input)
    })

    output$strata_order_ui <- renderUI({
      render_strata_order_input(ns, df, input$stratify_var)
    })

    # ---- Update variable selector ----
    observe({
      req(df())
      num_vars <- names(df())[sapply(df(), is.numeric)]
      updateSelectInput(session, "vars", choices = num_vars, selected = num_vars)
    })

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
          selected_vars = selected_vars
        ))
        return()
      }

      group_var <- input$stratify_var
      if (is.null(group_var) || identical(group_var, "None") || identical(group_var, "")) {
        group_var <- NULL
      }

      if (!is.null(group_var) && !guard_stratification_levels(data, group_var)) {
        correlation_store(NULL)
        return()
      }

      strata_levels <- "Overall"
      if (!is.null(group_var) && group_var %in% names(data)) {
        values <- data[[group_var]]
        values <- values[!is.na(values)]
        if (!is.null(input$strata_order) && length(input$strata_order) > 0) {
          unique_values <- unique(as.character(values))
          strata_levels <- input$strata_order[input$strata_order %in% unique_values]
        } else {
          strata_levels <- unique(as.character(values))
        }
      }

      matrices <- list()
      plots <- list()

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
        selected_vars = selected_vars
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
          cat(sprintf("Stratum: %s\n", name))
        }
        if (is.null(mat)) {
          cat("  No data available for this stratum.\n\n")
        } else {
          print(round(mat, 2))
          cat("\n")
        }
      }
    })

    # ---- Return structured output for visualization ----
    reactive({
      list(
        type = "pairwise_correlation",
        data = df,
        group_var = reactive(input$stratify_var),
        strata_order = reactive(input$strata_order),
        results = reactive(correlation_store())
      )
    })
  })
}
