# ===============================================================
# 📈 Longitudinal visualization helpers
# ===============================================================

visualize_longitudinal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        wellPanel(
          h4("Plot options"),
          sliderInput(
            ns("ci_level"),
            "Confidence level:",
            min = 0.8,
            max = 0.99,
            value = 0.95,
            step = 0.01
          ),
          checkboxInput(ns("show_points"), "Show points", value = TRUE),
          checkboxInput(ns("show_lines"), "Show lines", value = TRUE),
          checkboxInput(ns("show_interval"), "Show confidence interval", value = TRUE),
          uiOutput(ns("strata_controls")),
          uiOutput(ns("facet_controls"))
        )
      ),
      column(
        width = 8,
        plotOutput(ns("longitudinal_plot"), height = "480px"),
        br(),
        uiOutput(ns("notes"))
      )
    )
  )
}

longitudinal_prepare_emmeans <- function(model, metadata, ci_level = 0.95) {
  time_var <- metadata$time_var
  treatment_var <- metadata$treatment_var
  if (is.null(time_var) || !nzchar(time_var)) stop("Time variable missing in metadata.")
  if (is.null(treatment_var) || !nzchar(treatment_var)) stop("Treatment variable missing in metadata.")

  emm <- emmeans::emmeans(
    model,
    specs = stats::as.formula(paste("~", time_var, "*", treatment_var)),
    level = ci_level,
    type = if (!is.null(metadata$family) && metadata$family == "binomial") "response" else "link"
  )
  df <- as.data.frame(emm)
  df$estimate <- df[["emmean"]]
  if ("lower.CL" %in% names(df)) {
    df$lower <- df[["lower.CL"]]
  } else if ("lower" %in% names(df)) {
    df$lower <- df[["lower"]]
  }
  if ("upper.CL" %in% names(df)) {
    df$upper <- df[["upper.CL"]]
  } else if ("upper" %in% names(df)) {
    df$upper <- df[["upper"]]
  }
  df$time <- df[[time_var]]
  df$treatment <- df[[treatment_var]]
  df
}

visualize_longitudinal_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    info <- reactive({
      res <- model_info()
      validate(need(!is.null(res) && identical(res$type, "longitudinal"), "Run a longitudinal model first."))
      res
    })

    metadata <- reactive({
      info()$metadata
    })

    available_models <- reactive({
      meta <- metadata()
      validate(need(!is.null(meta$models) && length(meta$models) > 0, "No fitted longitudinal models available."))
      meta$models
    })

    strata_levels <- reactive({
      strat <- info()$stratification
      if (is.null(strat$var)) return(NULL)
      levels <- strat$levels
      if (is.null(levels) || length(levels) == 0) {
        levels <- names(available_models())
      }
      levels[nzchar(levels)]
    })

    output$strata_controls <- renderUI({
      levels <- strata_levels()
      if (is.null(levels) || length(levels) <= 1) return(NULL)
      selectInput(
        ns("strata_filter"),
        "Strata to display:",
        choices = levels,
        selected = levels,
        multiple = TRUE
      )
    })

    output$facet_controls <- renderUI({
      levels <- strata_levels()
      if (is.null(levels) || length(levels) == 0) return(NULL)
      selectInput(
        ns("facet_layout"),
        "Facet layout:",
        choices = c("Wrap" = "wrap", "Rows" = "rows", "Columns" = "cols", "None" = "none"),
        selected = "wrap"
      )
    })

    estimation_data <- reactive({
      models <- available_models()
      meta <- metadata()
      ci <- input$ci_level %||% 0.95
      selected_strata <- input$strata_filter
      if (!is.null(selected_strata)) {
        models <- models[names(models) %in% selected_strata]
      }
      validate(need(length(models) > 0, "No strata selected."))
      purrr::imap_dfr(models, function(model, name) {
        df <- longitudinal_prepare_emmeans(model, meta, ci_level = ci)
        df$stratum <- if (is.null(name) || !nzchar(name)) "Overall" else name
        df
      })
    })

    output$longitudinal_plot <- renderPlot({
      df <- estimation_data()
      meta <- metadata()
      time_var <- meta$time_var
      treatment_var <- meta$treatment_var
      validate(need(time_var %in% names(df) && treatment_var %in% names(df), "Variables missing from estimates."))

      df$time_value <- df[[time_var]]
      df$treatment_value <- df[[treatment_var]]
      if (!is.numeric(df$time_value) && !inherits(df$time_value, "Date")) {
        df$time_value <- factor(df$time_value, levels = sort(unique(df$time_value)))
      }

      p <- ggplot(df, aes(x = time_value, y = estimate, color = treatment_value, group = treatment_value))
      if (isTRUE(input$show_lines)) {
        p <- p + geom_line(linewidth = 1)
      }
      if (isTRUE(input$show_points)) {
        p <- p + geom_point(size = 3)
      }
      if (isTRUE(input$show_interval) && all(c("lower", "upper") %in% names(df))) {
        width <- if (is.numeric(df$time_value)) 0 else 0.2
        p <- p + geom_errorbar(aes(ymin = lower, ymax = upper), width = width, alpha = 0.8)
      }
      p <- p + labs(
        x = meta$time_var,
        y = if (!is.null(meta$family) && meta$family == "binomial") "Estimated probability" else "Estimated mean",
        color = meta$treatment_var,
        title = "Estimated response over time"
      ) + theme_minimal(base_size = 14)

      levels <- strata_levels()
      if (!is.null(levels) && length(levels) > 1) {
        layout <- input$facet_layout %||% "wrap"
        df$stratum <- factor(df$stratum, levels = levels)
        if (layout == "rows") {
          p <- p + facet_grid(stratum ~ .)
        } else if (layout == "cols") {
          p <- p + facet_grid(. ~ stratum)
        } else if (layout == "wrap") {
          p <- p + facet_wrap(~stratum)
        }
      }

      p
    })

    output$notes <- renderUI({
      meta <- metadata()
      tagList(
        p(
          class = "text-muted",
          sprintf(
            "Model: %s | Method: %s",
            meta$formula %||% "unknown",
            if (meta$method == "gee") {
              sprintf("GEE (%s, %s correlation)", meta$family %||% "gaussian", meta$corstr %||% "exchangeable")
            } else {
              "LMM"
            }
          )
        )
      )
    })
  })
}
