# ===============================================================
# Visualization Module — Descriptive Statistics
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 5 — Visualize Descriptive Statistics"),
      p("Explore distributions, variability, and normality across variables."),
      hr(),
      selectInput(
        ns("plot_type"),
        label = "Visualization type:",
        choices = c(
          "Categorical Distributions" = "categorical",
          "Numeric Boxplots" = "boxplots",
          "Numeric Histograms" = "histograms",
          "CV (%)" = "cv",
          "Outlier Counts" = "outliers",
          "Missingness (%)" = "missing",
          "Shapiro–Wilk p-values" = "shapiro"
        ),
        selected = "boxplots"
      ),
      hr(),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(
          6,
          numericInput(
            ns("plot_width"),
            label = "Subplot width (px)",
            value = 400,
            min = 200,
            max = 2000,
            step = 50
          )
        ),
        column(
          6,
          numericInput(
            ns("plot_height"),
            label = "Subplot height (px)",
            value = 300,
            min = 200,
            max = 2000,
            step = 50
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      h4("Descriptive Visualization"),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    
    # ------------------------------------------------------------
    # 1️⃣ Base reactive: Data + summary check
    # ------------------------------------------------------------
    df <- reactive({
      data <- filtered_data()
      validate(need(!is.null(data) && is.data.frame(data), "No data available."))
      data
    })
    
    summary_info <- reactive({
      info <- descriptive_summary()
      validate(need(!is.null(info), "Run descriptive summary first."))
      validate(need(!is.null(info$summary), "Summary not available."))
      info
    })
    
    layout_state <- initialize_layout_state(input, session)
    
    # ------------------------------------------------------------
    # 2️⃣ Build all plots using the shared helper
    # ------------------------------------------------------------
    plots_all <- reactive({
      info <- summary_info()

      summary_data <- info$summary
      if (is.reactive(summary_data)) summary_data <- summary_data()

      selected_vars <- info$selected_vars
      if (is.reactive(selected_vars)) selected_vars <- selected_vars()

      data_subset <- df()
      if (!is.null(selected_vars)) {
        data_subset <- data_subset[, intersect(names(data_subset), selected_vars), drop = FALSE]
      }

      layout_overrides <- list(
        rows = layout_state$effective_input("resp_rows"),
        cols = layout_state$effective_input("resp_cols")
      )

      build_descriptive_plots(summary_data, data_subset, layout_overrides)
    })

    # ------------------------------------------------------------
    # 3️⃣ Pick correct plot based on selection
    # ------------------------------------------------------------
    build_current_plot <- reactive({
      plots <- plots_all()
      validate(need(!is.null(plots), "No plots available."))

      req(!is.null(input$plot_type))

      plot_choice <- as.character(input$plot_type)
      validate(need(length(plot_choice) == 1, "Select a single plot type."))
      plot_choice <- plot_choice[[1]]

      metrics <- plots$metrics
      entry <- switch(
        plot_choice,
        categorical = plots$factors,
        boxplots = plots$boxplots,
        histograms = plots$histograms,
        cv = if (!is.null(metrics)) metrics$cv else NULL,
        outliers = if (!is.null(metrics)) metrics$outliers else NULL,
        missing = if (!is.null(metrics)) metrics$missing else NULL,
        shapiro = if (!is.null(metrics)) metrics$shapiro else NULL,
        NULL
      )

      entry
    })

    plot_entry <- reactive({
      entry <- build_current_plot()
      if (is.null(entry)) return(NULL)

      if (!is.list(entry) || is.null(entry$plot)) {
        return(list(
          plot = entry,
          panels = 1L,
          layout = list(rows = 1L, cols = 1L)
        ))
      }

      entry
    })

    plot_size <- reactive({
      entry <- plot_entry()

      w <- suppressWarnings(as.numeric(input$plot_width))
      h <- suppressWarnings(as.numeric(input$plot_height))

      subplot_w <- ifelse(is.na(w) || w <= 0, 400, w)
      subplot_h <- ifelse(is.na(h) || h <= 0, 300, h)

      if (is.null(entry) || is.null(entry$layout)) {
        return(list(w = subplot_w, h = subplot_h))
      }

      list(
        w = subplot_w * max(1, entry$layout$cols),
        h = subplot_h * max(1, entry$layout$rows)
      )
    })

    output$layout_controls <- renderUI({
      entry <- plot_entry()
      if (is.null(entry) || is.null(entry$panels) || entry$panels <= 1) {
        return(NULL)
      }

      tagList(
        h4("Layout Controls"),
        fluidRow(
          column(
            width = 6,
            numericInput(
              session$ns("resp_rows"),
              "Grid rows",
              value = isolate(layout_state$default_ui_value(input$resp_rows)),
              min = 0,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              session$ns("resp_cols"),
              "Grid columns",
              value = isolate(layout_state$default_ui_value(input$resp_cols)),
              min = 0,
              step = 1
            )
          )
        )
      )
    })

    layout_info <- reactive({
      entry <- plot_entry()
      if (is.null(entry)) return(NULL)

      list(
        has_strata = FALSE,
        layout = list(
          strata = list(rows = 1L, cols = 1L),
          responses = list(nrow = entry$layout$rows, ncol = entry$layout$cols)
        ),
        n_responses = entry$panels
      )
    })

    observe_layout_synchronization(input, layout_info, layout_state, session)

    # ------------------------------------------------------------
    # 4️⃣ Render plot safely
    # ------------------------------------------------------------
    output$plot <- renderPlot({
      entry <- plot_entry()
      validate(need(!is.null(entry), "Selected plot not available."))
      # patchwork objects are fine with print()
      print(entry$plot)
    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)
    
    # ------------------------------------------------------------
    # 5️⃣ Download
    # ------------------------------------------------------------
    output$download_plot <- downloadHandler(
      filename = function() paste0("descriptive_plot_", Sys.Date(), ".png"),
      content = function(file) {
        entry <- plot_entry()
        validate(need(!is.null(entry), "No plot available to download."))

        size <- plot_size()
        ggsave(
          filename = file,
          plot = entry$plot,
          device = "png",
          dpi = 300,
          width = size$w / 96,
          height = size$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
