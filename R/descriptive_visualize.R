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

    dimension_defaults <- list(width = 400L, height = 300L)
    dimension_state <- reactiveVal(list())

    sanitize_dimension <- function(value, default) {
      val <- suppressWarnings(as.numeric(value))
      if (length(val) == 0 || is.na(val) || val < 200) {
        as.integer(default)
      } else {
        as.integer(round(val))
      }
    }

    ensure_dimensions <- function(key) {
      if (is.null(key) || key == "") key <- "default"
      dims_map <- dimension_state()
      if (is.null(dims_map[[key]])) {
        dims_map[[key]] <- dimension_defaults
        dimension_state(dims_map)
        dims_map <- dimension_state()
      }
      dims_map[[key]]
    }

    get_dimensions <- function(key) {
      if (is.null(key) || key == "") key <- "default"
      ensure_dimensions(key)
      dimension_state()[[key]]
    }

    set_dimension <- function(key, name, value) {
      if (is.null(key) || key == "") key <- "default"
      dims <- ensure_dimensions(key)
      sanitized <- sanitize_dimension(value, dimension_defaults[[name]])
      if (!identical(dims[[name]], sanitized)) {
        dims[[name]] <- sanitized
        dims_map <- dimension_state()
        dims_map[[key]] <- dims
        dimension_state(dims_map)
      }
    }

    sync_dimension_inputs <- function(key) {
      dims <- get_dimensions(key)

      current_w <- suppressWarnings(as.numeric(input$plot_width))
      current_w_int <- if (length(current_w) == 0 || is.na(current_w)) NA_integer_ else as.integer(round(current_w))
      if (!identical(current_w_int, dims$width)) {
        updateNumericInput(session, "plot_width", value = dims$width)
      }

      current_h <- suppressWarnings(as.numeric(input$plot_height))
      current_h_int <- if (length(current_h) == 0 || is.na(current_h)) NA_integer_ else as.integer(round(current_h))
      if (!identical(current_h_int, dims$height)) {
        updateNumericInput(session, "plot_height", value = dims$height)
      }
    }

    observeEvent(input$plot_type, {
      type <- input$plot_type
      if (is.null(type) || length(type) == 0) return()
      key <- as.character(type[[1]])
      layout_state$set_active_key(key, sync_ui = TRUE)
      sync_dimension_inputs(key)
    }, ignoreNULL = FALSE)

    observeEvent(input$plot_width, {
      key <- layout_state$active_key()
      set_dimension(key, "width", input$plot_width)
    })

    observeEvent(input$plot_height, {
      key <- layout_state$active_key()
      set_dimension(key, "height", input$plot_height)
    })
    
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

      layout_for <- function(key) {
        layout_state$ensure_key(key)
        list(
          rows = layout_state$effective_input("resp_rows", key),
          cols = layout_state$effective_input("resp_cols", key)
        )
      }

      layout_overrides <- list(
        categorical = layout_for("categorical"),
        boxplots = layout_for("boxplots"),
        histograms = layout_for("histograms")
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

      if (!is.null(entry)) {
        entry$key <- plot_choice
      }
      entry
    })

    plot_entry <- reactive({
      entry <- build_current_plot()
      if (is.null(entry)) return(NULL)

      if (!is.list(entry) || is.null(entry$plot)) {
        return(list(
          plot = entry,
          panels = 1L,
          layout = list(rows = 1L, cols = 1L),
          key = layout_state$active_key()
        ))
      }

      if (is.null(entry$key)) {
        entry$key <- layout_state$active_key()
      }

      entry
    })

    plot_size <- reactive({
      entry <- plot_entry()

      if (is.null(entry)) {
        dims_default <- get_dimensions(layout_state$active_key())
        return(list(w = dims_default$width, h = dims_default$height))
      }

      key <- if (!is.null(entry$key)) entry$key else layout_state$active_key()
      dims <- get_dimensions(key)

      subplot_w <- dims$width
      subplot_h <- dims$height

      if (is.null(entry$layout)) {
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
              value = layout_state$ui_value("resp_rows", entry$key),
              min = 0,
              step = 1
            )
          ),
          column(
            width = 6,
            numericInput(
              session$ns("resp_cols"),
              "Grid columns",
              value = layout_state$ui_value("resp_cols", entry$key),
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
        n_responses = entry$panels,
        key = entry$key
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
