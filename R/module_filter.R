# ===============================================================
# 🧪 Table Analyzer — Filter Module (Refactored Reactive Version)
# ===============================================================

filter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 2 — Filter records"),
      p("Select the columns to focus on and adjust the filters to refine the dataset for analysis."),
      hr(),
      uiOutput(ns("column_selector")),
      uiOutput(ns("filter_widgets"))
    ),
    mainPanel(
      width = 8,
      h4("Filtered data preview"),
      DTOutput(ns("filtered_preview"))
    )
  )
}

filter_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(uploaded_data())
    
    # --- 1. Column selector ---
    output$column_selector <- renderUI({
      req(df())
      with_help_tooltip(
        selectInput(
          ns("columns"),
          "Select columns to filter:",
          choices = names(df()),
          multiple = TRUE
        ),
        "Help: Choose which variables you want to filter before running analyses."
      )
    })
    
    # --- 2. Dynamic filter widgets ---
    output$filter_widgets <- renderUI({
      req(df())
      cols <- input$columns
      req(cols)
      
      make_numeric_widget <- function(col, x) {
        rng <- suppressWarnings(range(x, na.rm = TRUE))
        if (any(!is.finite(rng))) rng <- c(0, 0)
        step_val <- ifelse(diff(rng) == 0 || any(!is.finite(diff(rng))), 1, diff(rng) / 100)
        fluidRow(
          column(
            6,
            with_help_tooltip(
              numericInput(
                ns(paste0("min_", col)), paste(col, "(min)"),
                value = rng[1], min = rng[1], max = rng[2], step = step_val
              ),
              sprintf("Help: Enter the smallest value to keep for %s.", col)
            )
          ),
          column(
            6,
            with_help_tooltip(
              numericInput(
                ns(paste0("max_", col)), paste(col, "(max)"),
                value = rng[2], min = rng[1], max = rng[2], step = step_val
              ),
              sprintf("Help: Enter the largest value to keep for %s.", col)
            )
          )
        )
      }
      
      make_logical_widget <- function(col) {
        with_help_tooltip(
          checkboxGroupInput(
            ns(paste0("filter_", col)), label = col,
            choices = c(TRUE, FALSE), selected = c(TRUE, FALSE), inline = TRUE
          ),
          sprintf("Help: Tick the logical values you want to keep for %s.", col)
        )
      }
      
      make_factor_widget <- function(col, x) {
        choices <- sort(unique(as.character(x)))
        with_help_tooltip(
          selectInput(
            ns(paste0("filter_", col)), label = col,
            choices = choices, multiple = TRUE, selected = choices
          ),
          sprintf("Help: Choose which categories should remain for %s.", col)
        )
      }
      
      tagList(lapply(cols, function(col) {
        x <- df()[[col]]
        if (is.numeric(x)) make_numeric_widget(col, x)
        else if (is.logical(x)) make_logical_widget(col)
        else make_factor_widget(col, x)
      }))
    })
    
    # --- 3. Reactive filtering ---
    filtered_df <- reactive({
      req(df())
      data <- df()
      cols <- input$columns
      
      if (is.null(cols) || !length(cols)) return(data)
      
      for (col in cols) {
        x <- data[[col]]

        # Numeric columns
        if (is.numeric(x)) {
          min_val <- input[[paste0("min_", col)]] %||% -Inf
          max_val <- input[[paste0("max_", col)]] %||% Inf
          if (all(is.na(x))) {
            next
          }
          keep <- is.na(x) | (x >= min_val & x <= max_val)
          data <- data[keep, , drop = FALSE]
        }
        # Logical / Factor / Character
        else {
          sel <- input[[paste0("filter_", col)]] %||% character(0)
          if (!length(sel)) {
            data <- data[0, , drop = FALSE]
            break
          }
          keep <- is.na(x) | (as.character(x) %in% sel)
          data <- data[keep, , drop = FALSE]
        }
      }
      
      data
    })
    
    # --- 4. Preview table ---
    output$filtered_preview <- renderDT({
      datatable(filtered_df(), options = list(scrollX = TRUE, pageLength = 5))
    })
    
    # --- 5. Return filtered data downstream ---
    filtered_df
  })
}
