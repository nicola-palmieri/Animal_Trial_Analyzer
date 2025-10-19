# ===============================================================
# 🧪 Animal Trial Analyzer — Filter Module (cleaned, identical behavior)
# ===============================================================

filter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 2 — Filter Records"),
      p("Pick the columns to focus on and adjust the filters to refine the dataset for analysis."),
      hr(),
      uiOutput(ns("column_selector")),
      hr(),
      uiOutput(ns("filter_widgets"))
    ),
    mainPanel(
      width = 8,
      h4("Filtered Data Preview"),
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
      selectInput(
        ns("columns"),
        "Select columns to filter:",
        choices = names(df()),
        multiple = TRUE
      )
    })
    
    # --- 2. Dynamic filter widgets ---
    output$filter_widgets <- renderUI({
      req(df())
      cols <- input$columns
      if (is.null(cols) || length(cols) == 0) return(NULL)
      
      make_numeric_widget <- function(col, x) {
        rng <- suppressWarnings(range(x, na.rm = TRUE))
        if (any(!is.finite(rng))) rng <- c(0, 0)
        step_val <- ifelse(diff(rng) == 0, 1, diff(rng) / 100)
        fluidRow(
          column(
            6,
            numericInput(
              ns(paste0("min_", col)),
              label = paste(col, "(min)"),
              value = rng[1],
              min = rng[1],
              max = rng[2],
              step = step_val
            )
          ),
          column(
            6,
            numericInput(
              ns(paste0("max_", col)),
              label = paste(col, "(max)"),
              value = rng[2],
              min = rng[1],
              max = rng[2],
              step = step_val
            )
          )
        )
      }
      
      make_logical_widget <- function(col) {
        checkboxGroupInput(
          ns(paste0("filter_", col)),
          label = col,
          choices = c(TRUE, FALSE),
          selected = c(TRUE, FALSE),
          inline = TRUE
        )
      }
      
      make_factor_widget <- function(col, x) {
        choices <- sort(unique(as.character(x)))
        selectInput(
          ns(paste0("filter_", col)),
          label = col,
          choices = choices,
          multiple = TRUE,
          selected = choices
        )
      }
      
      widgets <- lapply(cols, function(col) {
        col_data <- df()[[col]]
        if (is.numeric(col_data)) make_numeric_widget(col, col_data)
        else if (is.logical(col_data)) make_logical_widget(col)
        else make_factor_widget(col, col_data)
      })
      
      tagList(widgets)
    })
    
    # --- 3. Reactive filtering ---
    filtered_df <- reactive({
      req(df())
      data <- df()
      cols <- input$columns
      if (is.null(cols) || length(cols) == 0) return(data)
      
      for (col in cols) {
        col_data <- data[[col]]
        
        if (is.numeric(col_data)) {
          min_val <- input[[paste0("min_", col)]]
          max_val <- input[[paste0("max_", col)]]
          if (is.null(min_val) || is.null(max_val)) {
            data <- data[0, , drop = FALSE]
            break
          }
          data <- data[data[[col]] >= min_val & data[[col]] <= max_val, , drop = FALSE]
        } else {
          sel <- input[[paste0("filter_", col)]]
          if (is.null(sel) || length(sel) == 0) {
            data <- data[0, , drop = FALSE]
            break
          }
          data <- data[data[[col]] %in% sel, , drop = FALSE]
        }
      }
      
      data
    })
    
    # --- 4. Preview table ---
    output$filtered_preview <- renderDT({
      datatable(
        filtered_df(),
        options = list(scrollX = TRUE, pageLength = 5)
      )
    })
    
    # --- 5. Return filtered data for downstream modules ---
    return(filtered_df)
  })
}
