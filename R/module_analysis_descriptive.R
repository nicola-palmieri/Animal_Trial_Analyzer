# ===============================================================
# 🧾 Animal Trial Analyzer — Descriptive Statistics Module 
#      (skim + CV% + Outliers + Shapiro)
# ===============================================================

source("R/module_analysis_descriptive_helpers.R")

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      selectInput(ns("group_var"), "Stratify by:", choices = NULL),
      br(),
      fluidRow(
        column(6, actionButton(ns("run"), "Run", width = "100%")),
        column(6, downloadButton(ns("download_summary"), "Download Summary", width = "100%"))
      ),
      hr()
    ),
    results = tagList(
      verbatimTextOutput(ns("summary_text"))
    )
  )
}

descriptive_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      req(filtered_data())
      filtered_data()
    })
    
    observe({
      data <- df()
      vars <- names(data)
      updateSelectInput(session, "group_var", choices = c("None", vars), selected = "None")
    })
    
    summary_data <- eventReactive(input$run, {
      data <- df()
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      
      # ------------------------------------------------------------
      # 1️⃣ Skim summary
      # ------------------------------------------------------------
      if (input$group_var != "None") {
        group_var <- input$group_var
        
        skim_out <- data %>%
          group_by(.data[[group_var]]) %>%
          skim()
        
        # CV%
        cv_out <- data %>%
          group_by(.data[[group_var]]) %>%
          summarise(across(
            where(is.numeric),
            ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
            .names = "cv_{.col}"
          ), .groups = "drop")
        
        # Outliers
        outlier_out <- data %>%
          group_by(.data[[group_var]]) %>%
          summarise(across(
            all_of(numeric_vars),
            ~ {
              q <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
              iqr <- q[2] - q[1]
              sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
            },
            .names = "outliers_{.col}"
          ), .groups = "drop")
        
        # Missingness summary
        missing_out <- data %>%
          group_by(.data[[group_var]]) %>%
          summarise(across(
            all_of(numeric_vars),
            ~ 100 * mean(is.na(.x)),
            .names = "missing_{.col}"
          ), .groups = "drop")
        
        # Shapiro
        shapiro_out <- data %>%
          group_by(.data[[group_var]]) %>%
          summarise(across(
            all_of(numeric_vars),
            ~ tryCatch(shapiro.test(.x)$p.value, error = function(e) NA_real_),
            .names = "shapiro_{.col}"
          ), .groups = "drop")
        
      } else {
        skim_out <- skim(data)
        
        cv_out <- data %>%
          summarise(across(
            where(is.numeric),
            ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
            .names = "cv_{.col}"
          ))
        
        outlier_out <- data %>%
          summarise(across(
            all_of(numeric_vars),
            ~ {
              q <- quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
              iqr <- q[2] - q[1]
              sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
            },
            .names = "outliers_{.col}"
          ))
        
        shapiro_out <- data %>%
          summarise(across(
            all_of(numeric_vars),
            ~ tryCatch(shapiro.test(.x)$p.value, error = function(e) NA_real_),
            .names = "shapiro_{.col}"
          ))
      }
      
      list(
        skim = skim_out,
        cv = cv_out,
        outliers = outlier_out,
        shapiro = shapiro_out
      )
    })
    
    # ------------------------------------------------------------
    # 2️⃣ Printed Output
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      print_summary_sections(summary_data())
    })
    
    # ------------------------------------------------------------
    # 3️⃣ Download Handler
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        sink(file)
        print_summary_sections(summary_data())
        sink()
      }
    )
    
    return(summary_data)
  })
}
