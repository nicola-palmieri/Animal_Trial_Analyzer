# ===============================================================
# 🧾 Animal Trial Analyzer — Descriptive Statistics Module (with CV%)
# ===============================================================

library(skimr)
library(dplyr)

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
      
      if (input$group_var != "None") {
        group_var <- input$group_var
        skim_out <- data %>%
          group_by(.data[[group_var]]) %>%
          skim()
        
        cv_out <- data %>%
          group_by(.data[[group_var]]) %>%
          summarise(across(where(is.numeric),
                           ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
                           .names = "cv_{.col}"),
                    .groups = "drop")
      } else {
        skim_out <- skim(data)
        cv_out <- data %>%
          summarise(across(where(is.numeric),
                           ~ 100 * sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE),
                           .names = "cv_{.col}"))
      }
      
      list(skim = skim_out, cv = cv_out)
    })
    
    output$summary_text <- renderPrint({
      req(summary_data())
      print(summary_data()$skim)
      cat("\n── Coefficient of Variation (CV%) ──\n")
      print(summary_data()$cv)
    })
    
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        sink(file)
        print(summary_data()$skim)
        cat("\n── Coefficient of Variation (CV%) ──\n")
        print(summary_data()$cv)
        sink()
      }
    )
    
    return(summary_data)
  })
}
