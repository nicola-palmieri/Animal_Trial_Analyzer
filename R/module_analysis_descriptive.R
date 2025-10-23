# ===============================================================
# 🧾 Animal Trial Analyzer — Descriptive Statistics Module (refactored)
# ===============================================================

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
    df <- use_filtered_df(filtered_data)
    
    # ------------------------------------------------------------
    # 1️⃣ Update grouping variable selector
    # ------------------------------------------------------------
    observe({
      data <- df()
      vars <- names(data)
      updateSelectInput(session, "group_var",
                        choices = c("None", vars),
                        selected = "None"
      )
    })
    
    # ------------------------------------------------------------
    # 2️⃣ Main summary computation (delegated to helpers)
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      data <- df()
      group_var <- if (input$group_var == "None") NULL else input$group_var
      compute_descriptive_summary(data, group_var)
    })
    
    # ------------------------------------------------------------
    # 3️⃣ Printed Output
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      print_summary_sections(summary_data())
    })
    
    # ------------------------------------------------------------
    # 4️⃣ Download Handler
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        sink(file)
        print_summary_sections(summary_data())
        sink()
      }
    )
    
    return(reactive({
      list(
        type = "descriptive",
        data = df(),
        summary = summary_data
      )
    }))
    
  })
}
