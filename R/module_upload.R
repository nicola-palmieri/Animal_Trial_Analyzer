# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long + wide support)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload data"),
      p("Upload your Excel file, choose the worksheet to analyze, and ensure the data follow either the long or wide format shown below."),
      hr(),
      radioButtons(
        ns("layout_type"),
        label = "Data layout:",
        choices = c(
          "Long format (one row per measurement)" = "long",
          "Wide format (replicates in columns)" = "wide"
        ),
        selected = "long"
      ),
      uiOutput(ns("layout_example")),
      hr(),
      fileInput(
        ns("file"),
        "Upload Excel file (.xlsx / .xls / .xlsm)",
        accept = c(".xlsx", ".xls", ".xlsm")
      ),
      uiOutput(ns("sheet_selector"))
    ),
    mainPanel(
      width = 8,
      h4("Data preview"),
      verbatimTextOutput(ns("validation_msg")),
      DTOutput(ns("preview"))
    )
  )
}

upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactiveVal(NULL)
    
    # ---- Load default example on startup ----
    observe({
      path <- "data/toy_animal_trial_data_long.xlsx"
      if (!file.exists(path)) {
        output$validation_msg <- renderText("⚠️ Default example file not found in data folder.")
        return()
      }
      
      data <- readxl::read_excel(path)
      data <- preprocess_uploaded_table(data)
      df(data)
      
      output$validation_msg <- renderText(paste("📂 Loaded default long-format dataset from:", path))
      output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5, autoWidth = TRUE),  class = "compact stripe nowrap")
    })
    
    # ---- Example layout preview ----
    output$layout_example <- renderUI({
      req(input$layout_type)
      
      long_path <- "data/toy_animal_trial_data_long.xlsx"
      wide_path <- "data/toy_animal_trial_data_wide.xlsx"
      if (!file.exists(long_path) || !file.exists(wide_path))
        return(p("❌ Example files not found in /data folder."))
      
      if (input$layout_type == "long") {
        toy <- readxl::read_excel(long_path, n_max = 5)
        caption <- "Long format — one row per measurement."
      } else {
        toy <- readxl::read_excel(wide_path, n_max = 5)
        bad <- grepl("^\\.\\.\\.[0-9]+$", names(toy))
        names(toy)[bad] <- "\t"
        caption <- "Wide format — two header rows (top: response, bottom: replicate)."
      }
      
      DT::datatable(
        toy,
        caption = htmltools::tags$caption(htmltools::tags$b(caption)),
        elementId = ns("example_dt"),
        options = list(dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    # ---- File upload and sheet list ----
    observeEvent(input$file, {
      req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      if (!ext %in% c("xlsx", "xls", "xlsm")) {
        output$validation_msg <- renderText("❌ Invalid file type. Please upload .xlsx/.xls/.xlsm.")
        return()
      }
      
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
      if (is.null(sheets)) {
        output$validation_msg <- renderText("❌ No readable sheets found in the workbook.")
        return()
      }
      
      output$validation_msg <- renderText(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI({
        selectInput(ns("sheet"), "Select sheet:", choices = sheets)
      })
    }, ignoreInit = TRUE)
    
    # ---- Load selected sheet ----
    observeEvent(list(input$sheet, input$file$datapath, input$layout_type), {
      req(input$file, input$sheet)
      
      data <- tryCatch(
        readxl::read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) e
      )
      if (inherits(data, "error")) {
        output$validation_msg <- renderText(paste("❌ Error loading sheet —", conditionMessage(data)))
        return()
      }
      
      col_names <- names(readxl::read_excel(input$file$datapath, sheet = input$sheet, n_max = 1))
      if (anyDuplicated(col_names) > 0 && input$layout_type == "long") {
        output$validation_msg <- renderText("❌ Duplicate column names found — this looks like wide format.")
        output$preview <- renderDT(data.frame(), options = list(scrollX = TRUE))
        return()
      }
      
      
      if (input$layout_type == "wide") {
        data <- tryCatch(
          convert_wide_to_long(input$file$datapath, sheet = input$sheet, replicate_col = "Replicate"),
          error = function(e) e
        )
        if (inherits(data, "error")) {
          output$validation_msg <- renderText(paste("❌ Error converting wide format —", conditionMessage(data)))
          return()
        }
        output$validation_msg <- renderText("✅ Wide format detected and reshaped successfully.")
      } else {
        output$validation_msg <- renderText("✅ Long format detected and loaded successfully.")
      }
      
      data <- preprocess_uploaded_table(data)
      df(data)
      output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    return(df)
  })
}

