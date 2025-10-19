# ===============================================================
# 🧪 Animal Trial Analyzer — Upload Module (long + wide support)
# ===============================================================

source("R/module_upload_helpers.R")

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload Data"),
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
      h4("Data Preview"),
      verbatimTextOutput(ns("validation_msg")),
      DTOutput(ns("preview"))
    )
  )
}

upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactiveVal(NULL)
    
    # ---- Example layouts ----
    # ---- Example layouts ----
    output$layout_example <- renderUI({
      req(input$layout_type)
      
      long_path <- "data/toy_animal_trial_data_long.xlsx"
      wide_path <- "data/toy_animal_trial_data_wide.xlsx"
      
      if (!file.exists(long_path) || !file.exists(wide_path))
        return(p("❌ Example files not found in /data folder."))
      
      if (input$layout_type == "long") {
        toy <- readxl::read_excel(long_path, n_max = 5)
        caption_txt <- paste(
          "Long format — one row per measurement.",
          "Each measurement is a separate row; responses (FAMACHA, BCS, EPG) each have their own column."
        )
      } else {
        toy <- readxl::read_excel(wide_path, n_max = 5)
        
        # Replace default "...1", "...2", etc. with empty column names
        bad_names <- grepl("^\\.\\.\\.[0-9]+$", names(toy))
        names(toy)[bad_names] <- "\t"
        
        caption_txt <- paste(
          "Wide format — two header rows.",
          "Top row: responses (e.g., FAMACHA, BCS, EPG).",
          "Bottom row: replicate numbers (1, 2, 3...)."
        )
      }
      
      DT::datatable(
        toy,
        caption = htmltools::tags$caption(htmltools::tags$b(caption_txt)),
        elementId = ns("example_dt"),
        options = list(dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    
    # ---- File upload + sheet selection ----
    observeEvent(input$file, {
      req(input$file)
      fpath <- input$file$datapath
      ext <- tolower(tools::file_ext(input$file$name))
      
      if (!ext %in% c("xlsx", "xls", "xlsm")) {
        output$validation_msg <- renderText("❌ Invalid file type. Please upload .xlsx/.xls/.xlsm.")
        return()
      }
      
      sheets <- tryCatch(readxl::excel_sheets(fpath), error = function(e) NULL)
      if (is.null(sheets) || length(sheets) == 0) {
        output$validation_msg <- renderText("❌ No readable sheets found in the workbook.")
        return()
      }
      
      output$validation_msg <- renderText(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI({
        selectInput(ns("sheet"), "Select sheet:", choices = sheets)
      })
    }, ignoreInit = TRUE)
    
    # ---- Load selected sheet ----
    # ---- Load selected sheet ----
    observeEvent(list(input$sheet, input$file$datapath, input$layout_type), {
      req(input$file, input$sheet)
      
      tmp <- tryCatch(
        readxl::read_excel(input$file$datapath, sheet = input$sheet),
        error = function(e) e
      )
      if (inherits(tmp, "error")) {
        output$validation_msg <- renderText(paste("❌ Error loading sheet —", conditionMessage(tmp)))
        return()
      }
      
      # ---- Detect likely wide-format pattern ----
      first_row <- suppressMessages(readxl::read_excel(input$file$datapath, sheet = input$sheet, n_max = 1))
      if (any(duplicated(unlist(first_row))) && input$layout_type == "long") {
        output$validation_msg <- renderText(
          "❌ Wide-format structure detected — please select 'Wide format' before proceeding."
        )
        output$preview <- renderDT(data.frame(), options = list(scrollX = TRUE))
        return()
      }
      
      # ---- Convert or validate ----
      if (input$layout_type == "wide") {
        tmp <- tryCatch(
          convert_wide_to_long(input$file$datapath, sheet = input$sheet, replicate_col = "Replicate"),
          error = function(e) e
        )
        if (inherits(tmp, "error")) {
          output$validation_msg <- renderText(paste("❌ Error converting wide format —", conditionMessage(tmp)))
          return()
        }
        output$validation_msg <- renderText("✅ Wide format detected and reshaped successfully.")
      } else {
        output$validation_msg <- renderText("✅ Long format detected and loaded successfully.")
      }
      
      tmp <- janitor::clean_names(tmp)
      df(tmp)
      
      output$preview <- renderDT(tmp, options = list(scrollX = TRUE, pageLength = 5))
    })
    
    
    
    return(df)
  })
}
