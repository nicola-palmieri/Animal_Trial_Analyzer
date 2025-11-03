# ===============================================================
# 🧪 Table Analyzer — Upload Module (simple editable column types)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload data"),
      p("Choose whether to load the example dataset or upload your own Excel file."),
      hr(),
      radioButtons(
        ns("data_source"),
        label = "Data source:",
        choices = c(
          "Example dataset" = "example",
          "Upload (long format)" = "long",
          "Upload (wide format)" = "wide"
        ),
        selected = "example"
      ),
      uiOutput(ns("layout_example")),
      hr(),
      fileInput(
        ns("file"),
        "Upload Excel file (.xlsx / .xls / .xlsm)",
        accept = c(".xlsx", ".xls", ".xlsm)")
      ),
      uiOutput(ns("sheet_selector")),
      hr(),
      uiOutput(ns("type_selectors"))
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
    editable_cols <- reactiveVal(NULL)
    
    # ---- Reset ----
    observeEvent(input$data_source, {
      df(NULL)
      output$preview <- renderDT(data.frame())
      output$validation_msg <- renderText("")
      output$sheet_selector <- renderUI(NULL)
      output$type_selectors <- renderUI(NULL)
      
      if (input$data_source == "example") {
        path <- "data/toy_animal_trial_data_long.xlsx"
        validate(need(file.exists(path), "⚠️ Example dataset not found in data folder."))
        data <- readxl::read_excel(path)
        data <- preprocess_uploaded_table(data)
        df(data)
        output$validation_msg <- renderText("📂 Loaded built-in example dataset (long format).")
        output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5))
        create_type_selectors(data)
      }
    }, ignoreInit = FALSE)
    
    
    # ---- Example layout ----
    output$layout_example <- renderUI({
      req(input$data_source %in% c("long", "wide"))
      long_path <- "data/toy_animal_trial_data_long.xlsx"
      wide_path <- "data/toy_animal_trial_data_wide.xlsx"
      validate(need(file.exists(long_path) && file.exists(wide_path),
                    "❌ Example layout files not found in /data folder."))
      
      if (input$data_source == "long") {
        toy <- readxl::read_excel(long_path, n_max = 5)
        caption <- "Long format — one row per measurement."
      } else {
        toy <- readxl::read_excel(wide_path, n_max = 5)
        bad <- grepl("^\\.\\.\\.[0-9]+$", names(toy))
        names(toy)[bad] <- ""
        caption <- "Wide format — two header rows (top: response, bottom: replicate)."
      }
      
      DT::datatable(
        toy,
        caption = htmltools::tags$caption(htmltools::tags$b(caption)),
        options = list(dom = "t", scrollX = TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    
    
    # ---- File upload ----
    observeEvent(input$file, {
      req(input$data_source != "example", input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "xls", "xlsm"),
                    "❌ Invalid file type. Please upload .xlsx/.xls/.xlsm."))
      
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath),
                         error = function(e) NULL)
      validate(need(!is.null(sheets), "❌ No readable sheets found in the workbook."))
      output$validation_msg <- renderText(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI(selectInput(ns("sheet"), "Select sheet:", choices = sheets))
    }, ignoreInit = TRUE)
    
    
    # ---- Load selected sheet ----
    observeEvent(list(input$sheet, input$file$datapath, input$data_source), {
      req(input$data_source != "example", input$file, input$sheet)
      data <- NULL
      
      if (input$data_source == "wide") {
        data <- tryCatch(
          convert_wide_to_long(input$file$datapath, sheet = input$sheet, replicate_col = "Replicate"),
          error = function(e) {
            output$validation_msg <- renderText(paste("❌ Error converting wide format:", conditionMessage(e)))
            NULL
          }
        )
        req(!is.null(data))
        output$validation_msg <- renderText("✅ Wide format reshaped successfully.")
      } else {
        data <- tryCatch(
          readxl::read_excel(input$file$datapath, sheet = input$sheet),
          error = function(e) {
            output$validation_msg <- renderText(paste("❌ Error loading sheet:", conditionMessage(e)))
            NULL
          }
        )
        req(!is.null(data))
        output$validation_msg <- renderText("✅ Long format loaded successfully.")
      }
      
      data <- preprocess_uploaded_table(data)
      df(data)
      output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5))
      create_type_selectors(data)
    })
    
    
    # ---- Create type selectors ----
    create_type_selectors <- function(data) {
      num_vars <- names(data)[sapply(data, is.numeric)]
      few_level_nums <- num_vars[sapply(data[num_vars], function(x) length(unique(na.omit(x))) <= 10)]
      editable_cols(few_level_nums)
      
      if (length(few_level_nums) == 0) {
        output$type_selectors <- renderUI(NULL)
        return()
      }
      
      output$type_selectors <- renderUI({
        tagList(
          h5("Ambiguous type columns"),
          lapply(few_level_nums, function(col) {
            selectInput(
              ns(paste0("type_", col)),
              label = col,
              choices = c("Numeric", "Categorical"),
              selected = "Numeric",
              width = "100%"
            )
          })
        )
      })
    }
    
    
    # ---- Update df when user changes type ----
    observe({
      req(df(), editable_cols())
      data <- df()
      for (col in editable_cols()) {
        input_id <- paste0("type_", col)
        sel <- input[[input_id]]
        if (!is.null(sel) && sel == "Categorical") {
          data[[col]] <- factor(as.character(data[[col]]))
        } else if (!is.null(sel) && sel == "Numeric") {
          data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
        }
      }
      df(data)
    })
    
    
    return(df)
  })
}
