# ===============================================================
# 🧪 Table Analyzer — Upload Module (Stable wide-format version)
# ===============================================================

upload_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 1 — Upload data"),
      p("Choose whether to load the example dataset or upload your own Excel file."),
      hr(),
      with_help_tooltip(
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
        "Help: Decide whether to explore the built-in example data or load your own table."
      ),
      uiOutput(ns("layout_example")),
      hr(),
      with_help_tooltip(
        fileInput(
          ns("file"),
          "Upload Excel file (.xlsx / .xls / .xlsm)",
          accept = c(".xlsx", ".xls", ".xlsm")
        ),
        "Help: Provide the Excel workbook that stores your study measurements."
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
    
    # -----------------------------------------------------------
    # 1️⃣ Handle source selection
    # -----------------------------------------------------------
    observeEvent(input$data_source, {
      df(NULL)
      output$type_selectors <- renderUI(NULL)
      output$sheet_selector <- renderUI(NULL)
      output$preview <- renderDT(data.frame())
      
      if (input$data_source == "example") {
        path <- "data/toy_animal_trial_data_long.xlsx"
        validate(need(file.exists(path), "⚠️ Example dataset not found in data folder."))
        data <- readxl::read_excel(path)
        processed <- safe_preprocess_uploaded_table(data)
        if (!is.null(processed$error)) {
          output$validation_msg <- renderText(
            format_safe_error_message("Error preparing example dataset", processed$error)
          )
          return()
        }
        data <- processed$result
        df(data)
        output$validation_msg <- renderText("📂 Loaded built-in example dataset (long format).")
        output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5))
        create_type_selectors(data)
      } else {
        output$validation_msg <- renderText("Please upload an Excel file.")
      }
    }, ignoreInit = FALSE)
    
    # -----------------------------------------------------------
    # 2️⃣ Example layout preview
    # -----------------------------------------------------------
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
    
    # -----------------------------------------------------------
    # 3️⃣ File upload → detect sheets
    # -----------------------------------------------------------
    observeEvent(input$file, {
      req(input$data_source != "example")
      ext <- tolower(tools::file_ext(input$file$name))
      validate(need(ext %in% c("xlsx", "xls", "xlsm"),
                    "❌ Invalid file type. Please upload .xlsx/.xls/.xlsm."))
      
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath),
                         error = function(e) NULL)
      validate(need(!is.null(sheets), "❌ No readable sheets found in workbook."))
      
      output$validation_msg <- renderText(paste("✅ File loaded:", input$file$name))
      output$sheet_selector <- renderUI(
        with_help_tooltip(
          selectInput(ns("sheet"), "Select sheet:", choices = sheets),
          "Help: Pick the worksheet inside your Excel file that contains the data."
        )
      )
    }, ignoreInit = TRUE)
    
    # -----------------------------------------------------------
    # 4️⃣ Load selected sheet (handles both long & wide)
    # -----------------------------------------------------------
    observeEvent(list(input$sheet, input$data_source), {
      req(input$file, input$sheet, input$data_source != "example")
      path <- input$file$datapath
      data <- NULL
      
      success_message <- NULL

      if (input$data_source == "wide") {
        # ⚙️ Wide format conversion with error handling
        safe_result <- safe_convert_wide_to_long(
          path,
          sheet = input$sheet,
          replicate_col = "Replicate"
        )

        if (!is.null(safe_result$error)) {
          output$validation_msg <- renderText(
            format_safe_error_message("Error converting wide format", safe_result$error)
          )
          return()
        }

        data <- safe_result$result
        success_message <- "✅ Wide format reshaped successfully."
      } else {
        # 🧾 Simple long format load
        data <- tryCatch(
          readxl::read_excel(path, sheet = input$sheet),
          error = function(e) {
            output$validation_msg <- renderText(
              paste("❌ Error loading sheet:", conditionMessage(e))
            )
            NULL
          }
        )
        if (is.null(data)) return()
        success_message <- "✅ Long format loaded successfully."
      }

      # ✅ Shared postprocessing and preview
      processed <- safe_preprocess_uploaded_table(data)
      if (!is.null(processed$error)) {
        output$validation_msg <- renderText(
          format_safe_error_message("Error preparing data", processed$error)
        )
        return()
      }

      data <- processed$result
      df(data)
      output$preview <- renderDT(data, options = list(scrollX = TRUE, pageLength = 5))
      create_type_selectors(data)
      if (!is.null(success_message)) {
        output$validation_msg <- renderText(success_message)
      }
    })
    
    # -----------------------------------------------------------
    # 5️⃣ Create type selectors
    # -----------------------------------------------------------
    create_type_selectors <- function(data) {
      req(data)
      num_vars <- names(data)[sapply(data, is.numeric)]
      few_level_nums <- num_vars[sapply(data[num_vars], function(x)
        length(unique(na.omit(x))) <= 10)]
      editable_cols(few_level_nums)
      
      if (length(few_level_nums) == 0) {
        output$type_selectors <- renderUI(NULL)
        return()
      }
      
      output$type_selectors <- renderUI({
        tagList(
          h5("Ambiguous type columns"),
          lapply(few_level_nums, function(col) {
            with_help_tooltip(
              selectInput(
                ns(paste0("type_", col)),
                label = col,
                choices = c("Numeric", "Categorical"),
                selected = "Numeric",
                width = "100%"
              ),
              "Help: Tell the app whether this column should be treated as numbers or as groups."
            )
          })
        )
      })
    }
    
    # -----------------------------------------------------------
    # 6️⃣ Apply user type edits reactively
    # -----------------------------------------------------------
    observe({
      req(df(), editable_cols())
      data <- df()
      for (col in editable_cols()) {
        sel <- input[[paste0("type_", col)]] %||% "Numeric"
        if (sel == "Categorical") {
          data[[col]] <- factor(as.character(data[[col]]))
        } else {
          data[[col]] <- suppressWarnings(as.numeric(as.character(data[[col]])))
        }
      }
      df(data)
    })
    
    # -----------------------------------------------------------
    # ✅ Return reactive data
    # -----------------------------------------------------------
    df
  })
}
