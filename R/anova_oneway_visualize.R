# ===============================================================
# 🧪 Visualization Module — One-way ANOVA (Apply-button version)
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize one-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      
      selectInput(
        ns("plot_type"),
        label = "Select visualization type",
        choices = c(
          "Lineplots (mean ± SE)" = "lineplot_mean_se",
          "Barplots (mean ± SE)"  = "barplot_mean_se"
        ),
        selected = "lineplot_mean_se"
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(6, checkboxInput(ns("lineplot_show_lines"),  "Connect means with lines", value = TRUE)),
          column(6, checkboxInput(ns("lineplot_show_jitter"), "Overlay jittered data",  value = FALSE))
        )
      ),
      
      checkboxInput(ns("share_y_axis"), "Use common y-axis across plots", value = FALSE),
      
      subplot_size_ui(ns),
      uiOutput(ns("layout_controls")),
      
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = FALSE)),
        column(6, base_size_ui(ns, default = 13))
      ),
      
      br(),
      
      fluidRow(
        column(6, actionButton(ns("apply_plot"), "Apply changes", width = "100%")),
        column(6, downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"))
      )
    ),
    
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL
    )
    
    df <- reactive(filtered_data())
    
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    )

    base_size <- base_size_server(input = input, default = 14)

    saved_inputs <- reactiveVal(list())

    track_input <- function(input_id, store = TRUE) {
      if (isTRUE(store)) {
        observeEvent(input[[input_id]], {
          current <- isolate(saved_inputs())
          current[[input_id]] <- input[[input_id]]
          saved_inputs(current)
        }, ignoreInit = TRUE)
      }
    }

    update_if_needed <- function(input_id, value, updater) {
      if (is.null(value)) return()
      current <- isolate(input[[input_id]])
      if (!identical(current, value)) {
        updater(session, input_id, value)
      }
    }

    restore_saved_inputs <- function() {
      saved <- isolate(saved_inputs())

      update_if_needed("plot_type", saved$plot_type, function(session, id, value) {
        updateSelectInput(session, id, selected = value)
      })

      update_if_needed("lineplot_show_lines", saved$lineplot_show_lines, function(session, id, value) {
        updateCheckboxInput(session, id, value = value)
      })

      update_if_needed("lineplot_show_jitter", saved$lineplot_show_jitter, function(session, id, value) {
        updateCheckboxInput(session, id, value = value)
      })

      update_if_needed("share_y_axis", saved$share_y_axis, function(session, id, value) {
        updateCheckboxInput(session, id, value = value)
      })

      update_if_needed("plot_width", saved$plot_width, function(session, id, value) {
        updateNumericInput(session, id, value = value)
      })

      update_if_needed("plot_height", saved$plot_height, function(session, id, value) {
        updateNumericInput(session, id, value = value)
      })

      update_if_needed("plot_base_size", saved$plot_base_size, function(session, id, value) {
        updateNumericInput(session, id, value = value)
      })
    }

    track_input("plot_type")
    track_input("lineplot_show_lines")
    track_input("lineplot_show_jitter")
    track_input("share_y_axis")
    track_input("plot_width")
    track_input("plot_height")
    track_input("plot_base_size")

    observeEvent(model_info(), {
      session$onFlushed(function() {
        restore_saved_inputs()
      }, once = TRUE)
    }, ignoreNULL = FALSE)

    strata_grid   <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    observeEvent(input$apply_plot, {
      data <- df()
      info <- model_info()
      
      stored$plot_width  <- input$plot_width
      stored$plot_height <- input$plot_height
      
      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        stored$warning <- "No data or ANOVA results available."
        stored$plot <- NULL
        return()
      }
      
      layout_inputs <- list(
        strata_rows = strata_grid$rows(),
        strata_cols = strata_grid$cols(),
        resp_rows   = response_grid$rows(),
        resp_cols   = response_grid$cols()
      )
      
      results <- list(
        lineplot_mean_se = plot_anova_lineplot_meanse(
          data, info, layout_inputs,
          line_colors  = custom_colors(),
          base_size    = base_size(),
          show_lines   = input$lineplot_show_lines,
          show_jitter  = input$lineplot_show_jitter,
          share_y_axis = input$share_y_axis
        ),
        barplot_mean_se = plot_anova_barplot_meanse(
          data, info,
          layout_values     = layout_inputs,
          line_colors       = custom_colors(),
          base_size         = base_size(),
          posthoc_all       = info$posthoc,
          share_y_axis      = input$share_y_axis
        )
      )
      
      chosen <- input$plot_type
      chosen_result <- results[[chosen]]

      stored$warning <- chosen_result$warning
      stored$plot    <- chosen_result$plot
      stored$layout  <- chosen_result$layout

      apply_grid_defaults_if_empty(
        input,
        session,
        "strata_grid",
        chosen_result$defaults$strata,
        n_items = chosen_result$panel_counts$strata
      )

      apply_grid_defaults_if_empty(
        input,
        session,
        "response_grid",
        chosen_result$defaults$responses,
        n_items = chosen_result$panel_counts$responses
      )
    })
    
    output$plot_warning <- renderUI({
      if (!is.null(stored$warning)) {
        div(class = "alert alert-warning", HTML(stored$warning))
      }
    })
    
    output$plot <- renderPlot({
      p <- stored$plot
      if (is.null(p)) return(NULL)
      print(p)
    },
    width = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_width * (lay$strata$cols %||% 1) * (lay$responses$cols %||% 1)
    },
    height = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_height * (lay$strata$rows %||% 1) * (lay$responses$rows %||% 1)
    },
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        
        lay <- stored$layout
        req(!is.null(lay))
        
        total_cols <- (lay$strata$cols %||% 1) * (lay$responses$cols %||% 1)
        total_rows <- (lay$strata$rows %||% 1) * (lay$responses$rows %||% 1)
        
        # Pixels → inches conversion
        w_in <- (stored$plot_width  * total_cols) / 96
        h_in <- (stored$plot_height * total_rows) / 96
        
        ggsave(
          filename = file,
          plot = p,
          dpi = 300,
          width = w_in,
          height = h_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}


extract_tukey_for_signif <- function(posthoc_entry) {
  if (is.null(posthoc_entry) || !is.data.frame(posthoc_entry)) return(NULL)
  
  df <- posthoc_entry
  
  # split contrast into group1 and group2
  parts <- strsplit(as.character(df$contrast), " - ")
  df$group1 <- vapply(parts, `[`, "", 1)
  df$group2 <- vapply(parts, `[`, "", 2)
  
  # clean p.value column
  df$p.value <- as.character(df$p.value)
  df$p.value <- gsub("\\*", "", df$p.value)          # remove any stars
  df$p.value <- gsub("<0\\.001", "0.0009", df$p.value)  # make "<0.001" numeric
  df$p.value <- suppressWarnings(as.numeric(df$p.value))
  
  df <- df %>%
    dplyr::filter(!is.na(p.value)) %>%
    dplyr::select(group1, group2, p.value)
  
  df
}



