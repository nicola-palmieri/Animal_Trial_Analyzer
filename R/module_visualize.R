# ===============================================================
# 🧪 Animal Trial Analyzer — Visualization Module
# ===============================================================

source("R/module_visualize_helpers.R")
source("R/module_visualize_layout.R")
source("R/module_visualize_plot_builders.R")

visualize_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize Outcomes"),
      p("Tweak the layout of the mean ± SE plots, download the figure, and wrap up your workflow."),
      hr(),
      uiOutput(ns("layout_controls")),
      hr(),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("plot_width"),
            label = "Plot width (px)",
            value = 300,
            min = 200,
            max = 1200,
            step = 50
          )
        ),
        column(
          width = 6,
          numericInput(
            ns("plot_height"),
            label = "Plot height (px)",
            value = 200,
            min = 200,
            max = 1200,
            step = 50
          )
        )
      ),
      hr(),
      downloadButton(ns("download_plot"), "Download Plot")
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      plotOutput(ns("plots"))
    )
  )
}

visualize_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- use_filtered_df(filtered_data)

    model_info <- reactive({
      model_fit()
    })

    active_analysis_type <- reactiveVal(NULL)

    observe({
      info <- model_info()
      if (is.null(info)) return()

      current_type <- if (!is.null(info$type)) info$type else "anova"
      if (!identical(active_analysis_type(), current_type)) {
        active_analysis_type(current_type)

        defaults <- if (identical(current_type, "ggpairs") || identical(current_type, "pca")) {
          list(width = 800, height = 600)
        } else {
          list(width = 300, height = 200)
        }

        updateNumericInput(session, "plot_width", value = defaults$width)
        updateNumericInput(session, "plot_height", value = defaults$height)
      }
    })

    observe({
      type <- active_analysis_type()
      if (is.null(type)) return()

      if (identical(type, "ggpairs") || identical(type, "pca")) {
        updateNumericInput(session, "plot_width", label = "Plot width (px)")
        updateNumericInput(session, "plot_height", label = "Plot height (px)")
      } else {
        updateNumericInput(session, "plot_width", label = "Subplot width (px)")
        updateNumericInput(session, "plot_height", label = "Subplot height (px)")
      }
    })

    layout_state <- initialize_layout_state(input, session)

    output$layout_controls <- renderUI({
      info <- model_info()
      if (is.null(info)) return(NULL)

      current_type <- if (!is.null(info$type)) info$type else "anova"
      data_for_controls <- NULL
      if (identical(current_type, "pca")) {
        data_for_controls <- df()
      }

      build_layout_controls_for_type(
        ns = ns,
        input = input,
        info = info,
        default_ui_value = layout_state$default_ui_value,
        data_for_pca = data_for_controls
      )
    })

    plot_obj_info <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$models) || length(info$models) == 0) {
        return(NULL)
      }

      current_type <- if (!is.null(info$type)) info$type else "anova"
      if (!identical(current_type, "anova")) {
        return(NULL)
      }

      data <- df()
      req(data)

      build_anova_plot_info(data, info, layout_state$effective_input)
    })

    observe_layout_synchronization(plot_obj_info, layout_state, session)

    plot_obj <- reactive({
      info <- plot_obj_info()
      if (is.null(info)) return(NULL)
      info$plot
    })

    plot_size <- reactive({
      info <- plot_obj_info()
      w_sub <- input$plot_width
      h_sub <- input$plot_height

      if (is.null(w_sub) || is.na(w_sub)) w_sub <- 300
      if (is.null(h_sub) || is.na(h_sub)) h_sub <- 200

      if (is.null(info)) return(list(w = w_sub, h = h_sub))

      strata_cols <- info$layout$strata$cols
      strata_rows <- info$layout$strata$rows
      resp_cols <- info$layout$responses$ncol
      resp_rows <- info$layout$responses$nrow

      if (is.null(strata_cols) || strata_cols < 1) strata_cols <- 1
      if (is.null(strata_rows) || strata_rows < 1) strata_rows <- 1
      if (is.null(resp_cols) || resp_cols < 1) resp_cols <- 1
      if (is.null(resp_rows) || resp_rows < 1) resp_rows <- 1

      list(
        w = w_sub * strata_cols * resp_cols,
        h = h_sub * strata_rows * resp_rows
      )
    })

    output$plots <- renderPlot({
      info <- model_info()
      req(info)

      if (!is.null(info$type)) {
        if (info$type == "ggpairs") {
          validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
          return(build_ggpairs_plot(info$data))
        }

        if (info$type == "pca") {
          validate(need(!is.null(info$model), "Run PCA first."))
          pca_obj <- info$model

          color_var <- if (!is.null(input$pca_color) && input$pca_color != "None") input$pca_color else NULL
          shape_var <- if (!is.null(input$pca_shape) && input$pca_shape != "None") input$pca_shape else NULL
          label_var <- if (!is.null(input$pca_label) && input$pca_label != "None") input$pca_label else NULL
          label_size <- if (!is.null(input$pca_label_size) && !is.na(input$pca_label_size)) input$pca_label_size else 2

          return(
            build_pca_biplot(
              pca_obj,
              info$data,
              color_var = color_var,
              shape_var = shape_var,
              label_var = label_var,
              label_size = label_size
            )
          )
        }

      }

      req(plot_obj())
      plot_obj()

    },
    width = function() plot_size()$w,
    height = function() plot_size()$h,
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        info <- model_info()
        req(info)
        s <- plot_size()
        width_in  <- s$w / 96
        height_in <- s$h / 96

        g <- NULL

        if (!is.null(info$type)) {
          if (info$type == "ggpairs") {
            validate(need(ncol(info$data) >= 2, "Need at least two numeric columns for ggpairs."))
            g <- build_ggpairs_plot(info$data)
          } else if (info$type == "pca") {
            validate(need(!is.null(info$model), "Run PCA first."))
            pca_obj <- info$model

            color_var <- if (!is.null(input$pca_color) && input$pca_color != "None") input$pca_color else NULL
            shape_var <- if (!is.null(input$pca_shape) && input$pca_shape != "None") input$pca_shape else NULL
            label_var <- if (!is.null(input$pca_label) && input$pca_label != "None") input$pca_label else NULL
            label_size <- if (!is.null(input$pca_label_size) && !is.na(input$pca_label_size)) input$pca_label_size else 2

            g <- build_pca_biplot(
              pca_obj,
              info$data,
              color_var = color_var,
              shape_var = shape_var,
              label_var = label_var,
              label_size = label_size
            )
          }
        }

        if (is.null(g)) {
          req(plot_obj())
          g <- plot_obj()
        }

        ggsave(
          filename = file,
          plot = g,
          device = "png",
          dpi = 300,
          width = width_in,
          height = height_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
