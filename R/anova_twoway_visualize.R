# ===============================================================
# 🧪 Visualization Module — Two-way ANOVA (Simplified & Consistent)
# ===============================================================

visualize_twoway_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize two-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Lineplots (mean ± SE)" = "lineplot_mean_se",
            "Barplots (mean ± SE)"  = "barplot_mean_se"
          ),
          selected = "lineplot_mean_se"
        ),
        "Pick the chart style you prefer for viewing group means and uncertainty."
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'barplot_mean_se'", ns("plot_type")),
        with_help_tooltip(
          checkboxInput(
            ns("show_bar_labels"),
            "Show value labels on bars",
            value = FALSE
          ),
          "Turn on labels to display the mean value on each bar."
        )
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(4, with_help_tooltip(
            checkboxInput(
              ns("lineplot_show_lines"),
              "Connect means with lines",
              value = FALSE
            ),
            "Draw connecting lines between group means."
          )),
          column(4, with_help_tooltip(
            checkboxInput(
              ns("lineplot_use_dodge"),
              "Dodge grouped means",
              value = TRUE
            ),
            "Offset the level means of the second factor along the x-axis to prevent overlap."
          )),
          column(4, with_help_tooltip(
            checkboxInput(
              ns("lineplot_show_jitter"),
              "Overlay jittered data",
              value = FALSE
            ),
            "Overlay raw observations with light jitter for context."
          ))
        )
      ),
      with_help_tooltip(
        checkboxInput(
          ns("share_y_axis"),
          "Use common y-axis across plots",
          value = FALSE
        ),
        "Lock the y-axis range so every subplot uses the same scale."
      ),
      fluidRow(
        column(6, with_help_tooltip(
          numericInput(ns("plot_width"),  "Subplot width (px)",  value = 400, min = 200, max = 1200, step = 50),
          "Set how wide each subplot should be in pixels."
        )),
        column(6, with_help_tooltip(
          numericInput(ns("plot_height"), "Subplot height (px)", value = 300, min = 200, max = 1200, step = 50),
          "Set how tall each subplot should be in pixels."
        ))
      ),
      uiOutput(ns("layout_controls")),
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = TRUE)),
        column(6, tagList(
          base_size_ui(
            ns,
            default = 13,
            help_text = "Adjust the base font size used for the ANOVA plots."
          ),
          uiOutput(ns("common_legend_controls"))
        ))
      ),
      br(),
      with_help_tooltip(
        downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
        "Save the current figure as an image file."
      )
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      # Pre-mounted panels for instant switching
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        plotOutput(ns("plot_line"), height = "auto")
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] === 'barplot_mean_se'", ns("plot_type")),
        plotOutput(ns("plot_bar"), height = "auto")
      )
    )
  )
}


visualize_twoway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive(filtered_data())
    
    color_var <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$factors)) return(NULL)
      info$factors$factor2
    })
    
    factor2_levels <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$orders)) return(NULL)
      info$orders$order2
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, df,
      color_var_reactive = color_var,
      multi_group = TRUE,
      level_order_reactive = factor2_levels
    )
    
    base_size <- base_size_server(input, default = 13)
    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")

    active <- reactive(TRUE)

    legend_state <- reactiveValues(
      enabled = FALSE,
      position = "bottom"
    )

    observeEvent(input$use_common_legend, {
      req(!is.null(input$use_common_legend))
      legend_state$enabled <- isTRUE(input$use_common_legend)
    }, ignoreNULL = TRUE)

    observeEvent(input$common_legend_position, {
      req(!is.null(input$common_legend_position))
      legend_state$position <- input$common_legend_position
    }, ignoreNULL = TRUE)

    common_legend_available <- reactive({
      info <- model_info()
      if (is.null(info)) {
        return(FALSE)
      }
      has_multiple_responses <- length(info$responses %||% character()) > 1
      has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
      has_multiple_responses || has_strata
    })

    observeEvent(common_legend_available(), {
      if (!isTRUE(common_legend_available())) {
        legend_state$enabled <- FALSE
      }
    })

    output$common_legend_controls <- renderUI({
      if (!isTRUE(common_legend_available()) || input$plot_type != "lineplot_mean_se") {
        return(NULL)
      }

      checkbox <- div(
        class = "mt-3",
        with_help_tooltip(
          checkboxInput(
            ns("use_common_legend"),
            "Use common legend",
            value = isTRUE(legend_state$enabled)
          ),
          "Merge the legends across responses or strata into a single shared legend."
        )
      )

      legend_position <- if (isTRUE(legend_state$enabled)) {
        div(
          class = "mt-2",
          with_help_tooltip(
            selectInput(
              ns("common_legend_position"),
              "Legend position",
              choices = c(
                "Bottom" = "bottom",
                "Right" = "right",
                "Top" = "top",
                "Left" = "left"
              ),
              selected = legend_state$position %||% "bottom"
            ),
            "Choose where the combined legend should be displayed."
          )
        )
      } else {
        NULL
      }

      tagList(checkbox, legend_position)
    })

    state <- reactive({
      use_common_legend <- isTRUE(common_legend_available()) &&
        input$plot_type == "lineplot_mean_se" &&
        isTRUE(legend_state$enabled)
      legend_pos <- legend_state$position
      valid_positions <- c("bottom", "top", "left", "right")
      resolved_position <- if (!is.null(legend_pos) && legend_pos %in% valid_positions) {
        legend_pos
      } else {
        "bottom"
      }
      list(
        data        = df(),
        info        = model_info(),
        strata_rows = strata_grid$rows(),
        strata_cols = strata_grid$cols(),
        resp_rows   = response_grid$rows(),
        resp_cols   = response_grid$cols(),
        colors        = custom_colors(),
        base_size     = base_size(),
        show_labels   = isTRUE(input$show_bar_labels),
        show_lines    = isTRUE(input$lineplot_show_lines),
        use_dodge     = isTRUE(input$lineplot_use_dodge),
        show_jitter   = isTRUE(input$lineplot_show_jitter),
        plot_type     = input$plot_type,
        share_y_axis  = isTRUE(input$share_y_axis),
        common_legend = use_common_legend,
        legend_position = if (use_common_legend) resolved_position else NULL
      )
    })

    compute_all_plots <- function(data,
                                  info,
                                  layout_inputs,
                                  colors,
                                  base_size_value,
                                  show_labels,
                                  show_lines,
                                  show_jitter,
                                  use_dodge,
                                  share_y_axis,
                                  common_legend = FALSE,
                                  legend_position = NULL) {
      if (is.null(info) || !identical(info$type, "twoway_anova") || is.null(data) || nrow(data) == 0) {
        return(list(
          lineplot_mean_se = list(plot = NULL, warning = "No data or results available.", layout = NULL),
          barplot_mean_se  = list(plot = NULL, warning = "No data or results available.", layout = NULL)
        ))
      }

      responses <- info$responses
      if (!is.null(responses) && length(responses) > 0 && is.data.frame(data)) {
        missing_cols <- setdiff(responses, names(data))
        if (length(missing_cols) > 0) {
          message <- sprintf(
            "The following response variable(s) are no longer available in the dataset: %s.",
            paste(missing_cols, collapse = ", ")
          )
        } else {
          non_numeric <- responses[!vapply(responses, function(col) is.numeric(data[[col]]), logical(1))]
          message <- if (length(non_numeric) > 0) {
            sprintf(
              "The selected response variable(s) must be numeric for visualization. Please update their type in the Upload tab: %s.",
              paste(non_numeric, collapse = ", ")
            )
          } else {
            NULL
          }
        }

        if (!is.null(message)) {
          placeholder_layout <- list(
            strata = list(rows = 1L, cols = 1L),
            responses = list(rows = 1L, cols = 1L)
          )
          placeholder <- list(plot = NULL, warning = message, layout = placeholder_layout)
          return(list(
            lineplot_mean_se = placeholder,
            barplot_mean_se  = placeholder
          ))
        }
      }
      list(
        lineplot_mean_se = plot_anova_lineplot_meanse(
          data, info, layout_inputs,
          line_colors = colors,
          base_size = base_size_value,
          show_lines = show_lines,
          show_jitter = show_jitter,
          use_dodge = use_dodge,
          share_y_axis = share_y_axis,
          common_legend = common_legend,
          legend_position = legend_position
        ),
        barplot_mean_se = plot_anova_barplot_meanse(
          data, info, layout_values = layout_inputs,
          line_colors = colors,
          show_value_labels = show_labels,
          base_size = base_size_value,
          posthoc_all = info$posthoc,
          share_y_axis = share_y_axis
        )
      )
    }
    
    plot_info <- reactive({
      s <- state()
      req(!is.null(s$data), !is.null(s$info))
      layout_inputs <- list(
        strata_rows = s$strata_rows,
        strata_cols = s$strata_cols,
        resp_rows   = s$resp_rows,
        resp_cols   = s$resp_cols
      )
      res <- compute_all_plots(
        s$data, s$info, layout_inputs,
        s$colors, s$base_size,
        s$show_labels,
        s$show_lines,
        s$show_jitter,
        s$use_dodge,
        s$share_y_axis,
        s$common_legend,
        s$legend_position
      )
      res[[if (!is.null(s$plot_type) && s$plot_type %in% names(res)) s$plot_type else "lineplot_mean_se"]]
    })
    
    # ---- Cached ggplot object to avoid flicker ----
    if (!requireNamespace("digest", quietly = TRUE)) stop("Please install the 'digest' package.")
    hash_key <- function(data) {
      if (is.null(data) || !is.data.frame(data)) return("no-data")
      digest::digest(data, algo = "xxhash64")
    }
    
    cached_plot <- reactiveVal(NULL)
    cached_key  <- reactiveVal(NULL)
    
    observe({
      s <- state()
      dat <- s$data
      key <- paste(
        hash_key(dat),
        s$plot_type,
        s$show_labels,
        s$show_lines,
        s$show_jitter,
        s$use_dodge,
        s$colors,
        s$base_size,
        s$strata_rows,
        s$strata_cols,
        s$resp_rows,
        s$resp_cols,
        s$share_y_axis,
        s$common_legend,
        s$legend_position %||% "none",
        sep = "_"
      )
      if (!identical(key, cached_key())) {
        info <- plot_info()
        if (!is.null(info$plot)) {
          cached_plot(info$plot)
          cached_key(key)
        }
      }
    })
    
    plot_dimensions <- reactive({
      info <- plot_info()
      lay <- info$layout
      nrow_l <- (lay$strata$rows %||% 1L) * (lay$responses$rows %||% 1L)
      ncol_l <- (lay$strata$cols %||% 1L) * (lay$responses$cols %||% 1L)
      list(
        width = max(200, as.numeric(input$plot_width  %||% 400) * ncol_l),
        height = max(200, as.numeric(input$plot_height %||% 300) * nrow_l)
      )
    })
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    output$plot_warning <- renderUI({
      info <- plot_info()
      if (!is.null(info$warning))
        div(class = "alert alert-warning", HTML(info$warning))
    })
    
    output$plot_line <- renderPlot({
      p <- cached_plot()
      if (is.null(p) || input$plot_type != "lineplot_mean_se") return(NULL)
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    output$plot_bar <- renderPlot({
      p <- cached_plot()
      if (is.null(p) || input$plot_type != "barplot_mean_se") return(NULL)
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- cached_plot()
        req(!is.null(p))
        s <- plot_dimensions()
        ggsave(
          file, p, device = "png", dpi = 300,
          width = s$width / 96, height = s$height / 96,
          units = "in", limitsize = FALSE
        )
      }
    )
    
    outputOptions(output, "plot_line", suspendWhenHidden = TRUE)
    outputOptions(output, "plot_bar",  suspendWhenHidden = TRUE)
    
  })
}

