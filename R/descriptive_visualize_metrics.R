# ===============================================================
# 🟦 Descriptive Visualization — Summary Metrics
# ===============================================================

# ---- UI helpers ----
metric_panel_ui <- function(id, default_width = 400, default_height = 300,
                            default_rows = 1, default_cols = 1) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  default_width, 200, 2000, 50)),
      column(6, numericInput(ns("plot_height"), "Subplot height (px)", default_height, 200, 2000, 50))
    ),
    fluidRow(
      column(6, numericInput(ns("resp_rows"), "Grid rows",    value = default_rows, min = 1, max = 10, step = 1)),
      column(6, numericInput(ns("resp_cols"), "Grid columns", value = default_cols, min = 1, max = 10, step = 1))
    ),
    hr(),
    downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
  )
}


visualize_cv_ui <- function(id) {
  metric_panel_ui(id, default_width = 400, default_height = 320, default_rows = 2, default_cols = 3)
}

visualize_outliers_ui <- function(id) {
  metric_panel_ui(id, default_width = 400, default_height = 320, default_rows = 2, default_cols = 3)
}

visualize_missing_ui <- function(id) {
  metric_panel_ui(id, default_width = 400, default_height = 320, default_rows = 2, default_cols = 3)
}

metric_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}

visualize_cv_plot_ui <- function(id) {
  metric_plot_ui(id)
}

visualize_outliers_plot_ui <- function(id) {
  metric_plot_ui(id)
}

visualize_missing_plot_ui <- function(id) {
  metric_plot_ui(id)
}


# ---- Shared computation helpers ----
resolve_metric_input <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.reactive(x)) x() else x
}

safe_cv <- function(x) {
  m <- mean(x, na.rm = TRUE)
  s <- stats::sd(x, na.rm = TRUE)
  if (!is.finite(m) || abs(m) < .Machine$double.eps) {
    return(NA_real_)
  }
  100 * s / m
}

count_outliers <- function(x) {
  q <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1]
  sum(x < q[1] - 1.5 * iqr | x > q[2] + 1.5 * iqr, na.rm = TRUE)
}

missing_pct <- function(x) {
  100 * mean(is.na(x))
}

prepare_metric_data <- function(data, numeric_vars, group_var, strata_levels, metric) {
  if (length(numeric_vars) == 0) {
    return(NULL)
  }

  if (is.null(group_var) || !group_var %in% names(data)) {
    group_var <- NULL
  }

  data_tbl <- tibble::as_tibble(data)

  if (!is.null(group_var)) {
    if (!is.null(strata_levels) && length(strata_levels) > 0) {
      data_tbl[[group_var]] <- factor(as.character(data_tbl[[group_var]]), levels = strata_levels)
      data_tbl <- droplevels(data_tbl)
    }
    data_tbl <- dplyr::group_by(data_tbl, .data[[group_var]], .drop = TRUE)
  }

  summarised <- switch(
    metric,
    cv = dplyr::summarise(
      data_tbl,
      dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ safe_cv(.x),
        .names = "cv_{.col}"
      ),
      .groups = "drop"
    ),
    outliers = dplyr::summarise(
      data_tbl,
      dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ count_outliers(.x),
        .names = "outliers_{.col}"
      ),
      .groups = "drop"
    ),
    missing = dplyr::summarise(
      data_tbl,
      dplyr::across(
        dplyr::all_of(numeric_vars),
        ~ missing_pct(.x),
        .names = "missing_{.col}"
      ),
      .groups = "drop"
    ),
    stop("Unsupported metric type.")
  )

  tidy <- tidy_descriptive_metric(summarised, metric)
  if (is.null(tidy)) {
    return(NULL)
  }

  tidy$data <- tidy$data[tidy$data$variable %in% numeric_vars, , drop = FALSE]
  if (nrow(tidy$data) == 0) {
    return(NULL)
  }

  if (!is.null(group_var) && !is.null(strata_levels) && length(strata_levels) > 0) {
    tidy$data$.group <- factor(as.character(tidy$data$.group), levels = strata_levels)
  }

  tidy
}

tidy_descriptive_metric <- function(df, prefix) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  metric_cols <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
  if (length(metric_cols) == 0) return(NULL)
  group_cols <- setdiff(names(df), metric_cols)
  has_group <- length(group_cols) > 0
  group_label <- if (has_group) paste(group_cols, collapse = " / ") else NULL
  if (!has_group) {
    df <- df |> dplyr::mutate(.group = "Overall")
    group_cols <- ".group"
  }
  tidy <- df |>
    tidyr::unite(".group", dplyr::all_of(group_cols), sep = " / ", remove = FALSE) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(metric_cols),
      names_to = "variable",
      values_to = "value"
    ) |>
    dplyr::mutate(
      variable = gsub(paste0("^", prefix, "_"), "", .data$variable),
      value = ifelse(is.finite(.data$value), .data$value, NA_real_),
      .group = factor(.data$.group, levels = unique(.data$.group))
    ) |>
    tidyr::drop_na("value")
  if (nrow(tidy) == 0) return(NULL)
  list(data = tidy, has_group = has_group, group_label = group_label)
}


build_metric_plot <- function(metric_info, y_label, title, n_rows, n_cols) {
  df <- metric_info$data
  has_group <- isTRUE(metric_info$has_group)
  
  if (has_group) {
    legend_title <- if (!is.null(metric_info$group_label)) metric_info$group_label else "Group"
    palette <- resolve_palette_for_levels(levels(df$.group))
    p <- ggplot(df, aes(x = variable, y = value, fill = .group)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      scale_fill_manual(values = palette) +
      labs(fill = legend_title)
  } else {
    p <- ggplot(df, aes(x = variable, y = value)) +
      geom_col(width = 0.65, fill = resolve_single_color()) +
      guides(fill = "none")
  }
  
  p +
    theme_minimal(base_size = 13) +
    labs(x = NULL, y = y_label, title = title) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}


metric_module_server <- function(id, filtered_data, summary_info, metric_key,
                                 y_label, title, filename_prefix, is_active = NULL) {
  moduleServer(id, function(input, output, session) {

    plot_width <- reactive({
      w <- input$plot_width
      if (is.null(w) || !is.numeric(w) || is.na(w)) 400 else w
    })

    plot_height <- reactive({
      h <- input$plot_height
      if (is.null(h) || !is.numeric(h) || is.na(h)) 300 else h
    })

    module_active <- reactive({
      if (is.null(is_active)) {
        TRUE
      } else {
        isTRUE(is_active())
      }
    })

    plot_details <- reactive({
      req(module_active())

      info <- summary_info()
      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_metric_input(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_metric_input(info$selected_vars)
      group_var <- resolve_metric_input(info$group_var)
      strata_levels <- resolve_metric_input(info$strata_levels)
      group_label <- resolve_metric_input(info$group_label)

      numeric_vars <- names(dat)[vapply(dat, is.numeric, logical(1))]
      if (!is.null(selected_vars) && length(selected_vars) > 0) {
        numeric_vars <- intersect(numeric_vars, selected_vars)
      }
      validate(need(length(numeric_vars) > 0, "No numeric variables available for plotting."))

      metric_info <- prepare_metric_data(
        data = dat,
        numeric_vars = numeric_vars,
        group_var = group_var,
        strata_levels = strata_levels,
        metric = metric_key
      )

      validate(need(!is.null(metric_info), "Unable to compute metric for the selected variables."))

      if (!is.null(group_label)) {
        metric_info$group_label <- group_label
      }

      n_rows <- basic_grid_value(input$resp_rows, default = 2)
      n_cols <- basic_grid_value(input$resp_cols, default = 3)

      plot <- build_metric_plot(metric_info, y_label, title, n_rows, n_cols)

      list(
        plot = plot,
        layout = list(nrow = n_rows, ncol = n_cols)
      )
    })

    plot_size <- reactive({
      req(module_active())
      details <- plot_details()
      if (is.null(details$layout)) {
        list(w = plot_width(), h = plot_height())
      } else {
        list(
          w = plot_width()  * details$layout$ncol,
          h = plot_height() * details$layout$nrow
        )
      }
    })

    output$download_plot <- downloadHandler(
      filename = function() paste0(filename_prefix, "_", Sys.Date(), ".png"),
      content = function(file) {
        req(module_active())
        details <- plot_details()
        req(details$plot)
        size <- plot_size()
        ggplot2::ggsave(
          filename = file,
          plot = details$plot,
          device = "png",
          dpi = 300,
          width = size$w / 96,
          height = size$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )

    output$plot <- renderPlot({
      req(module_active())
      details <- plot_details()
      validate(need(!is.null(details$plot), "No plot available."))
      print(details$plot)
    },
    width = function() {
      req(module_active())
      plot_size()$w
    },
    height = function() {
      req(module_active())
      plot_size()$h
    },
    res = 96)
  })
}


visualize_cv_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  metric_module_server(
    id = id,
    filtered_data = filtered_data,
    summary_info = summary_info,
    metric_key = "cv",
    y_label = "CV (%)",
    title = "",
    filename_prefix = "cv_summary",
    is_active = is_active
  )
}

visualize_outliers_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  metric_module_server(
    id = id,
    filtered_data = filtered_data,
    summary_info = summary_info,
    metric_key = "outliers",
    y_label = "Outlier Count",
    title = "",
    filename_prefix = "outlier_summary",
    is_active = is_active
  )
}

visualize_missing_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  metric_module_server(
    id = id,
    filtered_data = filtered_data,
    summary_info = summary_info,
    metric_key = "missing",
    y_label = "Missing (%)",
    title = "",
    filename_prefix = "missing_summary",
    is_active = is_active
  )
}
