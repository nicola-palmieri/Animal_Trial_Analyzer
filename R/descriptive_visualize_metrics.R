# ===============================================================
# 🟦 Descriptive Visualization — Summary Metrics
# ===============================================================

# ---- UI helpers ----
metric_panel_ui <- function(id, default_width = 400, default_height = 300,
                            default_rows = 1, default_cols = 1) {
  ns <- NS(id)
  tagList(
    subplot_size_ui(
      ns,
      width_value = default_width,
      height_value = default_height,
      width_help = "Set the width of each metric panel in pixels.",
      height_help = "Set the height of each metric panel in pixels."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, base_size_ui(
        ns,
        default = 13,
        help_text = "Adjust the base font size used for metric plot text elements."
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
      "Save the metric charts as an image file."
    )
  )
}


visualize_cv_ui <- visualize_outliers_ui <- visualize_missing_ui <- function(id) {
  metric_panel_ui(id, default_width = 400, default_height = 320, default_rows = 2, default_cols = 3)
}

metric_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}

visualize_cv_plot_ui <- visualize_outliers_plot_ui <- visualize_missing_plot_ui <- metric_plot_ui


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

  tidy$data$variable <- factor(tidy$data$variable, levels = numeric_vars)

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


build_metric_plot <- function(metric_info,
                              y_label,
                              title,
                              custom_colors = NULL,
                              base_size = 13) {
  df <- metric_info$data
  has_group <- isTRUE(metric_info$has_group)

  if (has_group) {
    legend_title <- if (!is.null(metric_info$group_label)) metric_info$group_label else "Group"
    palette <- resolve_palette_for_levels(levels(df$.group), custom = custom_colors)
    p <- ggplot(df, aes(x = variable, y = value, fill = .group)) +
      geom_col(position = position_dodge(width = 0.7), width = 0.65) +
      scale_fill_manual(values = palette) +
      labs(fill = legend_title)
  } else {
    p <- ggplot(df, aes(x = variable, y = value)) +
      geom_col(width = 0.65, fill = resolve_single_color(custom_colors)) +
      guides(fill = "none")
  }
  
  p +
    theme_minimal(base_size = base_size) +
    labs(x = NULL, y = y_label, title = title) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.minor = element_blank()
    )
}


metric_module_server <- function(id, filtered_data, summary_info, metric_key,
                                 y_label, title, filename_prefix, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resolve_dimension <- function(value, default) {
      if (is.null(value) || !is.numeric(value) || is.na(value)) default else value
    }

    plot_width <- reactive(resolve_dimension(input$plot_width, 400))
    plot_height <- reactive(resolve_dimension(input$plot_height, 300))

    module_active <- reactive({
      if (is.null(is_active)) TRUE else isTRUE(is_active())
    })

    color_var_reactive <- reactive({
      info <- summary_info()
      if (is.null(info)) return(NULL)

      group_var <- resolve_reactive(info$group_var)
      if (is.null(group_var) || group_var %in% c("", "None")) return(NULL)

      dat <- filtered_data()
      if (is.null(dat) || !is.data.frame(dat) || !group_var %in% names(dat)) return(NULL)

      group_var
    })

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = filtered_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )

    base_size <- base_size_server(
      input = input,
      default = 13
    )

    cached_plot_details <- reactiveVal(NULL)

    invalidate_cache <- function() {
      cached_plot_details(NULL)
    }

    observeEvent(
      list(
        summary_info(),
        filtered_data(),
        custom_colors(),
        base_size()
      ),
      {
        invalidate_cache()
      },
      ignoreNULL = FALSE
    )

    compute_plot_details <- function() {
      info <- summary_info()
      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_reactive(info$selected_vars)
      group_var <- resolve_reactive(info$group_var)
      strata_levels <- resolve_reactive(info$strata_levels)
      group_label <- resolve_reactive(info$group_label)

      numeric_vars <- names(dat)[vapply(dat, is.numeric, logical(1))]
      if (!is.null(selected_vars) && length(selected_vars) > 0) {
        # Preserve the order specified in the UI while filtering to numeric columns.
        numeric_vars <- selected_vars[selected_vars %in% numeric_vars]
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

      list(
        plot = build_metric_plot(
          metric_info,
          y_label,
          title,
          custom_colors = custom_colors(),
          base_size = base_size()
        )
      )
    }

    plot_details <- reactive({
      req(module_active())
      details <- cached_plot_details()
      if (is.null(details)) {
        details <- compute_plot_details()
        cached_plot_details(details)
      }
      details
    })

    plot_size <- reactive({
      req(module_active())
      list(w = plot_width(), h = plot_height())
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
      if (!is.null(details$warning) || is.null(details$plot)) return(NULL)
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


metric_server_factory <- function(metric_key, y_label, filename_prefix) {
  force(metric_key)
  force(y_label)
  force(filename_prefix)

  function(id, filtered_data, summary_info, is_active = NULL) {
    metric_module_server(
      id = id,
      filtered_data = filtered_data,
      summary_info = summary_info,
      metric_key = metric_key,
      y_label = y_label,
      title = "",
      filename_prefix = filename_prefix,
      is_active = is_active
    )
  }
}

visualize_cv_server <- metric_server_factory("cv", "CV (%)", "cv_summary")
visualize_outliers_server <- metric_server_factory("outliers", "Outlier Count", "outlier_summary")
visualize_missing_server <- metric_server_factory("missing", "Missing (%)", "missing_summary")
