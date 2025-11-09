# ===============================================================
# 🟦 Descriptive Visualization — Categorical Barplots
# ===============================================================

visualize_categorical_barplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("show_proportions"), "Show proportions instead of counts", FALSE),
      "Switch between raw counts and percentages for each category."
    ),
    with_help_tooltip(
      checkboxInput(ns("show_value_labels"), "Show value labels on bars", FALSE),
      "Display the numeric value on top of each bar."
    ),
    fluidRow(
      column(6, with_help_tooltip(
        numericInput(ns("plot_width"),  "Subplot width (px)",  400, 200, 2000, 50),
        "Set the width of each categorical plot in pixels."
      )),
      column(6, with_help_tooltip(
        numericInput(ns("plot_height"), "Subplot height (px)", 300, 200, 2000, 50),
        "Set the height of each categorical plot in pixels."
      ))
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display when several charts are shown.",
      cols_help = "Choose how many columns of plots to display when several charts are shown."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, base_size_ui(
        ns,
        default = 13,
        help_text = "Adjust the base font size used for barplot text elements."
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
      "Save the categorical barplots as an image file."
    )
  )
}

visualize_categorical_barplots_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}

visualize_categorical_barplots_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resolve_input_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x)) x() else x
    }

    module_active <- reactive({
      if (is.null(is_active)) {
        TRUE
      } else {
        isTRUE(is_active())
      }
    })

    plot_width <- reactive({
      w <- input$plot_width
      if (is.null(w) || !is.numeric(w) || is.na(w)) 400 else w
    })

    plot_height <- reactive({
      h <- input$plot_height
      if (is.null(h) || !is.numeric(h) || is.na(h)) 300 else h
    })
    
    color_var_reactive <- reactive({
      info <- summary_info()
      if (is.null(info)) return(NULL)

      group_var <- resolve_input_value(info$group_var)
      if (is.null(group_var) || identical(group_var, "") || identical(group_var, "None")) {
        return(NULL)
      }

      dat <- filtered_data()
      if (is.null(dat) || !is.data.frame(dat) || !group_var %in% names(dat)) {
        return(NULL)
      }

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

    cached_plot_info <- reactiveVal(NULL)
    cache_ready <- reactiveVal(FALSE)

    grid_inputs <- plot_grid_server("plot_grid")

    invalidate_cache <- function() {
      cached_plot_info(NULL)
      cache_ready(FALSE)
      invisible(TRUE)
    }

    observeEvent(
      list(
        summary_info(),
        filtered_data(),
        input$show_proportions,
        input$show_value_labels,
        custom_colors(),
        base_size()
      ),
      {
        invalidate_cache()
      },
      ignoreNULL = FALSE
    )

    observeEvent(grid_inputs$values(), {
      invalidate_cache()
    }, ignoreNULL = FALSE)

    compute_plot_info <- function() {
      info <- summary_info()

      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_input_value(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_input_value(info$selected_vars)
      group_var     <- resolve_input_value(info$group_var)
      strata_levels <- resolve_input_value(info$strata_levels)

      out <- build_descriptive_categorical_plot(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        strata_levels = strata_levels,
        show_proportions = isTRUE(input$show_proportions),
        nrow_input = grid_inputs$rows(),
        ncol_input = grid_inputs$cols(),
        fill_colors = custom_colors(),
        show_value_labels = isTRUE(input$show_value_labels),
        base_size = base_size()
      )
      validate(need(!is.null(out), "No categorical variables available for plotting."))
      out
    }

    plot_info <- reactive({
      req(module_active())
      if (!isTRUE(cache_ready())) {
        cached_plot_info(compute_plot_info())
        cache_ready(TRUE)
      }
      cached_plot_info()
    })

    plot_size <- reactive({
      req(module_active())
      info <- plot_info()
      layout <- info$layout
      if (is.null(layout)) {
        list(w = plot_width(), h = plot_height())
      } else {
        list(
          w = plot_width()  * layout$ncol,
          h = plot_height() * layout$nrow
        )
      }
    })

    output$grid_warning <- renderUI({
      req(module_active())
      info <- plot_info()
      if (!is.null(info$warning)) {
        div(class = "alert alert-warning", info$warning)
      } else {
        NULL
      }
    })

    output$download_plot <- downloadHandler(
      filename = function() paste0("categorical_barplots_", Sys.Date(), ".png"),
      content  = function(file) {
        req(module_active())
        info <- plot_info()
        req(is.null(info$warning))
        req(info$plot)
        s <- plot_size()
        ggplot2::ggsave(
          filename = file,
          plot = info$plot,
          device = "png",
          dpi = 300,
          width  = s$w / 96,
          height = s$h / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )

    output$plot <- renderPlot({
      req(module_active())
      info <- plot_info()
      if (!is.null(info$warning) || is.null(info$plot)) return(NULL)
      print(info$plot)
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


format_value_labels <- function(df, show_proportions) {
  formatter <- if (isTRUE(show_proportions)) {
    scales::label_percent(accuracy = 0.1, trim = TRUE)
  } else {
    scales::label_comma(accuracy = 1, trim = TRUE)
  }

  df |>
    dplyr::mutate(
      label_text = formatter(.data$value),
      label_y = .data$value,
      label_vjust = ifelse(.data$value >= 0, -0.4, 1.2)
    )
}

apply_value_scale <- function(plot, show_proportions, show_value_labels) {
  if (isTRUE(show_proportions)) {
    scale_args <- list(labels = scales::percent_format(accuracy = 1))
    if (isTRUE(show_value_labels)) {
      scale_args$limits <- c(0, NA)
      scale_args$expand <- expansion(mult = c(0.02, 0.12))
    } else {
      scale_args$limits <- c(0, 1)
    }
    plot + do.call(scale_y_continuous, scale_args)
  } else if (isTRUE(show_value_labels)) {
    plot + scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.05, 0.12)))
  } else {
    plot
  }
}

add_value_labels <- function(plot, data, show_value_labels, show_proportions, position = NULL) {
  if (!isTRUE(show_value_labels)) return(plot)

  label_df <- format_value_labels(data, show_proportions)

  position <- if (is.null(position)) "identity" else position

  plot +
    geom_text(
      data = label_df,
      aes(label = label_text, y = label_y, vjust = label_vjust),
      position = position,
      color = "gray20",
      size = 3.5,
      fontface = "bold"
    )
}

build_descriptive_categorical_plot <- function(df,
                                               selected_vars = NULL,
                                               group_var = NULL,
                                               strata_levels = NULL,
                                               show_proportions = FALSE,
                                               nrow_input = NULL,
                                               ncol_input = NULL,
                                               fill_colors = NULL,
                                               show_value_labels = FALSE,
                                               base_size = 13) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  
  factor_vars <- names(df)[vapply(df, function(x) {
    is.character(x) || is.factor(x) || is.logical(x)
  }, logical(1))]
  
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    factor_vars <- intersect(factor_vars, selected_vars)
  }
  if (length(factor_vars) == 0) return(NULL)
  
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.character(df[[group_var]])
    df[[group_var]][is.na(df[[group_var]]) | trimws(df[[group_var]]) == ""] <- "Missing"
    
    if (!is.null(strata_levels) && length(strata_levels) > 0) {
      keep_levels <- unique(strata_levels)
      df <- df[df[[group_var]] %in% keep_levels, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      df[[group_var]] <- factor(df[[group_var]], levels = keep_levels)
    } else {
      df[[group_var]] <- factor(df[[group_var]], levels = unique(df[[group_var]]))
    }
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(factor_vars, function(var) {
    group_col <- if (!is.null(group_var) && !identical(group_var, var)) group_var else NULL
    cols_to_use <- c(var, group_col)
    cols_to_use <- cols_to_use[cols_to_use %in% names(df)]
    var_data <- df[, cols_to_use, drop = FALSE]
    
    var_data[[var]] <- as.character(var_data[[var]])
    keep <- !is.na(var_data[[var]]) & trimws(var_data[[var]]) != ""
    if (!any(keep)) return(NULL)
    var_data <- var_data[keep, , drop = FALSE]
    
    level_order <- if (is.factor(df[[var]])) {
      as.character(levels(df[[var]]))
    } else {
      unique(var_data[[var]])
    }
    var_data[[var]] <- factor(var_data[[var]], levels = level_order)
    
    y_label <- if (isTRUE(show_proportions)) "Proportion" else "Count"
    
    if (!is.null(group_col)) {
      var_data[[group_col]] <- droplevels(var_data[[group_col]])
      count_df <- dplyr::count(var_data, .data[[var]], .data[[group_col]], name = "count")
      if (nrow(count_df) == 0) return(NULL)

      if (isTRUE(show_proportions)) {
        count_df <- count_df |>
          dplyr::group_by(.data[[group_col]]) |>
          dplyr::mutate(total = sum(.data$count, na.rm = TRUE)) |>
          dplyr::mutate(value = ifelse(.data$total > 0, .data$count / .data$total, 0)) |>
          dplyr::ungroup()
        count_df$total <- NULL
      } else {
        count_df <- dplyr::mutate(count_df, value = .data$count)
      }

      count_df[[var]] <- factor(as.character(count_df[[var]]), levels = level_order)
      group_levels <- levels(droplevels(var_data[[group_col]]))
      count_df[[group_col]] <- factor(as.character(count_df[[group_col]]), levels = group_levels)

      palette <- resolve_palette_for_levels(group_levels, custom = fill_colors)
      group_dodge <- position_dodge(width = 0.75)

      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value, fill = .data[[group_col]])) +
        geom_col(position = group_dodge, width = 0.65) +
        scale_fill_manual(values = palette) +
        theme_minimal(base_size = base_size) +
        labs(title = var, x = NULL, y = y_label, fill = group_col) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      p <- add_value_labels(p, count_df, show_value_labels, show_proportions, group_dodge)
      p <- apply_value_scale(p, show_proportions, show_value_labels)
      p
    } else {
      count_df <- dplyr::count(var_data, .data[[var]], name = "count")
      if (nrow(count_df) == 0) return(NULL)

      total <- sum(count_df$count, na.rm = TRUE)
      if (isTRUE(show_proportions) && total > 0) {
        count_df$value <- count_df$count / total
      } else {
        count_df$value <- count_df$count
      }

      count_df[[var]] <- factor(as.character(count_df[[var]]), levels = level_order)

      single_fill <- if (!is.null(fill_colors) && length(fill_colors) > 0) {
        fill_colors[1]
      } else {
        resolve_single_color()
      }

      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value)) +
        geom_col(fill = single_fill, width = 0.65) +
        theme_minimal(base_size = base_size) +
        labs(title = var, x = NULL, y = y_label) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      p <- add_value_labels(p, count_df, show_value_labels, show_proportions)
      p <- apply_value_scale(p, show_proportions, show_value_labels)
      p
    }
  })
  
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  n_panels <- length(plots)
  defaults <- compute_default_grid(n_panels)

  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols
  )

  layout <- adjust_grid_layout(n_panels, layout)

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)

  combined <- NULL
  if (isTRUE(validation$valid)) {
    combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol) +
      patchwork::plot_annotation(
        theme = theme(plot.title = element_text(size = 16, face = "bold"))
      )
  }

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = n_panels,
    warning = validation$message,
    defaults = defaults
  )
}

