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
    
    if (!requireNamespace("digest", quietly = TRUE)) stop("Please install the 'digest' package.")
    
    hash_key <- function(data, cols = NULL) {
      if (is.null(data) || !is.data.frame(data)) return("no-data")
      keep <- if (is.null(cols)) names(data) else intersect(cols, names(data))
      digest::digest(data[, keep, drop = FALSE], algo = "xxhash64")
    }
    
    active <- reactive({
      if (is.null(is_active)) TRUE else isTRUE(is_active())
    })
    
    plot_width  <- reactive({ as.numeric(input$plot_width  %||% 400) })
    plot_height <- reactive({ as.numeric(input$plot_height %||% 300) })
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    base_size <- base_size_server(input, default = 13)
    
    color_var <- reactive({
      info <- summary_info()
      dat <- filtered_data()
      g <- resolve_reactive(info$group_var)
      if (is.null(info) || is.null(dat) || !is.data.frame(dat)) return(NULL)
      if (is.null(g) || g == "" || g == "None" || !g %in% names(dat)) return(NULL)
      g
    })
    
    custom_colors <- add_color_customization_server(
      ns, input, output, filtered_data, color_var, multi_group = TRUE
    )
    
    state <- reactive({
      list(
        info = summary_info(),
        dat = filtered_data(),
        proportions = isTRUE(input$show_proportions),
        labels = isTRUE(input$show_value_labels),
        colors = custom_colors(),
        base_size = base_size(),
        rows = grid$rows(),
        cols = grid$cols()
      )
    })
    
    plot_info <- reactive({
      req(active())
      s <- state()
      info <- s$info
      req(!is.null(info))
      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else s$dat
      req(!is.null(dat), is.data.frame(dat), nrow(dat) > 0)
      build_descriptive_categorical_plot(
        df = dat,
        selected_vars = resolve_reactive(info$selected_vars),
        group_var = resolve_reactive(info$group_var),
        strata_levels = resolve_reactive(info$strata_levels),
        show_proportions = s$proportions,
        show_value_labels = s$labels,
        nrow_input = s$rows,
        ncol_input = s$cols,
        fill_colors = s$colors,
        base_size = s$base_size
      )
    })
    
    plot_dimensions <- reactive({
      req(active())
      lay <- plot_info()$layout
      nrow_l <- if (!is.null(lay$nrow)) as.integer(lay$nrow) else 1L
      ncol_l <- if (!is.null(lay$ncol)) as.integer(lay$ncol) else 1L
      list(
        width = max(200, plot_width()  * ncol_l),
        height = max(200, plot_height() * nrow_l)
      )
    })
    
    output$grid_warning <- renderUI({
      req(active())
      info <- plot_info()
      if (!is.null(info$warning)) div(class = "alert alert-warning", info$warning)
    })
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("categorical_barplots_", Sys.Date(), ".png"),
      content = function(file) {
        req(active())
        info <- plot_info()
        req(is.null(info$warning), !is.null(info$plot))
        s <- plot_dimensions()
        ggplot2::ggsave(
          file, info$plot, device = "png", dpi = 300,
          width = s$width / 96, height = s$height / 96,
          units = "in", limitsize = FALSE
        )
      }
    )
    
    cached_plot <- reactiveVal(NULL)
    cached_key  <- reactiveVal(NULL)
    
    observe({
      req(active())
      s <- state()
      dat <- s$dat
      cols <- resolve_reactive(s$info$selected_vars)
      key <- paste(
        hash_key(dat, cols),
        resolve_reactive(s$info$group_var),
        resolve_reactive(s$info$strata_levels),
        s$proportions,
        s$labels,
        s$colors,
        s$base_size,
        sep = "_"
      )
      if (!identical(key, cached_key())) {
        info <- isolate(plot_info())
        if (!is.null(info$plot)) {
          cached_plot(info$plot)
          cached_key(key)
        }
      }
    })
    
    output$plot <- renderPlot({
      req(active())
      p <- cached_plot()
      validate(need(!is.null(p), "Plot not ready"))
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    outputOptions(output, "plot", suspendWhenHidden = TRUE)
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

add_value_labels <- function(plot,
                             data,
                             show_value_labels,
                             show_proportions,
                             position = NULL,
                             base_size = 13) {
  if (!isTRUE(show_value_labels)) return(plot)

  label_df <- format_value_labels(data, show_proportions)

  position <- if (is.null(position)) "identity" else position

  plot +
    geom_text(
      data = label_df,
      aes(label = label_text, y = label_y, vjust = label_vjust),
      position = position,
      color = "gray20",
      size = compute_label_text_size(base_size),
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

      p <- add_value_labels(p, count_df, show_value_labels, show_proportions, group_dodge, base_size)
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

      p <- add_value_labels(p, count_df, show_value_labels, show_proportions, base_size = base_size)
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

