# ===============================================================
# 🟦 Descriptive Visualization — Numeric Boxplots
# ===============================================================

visualize_numeric_boxplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(ns("show_points"), "Show individual data points", TRUE),
      "Add the raw observations on top of each boxplot."
    ),
    with_help_tooltip(
      checkboxInput(ns("show_outliers"), "Highlight boxplot outliers", FALSE),
      "Highlight points that fall outside the typical range."
    ),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("show_outliers")),
      uiOutput(ns("outlier_label_ui"))
    ),
    subplot_size_ui(
      ns,
      width_value = 200,
      height_value = 800,
      width_help = "Control how wide each boxplot panel should be.",
      height_help = "Control how tall each boxplot panel should be."
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display when multiple charts are shown.",
      cols_help = "Choose how many columns of plots to display when multiple charts are shown.",
      cols_max = 100L
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, tagList(
        base_size_ui(
          ns,
          default = 13,
          help_text = "Adjust the base font size used for boxplot text elements."
        ),
        uiOutput(ns("common_legend_controls"))
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"),
      "Save the boxplots as an image file."
    )
  )
}


visualize_numeric_boxplots_plot_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ta-plot-container",
    uiOutput(ns("grid_warning")),
    plotOutput(ns("plot"), width = "100%", height = "auto")
  )
}


visualize_numeric_boxplots_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    hash_key <- function(data, cols = NULL) {
      if (is.null(data) || !is.data.frame(data)) return("no-data")
      keep <- if (is.null(cols)) names(data) else intersect(cols, names(data))
      digest::digest(data[, keep, drop = FALSE], algo = "xxhash64")
    }
    
    active <- reactive({
      if (is.null(is_active)) TRUE else isTRUE(is_active())
    })
    
    plot_width  <- reactive({ as.numeric(input$plot_width  %||% 200) })
    plot_height <- reactive({ as.numeric(input$plot_height %||% 800) })
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
    
    output$outlier_label_ui <- renderUI({
      dat <- filtered_data()
      if (is.null(dat) || !is.data.frame(dat)) return(NULL)
      cat_cols <- names(dat)[vapply(dat, \(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      cat_cols <- sort(unique(cat_cols))
      current <- isolate(input$outlier_label)
      current <- if (is.null(current) || !nzchar(current) || !current %in% cat_cols) "" else current
      with_help_tooltip(
        selectInput(
          ns("outlier_label"),
          "Label outliers by",
          choices = c("None" = "", stats::setNames(cat_cols, cat_cols)),
          selected = current
        ),
        "Choose a column to annotate the highlighted outliers."
      )
    })
    
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

    state <- reactive({
      legend_allowed <- !is.null(color_var())
      valid_positions <- c("bottom", "top", "left", "right")
      resolved_position <- if (!is.null(legend_state$position) && legend_state$position %in% valid_positions) {
        legend_state$position
      } else {
        "bottom"
      }
      use_common_legend <- legend_allowed && isTRUE(legend_state$enabled)
      list(
        info = summary_info(),
        dat = filtered_data(),
        show_points = isTRUE(input$show_points),
        show_outliers = isTRUE(input$show_outliers),
        label_var = validate_outlier_label(input$outlier_label),
        colors = custom_colors(),
        base_size = base_size(),
        rows = grid$rows(),
        cols = grid$cols(),
        common_legend = use_common_legend,
        legend_position = if (use_common_legend) resolved_position else NULL
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
      build_descriptive_numeric_boxplot(
        df = dat,
        selected_vars = resolve_reactive(info$selected_vars),
        group_var = resolve_reactive(info$group_var),
        show_points = s$show_points,
        show_outliers = s$show_outliers,
        outlier_label_var = s$label_var,
        nrow_input = s$rows,
        ncol_input = s$cols,
        custom_colors = s$colors,
        base_size = s$base_size,
        common_legend = s$common_legend,
        legend_position = s$legend_position
      )
    })

    observeEvent(plot_info(), {
      req(active())
      info <- plot_info()
      apply_grid_defaults_if_empty(input, session, "plot_grid", info$defaults, n_items = info$panels)
    }, ignoreNULL = TRUE)

    common_legend_available <- reactive({
      req(active())
      info <- plot_info()
      has_group <- !is.null(color_var())
      n_panels <- info$panels %||% 0L
      has_group && n_panels > 1L
    })

    observeEvent(common_legend_available(), {
      if (!isTRUE(common_legend_available())) {
        legend_state$enabled <- FALSE
      }
    })

    output$common_legend_controls <- renderUI({
      legend_supported <- TRUE
      if (!isTRUE(common_legend_available()) || !legend_supported) {
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
          "Merge the legends across panels into a single shared legend."
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
      filename = function() paste0("numeric_boxplots_", Sys.Date(), ".png"),
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
        s$show_points,
        s$show_outliers,
        s$label_var,
        s$colors,
        s$base_size,
        s$rows,
        s$cols,
        s$common_legend,
        s$legend_position %||% "",
        sep = "_"
      )
      
      if (!identical(key, cached_key())) {
        info <- isolate(plot_info())

        if (!is.null(info$warning)) {
          cached_plot(NULL)
          cached_key(key)
          return()
        }

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






build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = TRUE,
                                              show_outliers = FALSE,
                                              outlier_label_var = NULL,
                                              nrow_input = NULL,
                                              ncol_input = NULL,
                                              custom_colors = NULL,
                                              base_size = 13,
                                              common_legend = FALSE,
                                              legend_position = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)
  
  # ensure discrete x if grouped
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.factor(df[[group_var]])
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(num_vars, function(var) {
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)

    if (!is.null(group_var)) {
      group_levels <- levels(df[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +
        scale_fill_manual(values = palette) +
        theme_minimal(base_size = base_size) +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "gray30"),
          axis.ticks = element_line(color = "gray30")
        )

      needs_color_scale <- FALSE
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(
          aes(color = .data[[group_var]]),
          width = 0.2,
          alpha = 0.5,
          size = 1
        )
        needs_color_scale <- TRUE
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          group_col = group_var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y, color = group),
            inherit.aes = FALSE,
            size = 2.5,
            show.legend = FALSE
          )
          needs_color_scale <- TRUE

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label, color = group),
              inherit.aes = FALSE,
              size = 3,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }

      if (needs_color_scale) {
        p <- p + scale_color_manual(values = palette, guide = "none")
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = single_color, width = 0.3) +
        theme_minimal(base_size = base_size) +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(color = "gray30"),
          axis.ticks = element_line(color = "gray30")
        )

      if (isTRUE(show_points)) {
        p <- p + geom_jitter(color = single_color, width = 0.05, alpha = 0.5, size = 1)
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            color = single_color,
            size = 2.5,
            show.legend = FALSE
          )

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 3,
              color = single_color,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }
    }

    if (inherits(p, "gg")) p else NULL
  })
  
  # keep only valid ggplots
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  n_panels <- length(plots)
  defaults <- list(
    rows = 1L,
    cols = max(1L, as.integer(n_panels))
  )

  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols,
    max_cols = max(100L, as.integer(defaults$cols))
  )

  layout <- adjust_grid_layout(n_panels, layout)

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)

  combined <- NULL
  if (isTRUE(validation$valid)) {
    combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)

    combined <- apply_common_legend_layout(
      combined,
      legend_position = legend_position,
      collect_guides = isTRUE(common_legend)
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


has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}


compute_outlier_bounds <- function(values) {
  if (is.null(values)) return(NULL)
  values <- values[!is.na(values)]
  if (!length(values)) return(NULL)

  stats <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  if (anyNA(stats)) return(NULL)

  iqr <- stats[2] - stats[1]
  list(
    lower = stats[1] - 1.5 * iqr,
    upper = stats[2] + 1.5 * iqr
  )
}


clean_outlier_labels <- function(values) {
  if (is.null(values)) return(character())
  out <- as.character(values)
  out[is.na(out) | trimws(out) == ""] <- NA_character_
  out
}


filter_labeled_outliers <- function(outliers) {
  if (!has_rows(outliers) || !"label" %in% names(outliers)) {
    return(NULL)
  }
  labeled <- outliers[!is.na(outliers$label) & nzchar(outliers$label), , drop = FALSE]
  if (has_rows(labeled)) labeled else NULL
}


validate_outlier_label <- function(label_input) {
  if (is.null(label_input) || !nzchar(label_input)) {
    return(NULL)
  }
  label_input
}


prepare_boxplot_outliers <- function(data,
                                     value_col,
                                     group_col = NULL,
                                     label_col = NULL) {
  if (is.null(data) || !is.data.frame(data) || !value_col %in% names(data)) {
    return(NULL)
  }

  extract_labels <- function(df, idx) {
    if (is.null(label_col) || !label_col %in% names(df)) {
      return(rep(NA_character_, length(idx)))
    }
    clean_outlier_labels(df[[label_col]][idx])
  }

  if (!is.null(group_col) && group_col %in% names(data)) {
    grouped <- data
    grouped[[group_col]] <- droplevels(as.factor(grouped[[group_col]]))
    group_levels <- levels(grouped[[group_col]])
    split_data <- split(grouped, grouped[[group_col]], drop = TRUE)

    out_list <- lapply(group_levels, function(lvl) {
      subset <- split_data[[lvl]]
      if (is.null(subset)) return(NULL)

      bounds <- compute_outlier_bounds(subset[[value_col]])
      if (is.null(bounds)) return(NULL)

      idx <- which(subset[[value_col]] < bounds$lower | subset[[value_col]] > bounds$upper)
      if (!length(idx)) return(NULL)

      data.frame(
        x = factor(rep(lvl, length(idx)), levels = group_levels),
        y = subset[[value_col]][idx],
        group = factor(rep(lvl, length(idx)), levels = group_levels),
        label = extract_labels(subset, idx),
        stringsAsFactors = FALSE
      )
    })

    out_list <- Filter(has_rows, out_list)
    if (!length(out_list)) return(NULL)

    outliers <- do.call(rbind, out_list)
    rownames(outliers) <- NULL
    return(outliers)
  }

  bounds <- compute_outlier_bounds(data[[value_col]])
  if (is.null(bounds)) return(NULL)

  idx <- which(data[[value_col]] < bounds$lower | data[[value_col]] > bounds$upper)
  if (!length(idx)) return(NULL)

  data.frame(
    x = factor(rep(1, length(idx))),
    y = data[[value_col]][idx],
    group = NA,
    label = extract_labels(data, idx),
    stringsAsFactors = FALSE
  )
}

