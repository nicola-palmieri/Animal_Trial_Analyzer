# ===============================================================
# 🟦 Descriptive Visualization — Numeric Boxplots
# ===============================================================

visualize_numeric_boxplots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    checkboxInput(ns("show_points"), "Show individual data points", TRUE),
    checkboxInput(ns("show_outliers"), "Highlight boxplot outliers", FALSE),
    conditionalPanel(
      condition = sprintf("input['%s']", ns("show_outliers")),
      uiOutput(ns("outlier_label_ui"))
    ),
    fluidRow(
      column(6, numericInput(ns("plot_width"),  "Subplot width (px)",  200, 200, 2000, 50)),
      column(6, numericInput(ns("plot_height"), "Subplot height (px)", 800, 200, 2000, 50))
    ),
    fluidRow(
      column(
        6,
        numericInput(
          ns("resp_rows"),
          "Grid rows",
          value = NA,
          min = 1,
          max = 10,
          step = 1
        )
      ),
      column(
        6,
        numericInput(
          ns("resp_cols"),
          "Grid columns",
          value = NA,
          min = 1,
          max = 100,
          step = 1
        )
      )
    ),
    add_color_customization_ui(ns, multi_group = TRUE),
    hr(),
    downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;")
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

    output$outlier_label_ui <- renderUI({
      dat <- filtered_data()
      cat_cols <- character(0)
      if (!is.null(dat) && is.data.frame(dat)) {
        cat_cols <- names(dat)[vapply(
          dat,
          function(x) is.character(x) || is.factor(x) || is.logical(x),
          logical(1)
        )]
        cat_cols <- sort(unique(cat_cols))
      }

      current <- isolate(input$outlier_label)
      if (is.null(current) || !nzchar(current) || !current %in% cat_cols) {
        current <- ""
      }

      selectInput(
        ns("outlier_label"),
        label = "Label outliers by:",
        choices = c("None" = "", stats::setNames(cat_cols, cat_cols)),
        selected = current
      )
    })

    cached_plot_info <- reactiveVal(NULL)
    cache_ready <- reactiveVal(FALSE)

    invalidate_cache <- function() {
      cached_plot_info(NULL)
      cache_ready(FALSE)
    }

    observeEvent(
      list(
        summary_info(),
        filtered_data(),
        input$show_points,
        input$show_outliers,
        input$outlier_label,
        input$resp_rows,
        input$resp_cols,
        custom_colors()
      ),
      {
        invalidate_cache()
      },
      ignoreNULL = FALSE
    )

    compute_plot_info <- function() {
      info <- summary_info()

      validate(need(!is.null(info), "Summary not available."))

      processed <- resolve_input_value(info$processed_data)
      dat <- if (!is.null(processed)) processed else filtered_data()

      validate(need(!is.null(dat) && is.data.frame(dat) && nrow(dat) > 0, "No data available."))

      selected_vars <- resolve_input_value(info$selected_vars)
      group_var     <- resolve_input_value(info$group_var)

      out <- build_descriptive_numeric_boxplot(
        df = dat,
        selected_vars = selected_vars,
        group_var = group_var,
        show_points = isTRUE(input$show_points),
        show_outliers = isTRUE(input$show_outliers),
        outlier_label_var = validate_outlier_label(input$outlier_label),
        nrow_input = input$resp_rows,
        ncol_input = input$resp_cols,
        custom_colors = custom_colors()
      )

      validate(need(!is.null(out), "No numeric variables available for plotting."))
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

    observeEvent(plot_info(), {
      info <- plot_info()
      if (is.null(info) || is.null(info$defaults)) return()

      rows <- info$defaults$rows
      cols <- info$defaults$cols
      if (is.null(rows) || is.null(cols)) return()

      sync_numeric_input(session, "resp_rows", input$resp_rows, rows)
      sync_numeric_input(session, "resp_cols", input$resp_cols, cols)
    }, ignoreNULL = FALSE)

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
      filename = function() paste0("numeric_boxplots_", Sys.Date(), ".png"),
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


build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = TRUE,
                                              show_outliers = FALSE,
                                              outlier_label_var = NULL,
                                              nrow_input = NULL,
                                              ncol_input = NULL,
                                              custom_colors = NULL) {
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
    # skip all-NA vars early
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)
    
    if (!is.null(group_var)) {
      group_levels <- levels(df[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +
        scale_fill_manual(values = palette) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      added_color_scale <- FALSE
      if (isTRUE(show_points)) {
        p <- p +
          geom_jitter(aes(color = .data[[group_var]]), width = 0.2, alpha = 0.5, size = 1) +
          scale_color_manual(values = palette, guide = "none")
        added_color_scale <- TRUE
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          group_col = group_var,
          label_col = outlier_label_var
        )
        if (!is.null(outliers) && nrow(outliers) > 0) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y, color = group),
            inherit.aes = FALSE,
            size = 2.5,
            show.legend = FALSE
          )
          if (!added_color_scale) {
            p <- p + scale_color_manual(values = palette, guide = "none")
            added_color_scale <- TRUE
          }
          label_data <- outliers[!is.na(outliers$label) & nzchar(outliers$label), , drop = FALSE]
          if (nrow(label_data) > 0) {
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
    } else {
      # ✅ always provide an x aesthetic
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = resolve_single_color(custom_colors), width = 0.3) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(color = resolve_single_color(custom_colors), width = 0.05, alpha = 0.5, size = 1)
      }
      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          label_col = outlier_label_var
        )
        if (!is.null(outliers) && nrow(outliers) > 0) {
          single_color <- resolve_single_color(custom_colors)
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            color = single_color,
            size = 2.5,
            show.legend = FALSE
          )
          label_data <- outliers[!is.na(outliers$label) & nzchar(outliers$label), , drop = FALSE]
          if (nrow(label_data) > 0) {
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

  clean_labels <- function(values) {
    if (is.null(values)) return(rep(NA_character_, length.out = 0))
    out <- as.character(values)
    out[is.na(out) | trimws(out) == ""] <- NA_character_
    out
  }

  if (!is.null(group_col) && group_col %in% names(data)) {
    grouped <- data
    grouped[[group_col]] <- droplevels(as.factor(grouped[[group_col]]))
    group_levels <- levels(grouped[[group_col]])

    out_list <- lapply(group_levels, function(lvl) {
      subset <- grouped[grouped[[group_col]] == lvl, , drop = FALSE]
      values <- subset[[value_col]]
      values <- values[!is.na(values)]
      if (length(values) == 0) return(NULL)
      stats <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
      if (anyNA(stats)) return(NULL)
      iqr <- stats[2] - stats[1]
      lower <- stats[1] - 1.5 * iqr
      upper <- stats[2] + 1.5 * iqr
      idx <- which(subset[[value_col]] < lower | subset[[value_col]] > upper)
      if (length(idx) == 0) return(NULL)

      labels <- if (!is.null(label_col) && label_col %in% names(subset)) {
        clean_labels(subset[[label_col]][idx])
      } else {
        rep(NA_character_, length(idx))
      }

      data.frame(
        x = factor(rep(lvl, length(idx)), levels = group_levels),
        y = subset[[value_col]][idx],
        group = factor(rep(lvl, length(idx)), levels = group_levels),
        label = labels,
        stringsAsFactors = FALSE
      )
    })

    out_list <- Filter(Negate(is.null), out_list)
    if (length(out_list) == 0) {
      return(NULL)
    }
    outliers <- do.call(rbind, out_list)
    rownames(outliers) <- NULL
    return(outliers)
  }

  values <- data[[value_col]]
  values <- values[!is.na(values)]
  if (length(values) == 0) return(NULL)
  stats <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  if (anyNA(stats)) return(NULL)
  iqr <- stats[2] - stats[1]
  lower <- stats[1] - 1.5 * iqr
  upper <- stats[2] + 1.5 * iqr
  idx <- which(data[[value_col]] < lower | data[[value_col]] > upper)
  if (length(idx) == 0) return(NULL)

  labels <- if (!is.null(label_col) && label_col %in% names(data)) {
    clean_values <- clean_labels(data[[label_col]][idx])
    clean_values
  } else {
    rep(NA_character_, length(idx))
  }

  data.frame(
    x = factor(rep(1, length(idx))),
    y = data[[value_col]][idx],
    group = NA,
    label = labels,
    stringsAsFactors = FALSE
  )
}

