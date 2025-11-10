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
    fluidRow(
      column(6, with_help_tooltip(
        numericInput(ns("plot_width"),  "Subplot width (px)",  200, 200, 2000, 50),
        "Control how wide each boxplot panel should be."
      )),
      column(6, with_help_tooltip(
        numericInput(ns("plot_height"), "Subplot height (px)", 800, 200, 2000, 50),
        "Control how tall each boxplot panel should be."
      ))
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of plots to display when multiple charts are shown.",
      cols_help = "Choose how many columns of plots to display when multiple charts are shown.",
      cols_max = 100L
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, base_size_ui(
        ns,
        default = 13,
        help_text = "Adjust the base font size used for boxplot text elements."
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
    
    resolve_input_value <- function(x) {
      if (is.null(x)) return(NULL)
      if (is.reactive(x)) x() else x
    }
    
    module_active <- reactive({
      if (is.null(is_active)) TRUE else isTRUE(is_active())
    })
    
    plot_width  <- reactive({ w <- input$plot_width;  if (is.null(w) || is.na(w)) 200 else as.numeric(w) })
    plot_height <- reactive({ h <- input$plot_height; if (is.null(h) || is.na(h)) 800 else as.numeric(h) })
    
    grid <- plot_grid_server("plot_grid", cols_max = 100L)
    
    color_var_reactive <- reactive({
      info <- summary_info()
      if (is.null(info)) return(NULL)
      g <- resolve_input_value(info$group_var)
      dat <- filtered_data()
      if (is.null(g) || identical(g, "") || identical(g, "None")) return(NULL)
      if (is.null(dat) || !is.data.frame(dat) || !g %in% names(dat)) return(NULL)
      g
    })
    
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = filtered_data,
      color_var_reactive = color_var_reactive,
      multi_group = TRUE
    )
    
    base_size <- base_size_server(input = input, default = 13)
    
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
      if (is.null(current) || !nzchar(current) || !current %in% cat_cols) current <- ""
      
      with_help_tooltip(
        selectInput(
          ns("outlier_label"),
          label = "Label outliers by",
          choices = c("None" = "", stats::setNames(cat_cols, cat_cols)),
          selected = current
        ),
        "Choose a column to annotate the highlighted outliers."
      )
    })
    
    state <- reactive({
      list(
        info          = summary_info(),
        dat           = filtered_data(),
        show_points   = isTRUE(input$show_points),
        show_outliers = isTRUE(input$show_outliers),
        label_var     = validate_outlier_label(input$outlier_label),
        colors        = custom_colors(),
        base_size     = base_size(),
        rows_input    = grid$rows(),
        cols_input    = grid$cols()
      )
    })
    
    plot_info <- reactive({
      req(module_active())
      s <- state()
      req(!is.null(s$info))
      processed <- resolve_input_value(s$info$processed_data)
      dat <- if (!is.null(processed)) processed else s$dat
      req(!is.null(dat), is.data.frame(dat), nrow(dat) > 0)
      
      selected_vars <- resolve_input_value(s$info$selected_vars)
      group_var     <- resolve_input_value(s$info$group_var)
      
      build_descriptive_numeric_boxplot(
        df                = dat,
        selected_vars     = selected_vars,
        group_var         = group_var,
        show_points       = s$show_points,
        show_outliers     = s$show_outliers,
        outlier_label_var = s$label_var,
        nrow_input        = s$rows_input,
        ncol_input        = s$cols_input,
        custom_colors     = s$colors,
        base_size         = s$base_size
      )
    })
    
    size_val <- reactiveVal(list(w = 200, h = 800))
    
    observeEvent(plot_info(), {
      req(module_active())
      info <- plot_info()
      lay <- info$layout
      nrow_l <- if (is.null(lay) || is.null(lay$nrow) || is.na(lay$nrow)) 1L else as.integer(lay$nrow)
      ncol_l <- if (is.null(lay) || is.null(lay$ncol) || is.na(lay$ncol)) 1L else as.integer(lay$ncol)
      size_val(list(w = plot_width() * ncol_l, h = plot_height() * nrow_l))
    }, ignoreInit = FALSE)
    
    output$grid_warning <- renderUI({
      req(module_active())
      info <- plot_info()
      if (!is.null(info$warning)) div(class = "alert alert-warning", info$warning) else NULL
    })
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_boxplots_", Sys.Date(), ".png"),
      content = function(file) {
        req(module_active())
        info <- plot_info()
        req(is.null(info$warning), !is.null(info$plot))
        s <- size_val()
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
    width = function() { size_val()$w },
    height = function() { size_val()$h },
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
                                              custom_colors = NULL,
                                              base_size = 13) {
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
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

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
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

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

