# ===============================================================
# 🎨 Visualization Plot Builders
# ===============================================================

build_descriptive_plots <- function(summary_info, original_data = NULL) {
  data <- summary_info()
  
  # ------------------------------------------------------------
  # 1️⃣ Helper: Tidy metric tables (CV, outliers, missing, shapiro)
  # ------------------------------------------------------------
  tidy_metric <- function(df, prefix) {
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
  
  # ------------------------------------------------------------
  # 2️⃣ Helper: Generic barplot for summary metrics
  # ------------------------------------------------------------
  build_bar_plot <- function(info, y_label) {
    if (is.null(info)) return(NULL)
    df <- info$data
    has_multiple_groups <- length(unique(df$.group)) > 1
    p <- ggplot(df, aes(x = variable, y = value))
    if (has_multiple_groups) {
      legend_title <- if (!is.null(info$group_label)) info$group_label else "Group"
      p <- p +
        geom_col(aes(fill = .group), position = position_dodge(width = 0.75), width = 0.65) +
        labs(fill = legend_title)
    } else {
      p <- p + geom_col(fill = "#2C7FB8", width = 0.65)
    }
    p +
      theme_minimal(base_size = 13) +
      labs(x = NULL, y = y_label) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  # ------------------------------------------------------------
  # 3️⃣ Add: Barplots for categorical variables
  # ------------------------------------------------------------
  build_factor_barplots <- function(df) {
    factor_vars <- names(df)[sapply(df, function(x) is.character(x) || is.factor(x))]
    if (length(factor_vars) == 0) return(NULL)
    plots <- lapply(factor_vars, function(v) {
      ggplot(df, aes(x = .data[[v]])) +
        geom_bar(fill = "#2C7FB8", width = 0.7) +
        theme_minimal(base_size = 13) +
        labs(title = v, x = NULL, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
    patchwork::wrap_plots(plots, ncol = 2)
  }
  
  # ------------------------------------------------------------
  # 4️⃣ Add: Box + Jitter plots for numeric variables
  # ------------------------------------------------------------
  build_jitter_boxplots <- function(df) {
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) == 0) return(NULL)
    plots <- lapply(num_vars, function(v) {
      ggplot(df, aes(x = 1, y = .data[[v]])) +
        geom_boxplot(outlier.shape = NA, fill = "#A6CEE3") +
        geom_jitter(width = 0.1, alpha = 0.5, color = "#1F78B4") +
        theme_minimal(base_size = 13) +
        labs(title = v, x = NULL, y = "Value") +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
    })
    patchwork::wrap_plots(plots, ncol = 2)
  }
  
  # ------------------------------------------------------------
  # 5️⃣ Add: Histograms for numeric variables
  # ------------------------------------------------------------
  build_histograms <- function(df) {
    num_vars <- names(df)[sapply(df, is.numeric)]
    if (length(num_vars) == 0) return(NULL)
    plots <- lapply(num_vars, function(v) {
      ggplot(df, aes(x = .data[[v]])) +
        geom_histogram(fill = "#FDBF6F", color = "white", bins = 20) +
        theme_minimal(base_size = 13) +
        labs(title = paste(v, "Distribution"), x = NULL, y = "Frequency")
    })
    patchwork::wrap_plots(plots, ncol = 2)
  }
  
  # ------------------------------------------------------------
  # 6️⃣ Combine all subplots together
  # ------------------------------------------------------------
  metric_plots <- list(
    cv = build_bar_plot(tidy_metric(data$cv, "cv"), "CV (%)"),
    outliers = build_bar_plot(tidy_metric(data$outliers, "outliers"), "Outlier Count"),
    missing = build_bar_plot(tidy_metric(data$missing, "missing"), "Missing (%)"),
    shapiro = build_bar_plot(tidy_metric(data$shapiro, "shapiro"), "Shapiro p-value")
  )
  
  metric_plots <- Filter(Negate(is.null), metric_plots)
  
  descriptive_plots <- list()

  if (length(metric_plots) > 0) {
    descriptive_plots$metrics <-
      patchwork::wrap_plots(metric_plots, ncol = 1) +
      patchwork::plot_annotation(
        title = "Summary Metrics",
        theme = theme(plot.title = element_text(size = 16, face = "bold"))
      )
  }

  if (!is.null(original_data)) {
    factor_plot <- build_factor_barplots(original_data)
    if (!is.null(factor_plot)) {
      descriptive_plots$factors <- factor_plot +
        patchwork::plot_annotation(
          title = "Categorical Distributions",
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )
    }

    box_plot <- build_jitter_boxplots(original_data)
    if (!is.null(box_plot)) {
      descriptive_plots$boxplots <- box_plot +
        patchwork::plot_annotation(
          title = "Boxplots",
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )
    }

    hist_plot <- build_histograms(original_data)
    if (!is.null(hist_plot)) {
      descriptive_plots$histograms <- hist_plot +
        patchwork::plot_annotation(
          title = "Histograms",
          theme = theme(plot.title = element_text(size = 16, face = "bold"))
        )
    }
  }

  if (length(descriptive_plots) == 0) return(NULL)

  descriptive_plots
}


build_anova_plot_info <- function(data, info, effective_input) {
  factor1 <- info$factors$factor1
  factor2 <- info$factors$factor2
  order1 <- info$orders$order1
  order2 <- info$orders$order2

  if (!is.null(factor1) && !is.null(order1)) {
    data[[factor1]] <- factor(data[[factor1]], levels = order1)
  }
  if (!is.null(factor2) && !is.null(order2)) {
    data[[factor2]] <- factor(data[[factor2]], levels = order2)
  }

  responses <- info$responses
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else character(0)
  if (has_strata && (is.null(strata_levels) || length(strata_levels) == 0)) {
    strata_levels <- unique(as.character(stats::na.omit(data[[strat_var]])))
  }

  response_plots <- list()
  max_strata_rows <- 1
  max_strata_cols <- 1

  compute_stats <- function(df_subset, resp_name) {
    if (is.null(factor2)) {
      df_subset |>
        dplyr::group_by(.data[[factor1]]) |>
        dplyr::summarise(
          mean = mean(.data[[resp_name]], na.rm = TRUE),
          se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
          .groups = "drop"
        )
    } else {
      df_subset |>
        dplyr::group_by(.data[[factor1]], .data[[factor2]]) |>
        dplyr::summarise(
          mean = mean(.data[[resp_name]], na.rm = TRUE),
          se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
          .groups = "drop"
        )
    }
  }

  build_plot <- function(stats_df, title_text, y_limits) {
    if (is.null(factor2)) {
      p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
        geom_line(aes(group = 1), color = "steelblue", linewidth = 1) +
        geom_point(size = 3, color = "steelblue") +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.15, color = "gray40") +
        theme_minimal(base_size = 14) +
        labs(x = factor1, y = "Mean ± SE") +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
    } else {
      p <- ggplot(stats_df, aes(
        x = !!sym(factor1),
        y = mean,
        color = !!sym(factor2),
        group = !!sym(factor2)
      )) +
        geom_line(linewidth = 1) +
        geom_point(size = 3) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.15) +
        theme_minimal(base_size = 14) +
        labs(
          x = factor1,
          y = "Mean ± SE",
          color = factor2
        ) +
        theme(
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank()
        )
    }

    if (!is.null(y_limits) && all(is.finite(y_limits))) {
      p <- p + scale_y_continuous(limits = y_limits)
    }

    p + ggtitle(title_text) +
      theme(plot.title = element_text(size = 12, face = "bold"))
  }

  for (resp in responses) {
    if (has_strata) {
      stratum_plots <- list()
      y_values <- c()

      for (stratum in strata_levels) {
        subset_data <- data[!is.na(data[[strat_var]]) & data[[strat_var]] == stratum, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- compute_stats(subset_data, resp)
        if (nrow(stats_df) == 0) next

        y_values <- c(y_values, stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
        stratum_plots[[stratum]] <- stats_df
      }

      if (length(stratum_plots) == 0) next

      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) y_limits <- NULL

      strata_plot_list <- lapply(names(stratum_plots), function(stratum_name) {
        build_plot(stratum_plots[[stratum_name]], stratum_name, y_limits)
      })

      layout <- compute_grid_layout(
        length(strata_plot_list),
        effective_input("strata_rows"),
        effective_input("strata_cols")
      )

      max_strata_rows <- max(max_strata_rows, layout$nrow)
      max_strata_cols <- max(max_strata_cols, layout$ncol)

      combined <- patchwork::wrap_plots(
        plotlist = strata_plot_list,
        nrow = layout$nrow,
        ncol = layout$ncol
      )

      title_plot <- ggplot() +
        theme_void() +
        ggtitle(resp) +
        theme(
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          plot.margin = margin(t = 0, r = 0, b = 6, l = 0)
        )

      response_plots[[resp]] <- title_plot / combined + plot_layout(heights = c(0.08, 1))

    } else {
      stats_df <- compute_stats(data, resp)
      if (nrow(stats_df) == 0) {
        next
      }

      y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }

      response_plots[[resp]] <- build_plot(stats_df, resp, y_limits)
      max_strata_rows <- max(max_strata_rows, 1)
      max_strata_cols <- max(max_strata_cols, 1)
    }
  }

  if (length(response_plots) == 0) {
    return(NULL)
  }

  resp_layout <- compute_grid_layout(
    length(response_plots),
    effective_input("resp_rows"),
    effective_input("resp_cols")
  )

  final_plot <- if (length(response_plots) == 1) {
    response_plots[[1]]
  } else {
    patchwork::wrap_plots(
      plotlist = response_plots,
      nrow = resp_layout$nrow,
      ncol = resp_layout$ncol
    ) &
      patchwork::plot_layout(guides = "collect")
  }

  list(
    plot = final_plot,
    layout = list(
      strata = list(rows = max_strata_rows, cols = max_strata_cols),
      responses = resp_layout
    ),
    has_strata = has_strata,
    n_responses = length(response_plots)
  )
}

build_ggpairs_plot <- function(data) {
  GGally::ggpairs(
    data,
    progress = FALSE,
    upper = list(
      continuous = GGally::wrap("cor", size = 4, color = "steelblue")
    ),
    lower = list(
      continuous = GGally::wrap("points", alpha = 0.6, color = "steelblue", size = 1.5)
    ),
    diag = list(
      continuous = GGally::wrap("densityDiag", fill = "steelblue", alpha = 0.4)
    )
  ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(color = "black")
    )
}


build_pca_biplot <- function(pca_obj, data, color_var = NULL, shape_var = NULL,
                             label_var = NULL, label_size = 2) {
  stopifnot(!is.null(pca_obj$x))
  
  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")
  
  if (!is.null(data) && nrow(data) == nrow(scores)) {
    plot_data <- cbind(scores, data)
  } else {
    plot_data <- scores
  }
  
  if (!is.null(label_var) && !identical(label_var, "") && !is.null(plot_data[[label_var]])) {
    label_values <- as.character(plot_data[[label_var]])
    label_values[is.na(label_values) | trimws(label_values) == ""] <- NA_character_
    
    if (any(!is.na(label_values))) {
      plot_data$label_value <- label_values
    } else {
      label_var <- NULL
    }
  } else {
    label_var <- NULL
  }
  
  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))
  
  g <- ggplot(plot_data, aes_mapping) +
    geom_point(
      size = 3,
      shape = if (is.null(shape_var)) 16 else NULL,
      color = if (is.null(color_var)) "black" else NULL
    ) +
    theme_minimal(base_size = 14) +
    labs(
      title = "PCA Biplot",
      x = "PC1",
      y = "PC2",
      color = if (!is.null(color_var)) color_var else NULL,
      shape = if (!is.null(shape_var)) shape_var else NULL
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      legend.position = "right"
    )
  
  if (!is.null(label_var)) {
    g <- g + ggrepel::geom_text_repel(
      aes(label = label_value),
      size = label_size,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2,
      na.rm = TRUE
    )
  }
  
  g
}


# ---------------------------------------------------------------
# Low-level utilities
# ---------------------------------------------------------------

compute_grid_layout <- function(n_items, rows_input, cols_input) {
  # Safely handle nulls
  if (is.null(n_items) || length(n_items) == 0 || is.na(n_items) || n_items <= 0) {
    return(list(nrow = 1, ncol = 1))
  }
  
  # Replace NULL or NA inputs with 0
  if (is.null(rows_input) || is.na(rows_input)) rows_input <- 0
  if (is.null(cols_input) || is.na(cols_input)) cols_input <- 0
  
  n_row_input <- suppressWarnings(as.numeric(rows_input))
  n_col_input <- suppressWarnings(as.numeric(cols_input))
  
  # Handle invalid inputs
  if (is.na(n_row_input)) n_row_input <- 0
  if (is.na(n_col_input)) n_col_input <- 0
  
  if (n_row_input > 0) {
    n_row_final <- n_row_input
    if (n_col_input > 0) {
      n_col_final <- max(n_col_input, ceiling(n_items / max(1, n_row_final)))
    } else {
      n_col_final <- ceiling(n_items / max(1, n_row_final))
    }
  } else if (n_col_input > 0) {
    n_col_final <- n_col_input
    n_row_final <- ceiling(n_items / max(1, n_col_final))
  } else {
    # Default heuristic: single row if <=5 items, otherwise two
    n_row_final <- ifelse(n_items <= 5, 1, 2)
    n_col_final <- ceiling(n_items / n_row_final)
  }
  
  list(
    nrow = max(1, as.integer(n_row_final)),
    ncol = max(1, as.integer(n_col_final))
  )
}

