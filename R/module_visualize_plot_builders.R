# ===============================================================
# 🎨 Visualization Plot Builders
# ===============================================================

build_descriptive_plots <- function(summary_info, original_data = NULL) {
  summary_data <- resolve_summary_input(summary_info)

  plots <- list()

  metric_plots <- build_descriptive_metric_plots(summary_data)
  if (length(metric_plots) > 0) {
    plots$metrics <- metric_plots
  }

  if (!is.null(original_data)) {
    factor_plot <- build_descriptive_categorical_plot(original_data)
    if (!is.null(factor_plot)) {
      plots$factors <- factor_plot
    }

    box_plot <- build_descriptive_boxplot(original_data)
    if (!is.null(box_plot)) {
      plots$boxplots <- box_plot
    }

    hist_plot <- build_descriptive_histogram(original_data)
    if (!is.null(hist_plot)) {
      plots$histograms <- hist_plot
    }
  }

  if (length(plots) == 0) return(NULL)

  plots
}


resolve_summary_input <- function(summary_info) {
  if (is.null(summary_info)) return(NULL)
  if (is.function(summary_info)) {
    return(summary_info())
  }
  summary_info
}


build_descriptive_metric_plots <- function(summary_data) {
  if (is.null(summary_data)) return(list())

  metric_plots <- list(
    cv = build_descriptive_metric_panel(summary_data$cv, "cv", "CV (%)"),
    outliers = build_descriptive_metric_panel(summary_data$outliers, "outliers", "Outlier Count"),
    missing = build_descriptive_metric_panel(summary_data$missing, "missing", "Missing (%)"),
    shapiro = build_descriptive_metric_panel(summary_data$shapiro, "shapiro", "Shapiro p-value")
  )

  Filter(Negate(is.null), metric_plots)
}


build_descriptive_metric_panel <- function(df, prefix, y_label) {
  tidy <- tidy_descriptive_metric(df, prefix)
  if (is.null(tidy)) return(NULL)

  build_descriptive_metric_bar_plot(tidy, y_label)
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


build_descriptive_metric_bar_plot <- function(info, y_label) {
  if (is.null(info)) return(NULL)
  df <- info$data
  has_multiple_groups <- length(unique(df$.group)) > 1
  primary_color <- get_primary_color()
  p <- ggplot(df, aes(x = variable, y = value))
  if (has_multiple_groups) {
    legend_title <- if (!is.null(info$group_label)) info$group_label else "Group"
    group_colors <- get_palette_for_values(df$.group)
    p <- p +
      geom_col(aes(fill = .group), position = position_dodge(width = 0.75), width = 0.65) +
      labs(fill = legend_title)
    if (length(group_colors) > 0) {
      p <- p + scale_fill_manual(values = group_colors)
    }
  } else {
    p <- p + geom_col(fill = primary_color, width = 0.65)
  }
  p +
    theme_minimal(base_size = 13) +
    labs(x = NULL, y = y_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


build_descriptive_categorical_plot <- function(df,
                                               selected_vars = NULL,
                                               group_var = NULL,
                                               strata_levels = NULL,
                                               show_proportions = FALSE,
                                               nrow_input = NULL,
                                               ncol_input = NULL) {
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

  primary_color <- get_primary_color()

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

      group_colors <- get_palette_for_values(count_df[[group_col]])

      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value, fill = .data[[group_col]])) +
        geom_col(position = position_dodge(width = 0.75), width = 0.65) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = y_label, fill = group_col) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      if (isTRUE(show_proportions)) {
        p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))
      }

      if (length(group_colors) > 0) {
        p <- p + scale_fill_manual(values = group_colors)
      }

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
      
      p <- ggplot(count_df, aes(x = .data[[var]], y = .data$value)) +
        geom_col(fill = primary_color, width = 0.65) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = y_label) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      if (isTRUE(show_proportions)) {
        p <- p + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1))
      }
      
      p
    }
  })
  
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  # ✅ Use the common layout helper to arrange plots using the requested grid
  layout <- resolve_grid_layout(
    n_items   = length(plots),
    rows_input = suppressWarnings(as.numeric(nrow_input)),
    cols_input = suppressWarnings(as.numeric(ncol_input))
  )
  
  combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)
  
  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = length(plots)
  )
}


build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = TRUE,
                                              nrow_input = NULL,
                                              ncol_input = NULL) {
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
  
  primary_color <- get_primary_color()

  plots <- lapply(num_vars, function(var) {
    # skip all-NA vars early
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)
    
    if (!is.null(group_var)) {
      group_colors <- get_palette_for_values(df[[group_var]])
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6, color = NA) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      if (length(group_colors) > 0) {
        p <- p + scale_fill_manual(values = group_colors)
      }
      if (isTRUE(show_points)) {
        p <- p +
          geom_jitter(
            aes(color = .data[[group_var]]),
            width = 0.2,
            alpha = 0.5,
            size = 1,
            show.legend = FALSE
          )
        if (length(group_colors) > 0) {
          p <- p + scale_color_manual(values = group_colors, guide = "none")
        }
      }
    } else {
      # ✅ always provide an x aesthetic
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = primary_color, color = primary_color, width = 0.3) +
        theme_minimal(base_size = 13) +
        labs(title = var, x = NULL, y = var) +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(width = 0.05, alpha = 0.5, size = 1, color = primary_color)
      }
    }
    
    if (inherits(p, "gg")) p else NULL
  })
  
  # keep only valid ggplots
  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)
  
  layout <- resolve_grid_layout(
    n_items = length(plots),
    rows_input = suppressWarnings(as.numeric(nrow_input)),
    cols_input = suppressWarnings(as.numeric(ncol_input))
  )
  
  combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)
  
  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = length(plots)
  )
}


build_descriptive_numeric_histogram <- function(df,
                                                selected_vars = NULL,
                                                group_var = NULL,
                                                strata_levels = NULL,
                                                use_density = FALSE,
                                                nrow_input = NULL,
                                                ncol_input = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)

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

  plots <- lapply(num_vars, function(var) {
    cols <- c(var, group_var)
    cols <- cols[cols %in% names(df)]
    plot_data <- df[, cols, drop = FALSE]

    keep <- is.finite(plot_data[[var]])
    keep[is.na(keep)] <- FALSE
    plot_data <- plot_data[keep, , drop = FALSE]
    if (nrow(plot_data) == 0) return(NULL)

    if (!is.null(group_var)) {
      plot_data[[group_var]] <- droplevels(plot_data[[group_var]])
      group_colors <- get_palette_for_values(plot_data[[group_var]])
    }

    density_mode <- isTRUE(use_density) && length(unique(plot_data[[var]])) > 1

    base <- ggplot(plot_data, aes(x = .data[[var]]))
    y_label <- if (density_mode) "Density" else "Count"

    if (!is.null(group_var)) {
      if (density_mode) {
        p <- base +
          geom_density(aes(color = .data[[group_var]], fill = .data[[group_var]]), alpha = 0.3) +
          labs(color = group_var, fill = group_var)
        if (length(group_colors) > 0) {
          p <- p +
            scale_color_manual(values = group_colors) +
            scale_fill_manual(values = group_colors)
        }
      } else {
        p <- base +
          geom_histogram(
            aes(fill = .data[[group_var]]),
            position = "identity",
            alpha = 0.5,
            bins = 30,
            color = NA
          ) +
          labs(fill = group_var)
        if (length(group_colors) > 0) {
          p <- p + scale_fill_manual(values = group_colors)
        }
      }
    } else {
      if (density_mode) {
        p <- base + geom_density(fill = primary_color, color = primary_color, alpha = 0.35)
      } else {
        p <- base + geom_histogram(fill = primary_color, color = primary_color, bins = 30)
      }
    }

    p +
      theme_minimal(base_size = 13) +
      labs(title = var, x = var, y = y_label)
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  layout <- resolve_grid_layout(
    n_items = length(plots),
    rows_input = suppressWarnings(as.numeric(nrow_input)),
    cols_input = suppressWarnings(as.numeric(ncol_input))
  )

  combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = length(plots)
  )
}


build_descriptive_histogram <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  if (length(num_vars) == 0) return(NULL)
  primary_color <- get_primary_color()

  plots <- lapply(num_vars, function(v) {
    ggplot(df, aes(x = .data[[v]])) +
      geom_histogram(fill = primary_color, color = primary_color, bins = 20) +
      theme_minimal(base_size = 13) +
      labs(title = paste(v, "Distribution"), x = NULL, y = "Frequency")
  })
  patchwork::wrap_plots(plots, ncol = 2)
}


build_anova_plot_info <- function(data, info, effective_input, line_colors = NULL) {
  factor1 <- info$factors$factor1
  factor2 <- info$factors$factor2
  order1 <- info$orders$order1
  order2 <- info$orders$order2
  primary_color <- get_primary_color()

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

  if (is.null(factor2)) {
    if (is.null(line_colors) || length(line_colors) == 0) {
      line_colors <- get_palette_for_n(1L)
    }
  } else {
    default_colors <- get_palette_for_values(data[[factor2]])
    if (is.null(line_colors) || length(line_colors) == 0) {
      line_colors <- default_colors
    } else {
      if (!is.null(names(line_colors)) && length(names(line_colors)) > 0) {
        matched <- line_colors[names(line_colors) %in% names(default_colors)]
        if (length(matched) > 0) {
          line_colors <- matched
        } else {
          line_colors <- default_colors
        }
      } else {
        line_colors <- default_colors
      }
    }
  }

  build_plot <- function(stats_df, title_text, y_limits) {
    single_col <- if (!is.null(line_colors) && length(line_colors) >= 1) line_colors[1] else primary_color

    if (is.null(factor2)) {
      p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
        geom_line(aes(group = 1), color = single_col, linewidth = 1) +
        geom_point(size = 3, color = single_col) +
        geom_errorbar(aes(ymin = mean - se, ymax = mean + se),
                      width = 0.15, color = single_col) +
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
      
      if (!is.null(line_colors) && length(line_colors) >= 1) {
        p <- p + scale_color_manual(values = line_colors)
      }
    }
    
    if (!is.null(y_limits) && all(is.finite(y_limits))) {
      p <- p + scale_y_continuous(limits = y_limits)
    }
    
    p
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
        build_plot(stratum_plots[[stratum_name]], y_limits)
      })

      layout <- resolve_grid_layout(
        n_items = length(strata_plot_list),
        rows_input = effective_input("strata_rows"),
        cols_input = effective_input("strata_cols")
      )

      max_strata_rows <- max(max_strata_rows, layout$nrow)
      max_strata_cols <- max(max_strata_cols, layout$ncol)

      combined <- patchwork::wrap_plots(
        plotlist = strata_plot_list,
        nrow = layout$nrow,
        ncol = layout$ncol
      )

      response_plots[[resp]] <- combined

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

      response_plots[[resp]] <- build_plot(stats_df, y_limits)
      max_strata_rows <- max(max_strata_rows, 1)
      max_strata_cols <- max(max_strata_cols, 1)
    }
  }

  if (length(response_plots) == 0) {
    return(NULL)
  }

  resp_layout <- resolve_grid_layout(
    n_items = length(response_plots),
    rows_input = effective_input("resp_rows"),
    cols_input = effective_input("resp_cols")
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
  palette_color <- get_primary_color()

  GGally::ggpairs(
    data,
    progress = FALSE,
    upper = list(
      continuous = GGally::wrap("cor", size = 4, color = palette_color)
    ),
    lower = list(
      continuous = GGally::wrap("points", alpha = 0.6, color = palette_color, size = 1.5)
    ),
    diag = list(
      continuous = GGally::wrap("densityDiag", fill = palette_color, alpha = 0.4)
    )
  ) +
    theme_minimal(base_size = 11) +
    theme(
      strip.background = element_rect(fill = "gray95", color = NA),
      strip.text = element_text(face = "bold", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
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

  primary_color <- get_primary_color()
  
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
      color = if (is.null(color_var)) primary_color else NULL
    ) +
    theme_minimal(base_size = 14) +
    labs(
      x = "PC1",
      y = "PC2",
      color = if (!is.null(color_var)) color_var else NULL,
      shape = if (!is.null(shape_var)) shape_var else NULL
    ) +
    theme(
      legend.position = "right"
    )

  if (!is.null(color_var)) {
    color_values <- get_palette_for_values(plot_data[[color_var]])
    if (length(color_values) > 0) {
      g <- g + scale_color_manual(values = color_values)
    }
  }
  
  if (!is.null(label_var)) {
    g <- g + ggrepel::geom_text_repel(
      aes(label = label_value),
      size = label_size,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2,
      na.rm = TRUE,
      color = if (is.null(color_var)) primary_color else NULL
    )
  }
  
  g
}


# ---------------------------------------------------------------
# Low-level utilities
# ---------------------------------------------------------------

resolve_grid_value <- function(value) {
  if (is.null(value) || length(value) == 0) return(NA_integer_)
  val <- suppressWarnings(as.integer(value[1]))
  if (is.na(val) || val < 1) return(NA_integer_)
  val
}

resolve_grid_layout <- function(n_items, rows_input = NULL, cols_input = NULL) {
  n_items <- suppressWarnings(as.integer(n_items[1]))
  if (is.na(n_items) || n_items <= 0) {
    n_items <- 1L
  }

  rows_raw <- resolve_grid_value(rows_input)
  cols_raw <- resolve_grid_value(cols_input)

  rows <- rows_raw
  cols <- cols_raw

  if (is.na(rows) && is.na(cols)) {
    rows <- ceiling(sqrt(n_items))
    cols <- ceiling(n_items / rows)
  } else if (is.na(rows)) {
    cols <- cols_raw
    if (is.na(cols) || cols <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      rows <- ceiling(n_items / cols)
    }
  } else if (is.na(cols)) {
    rows <- rows_raw
    if (is.na(rows) || rows <= 0) {
      rows <- ceiling(sqrt(n_items))
      cols <- ceiling(n_items / rows)
    } else {
      cols <- ceiling(n_items / rows)
    }
  }

  if ((is.na(rows_raw) || is.na(cols_raw)) && !is.na(rows) && !is.na(cols)) {
    while (rows * cols < n_items) {
      if (cols <= rows) {
        cols <- cols + 1L
      } else {
        rows <- rows + 1L
      }
    }
  }

  list(nrow = rows, ncol = cols)
}

