#### Section: Barplot Construction ####

plot_anova_barplot_meanse <- function(data,
                                      info,
                                      layout_values = list(),
                                      line_colors = NULL,
                                      show_value_labels = FALSE,
                                      base_size = 14,
                                      posthoc_all = NULL,
                                      share_y_axis = FALSE,
                                      common_legend = FALSE,
                                      legend_position = NULL) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  allowed_positions <- c("bottom", "top", "left", "right")
  legend_position_value <- if (!is.null(legend_position) && legend_position %in% allowed_positions) {
    legend_position
  } else {
    "bottom"
  }

  if (is.null(factor1) || length(context$responses) == 0) {
    return(NULL)
  }

  shared_y_limits <- if (isTRUE(share_y_axis)) {
    compute_barplot_shared_limits(
      context,
      data,
      factor1,
      factor2,
      posthoc_all,
      show_value_labels
    )
  } else {
    NULL
  }

  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }
  
  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels
  
  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }
    
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_plots <- list()
      
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next
        
        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        
        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }
        
        stratum_plots[[stratum]] <- build_bar_plot_panel(
          stats_df = stats_df,
          title_text = stratum,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_fill = base_fill,
          show_value_labels = show_value_labels,
          base_size = base_size,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc,
          y_limits = shared_y_limits
        )
      }
      
      if (length(stratum_plots) > 0) {
        strata_panel_count <- max(strata_panel_count, length(stratum_plots))
        current_layout <- adjust_grid_layout(length(stratum_plots), context$strata_layout)
        combined <- patchwork::wrap_plots(
          plotlist = stratum_plots,
          nrow = current_layout$nrow,
          ncol = current_layout$ncol
        )

        title_plot <- ggplot() +
          ta_plot_theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = base_size, face = "bold", hjust = 0.5))

        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      
      response_plots[[resp]] <- build_bar_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        show_value_labels = show_value_labels,
        base_size = base_size,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry,
        y_limits = shared_y_limits
      )
    }
  }
  
  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = isTRUE(common_legend),
    legend_position = if (isTRUE(common_legend)) legend_position_value else NULL
  )
}


#### Low-level utilities ####

compute_barplot_shared_limits <- function(context,
                                          data,
                                          factor1,
                                          factor2,
                                          posthoc_all = NULL,
                                          show_value_labels = FALSE) {
  combined <- NULL

  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }

    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }

        rng <- compute_barplot_panel_range(
          stats_df, factor1, factor2,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc
        )
        combined <- update_numeric_range(combined, rng)
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

      rng <- compute_barplot_panel_range(
        stats_df, factor1, factor2,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry
      )
      combined <- update_numeric_range(combined, rng)
    }
  }

  if (is.null(combined)) return(NULL)
  upper_padding <- if (isTRUE(show_value_labels)) 0.18 else 0.12

  limits <- expand_axis_limits(combined, lower_mult = 0.05, upper_mult = upper_padding)
  ensure_barplot_zero_baseline(limits)
}

compute_barplot_panel_range <- function(stats_df,
                                        factor1,
                                        factor2,
                                        posthoc_entry = NULL,
                                        nested_posthoc = NULL) {
  if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)
  values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NULL)
  rng <- range(values)
  max_val <- rng[2]

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    prep <- prepare_significance_annotations_data(stats_df, factor1, posthoc_entry)
  } else {
    prep <- prepare_nested_significance_annotations_data(stats_df, factor1, factor2, nested_posthoc)
  }

  if (!is.null(prep) && !is.null(prep$max_y) && is.finite(prep$max_y)) {
    max_val <- max(max_val, prep$max_y)
  }

  c(rng[1], max_val)
}

build_bar_plot_panel <- function(stats_df,
                                 title_text,
                                 factor1,
                                 factor2,
                                 line_colors,
                                 base_fill,
                                 show_value_labels = FALSE,
                                 base_size = 14,
                                 posthoc_entry = NULL,
                                 nested_posthoc = NULL,
                                 y_limits = NULL) {

  # Compute per-panel limits when not sharing axes so we can always anchor at zero
  if (is.null(y_limits)) {
    panel_range <- compute_barplot_panel_range(
      stats_df,
      factor1,
      factor2,
      posthoc_entry = posthoc_entry,
      nested_posthoc = nested_posthoc
    )

    if (!is.null(panel_range)) {
      y_limits <- expand_axis_limits(panel_range, lower_mult = 0, upper_mult = 0.12)
      y_limits <- ensure_barplot_zero_baseline(y_limits)
    }
  }

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    return(
      build_single_factor_barplot(
        stats_df,
        title_text,
        factor1,
        base_fill,
        show_value_labels,
        base_size,
        posthoc_entry,
        y_limits = y_limits
      )
    )
  }

  build_two_factor_barplot(
    stats_df,
    title_text,
    factor1,
    factor2,
    line_colors,
    base_fill,
    show_value_labels,
    base_size,
    nested_posthoc,
    y_limits = y_limits
  )
}

# ===============================================================
# 🔹 Helper: Single-factor barplot (one-way ANOVA)
# ===============================================================

build_single_factor_barplot <- function(stats_df,
                                        title_text,
                                        factor1,
                                        base_fill,
                                        show_value_labels,
                                        base_size,
                                        posthoc_entry,
                                        y_limits = NULL) {
  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)

  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
    geom_col(fill = base_fill, width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.15,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    )

  expand_scale <- is.null(y_limits)

  if (isTRUE(show_value_labels)) {
    plot_obj <- add_bar_value_labels(
      plot_obj, stats_df, factor1, format_numeric_labels, base_size,
      expand_scale = expand_scale
    )
  }

  if (!is.null(posthoc_entry)) {
    plot_obj <- add_significance_annotations(
      plot_obj, stats_df, factor1, posthoc_entry,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Two-factor grouped barplot (two-way ANOVA)
# ===============================================================

add_bar_value_labels <- function(plot_obj,
                                stats_df,
                                factor1,
                                format_numeric_labels,
                                base_size,
                                expand_scale = TRUE) {
  label_df <- stats_df |>
    dplyr::mutate(
      .se = dplyr::coalesce(se, 0),
      label_text = format_numeric_labels(mean),
      label_y = ifelse(mean >= 0, mean + .se, mean - .se),
      label_vjust = ifelse(mean >= 0, -0.4, 1.2)
    )

  plot_obj <- plot_obj +
    geom_text(
      data = label_df,
      aes(x = !!sym(factor1), y = label_y, label = label_text, vjust = label_vjust),
      color = "gray20",
      size = compute_label_text_size(base_size),
      fontface = "bold",
      inherit.aes = FALSE
    )

  if (isTRUE(expand_scale)) {
    plot_obj <- plot_obj + scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Add value labels to grouped (two-factor) barplots
# ===============================================================

build_two_factor_barplot <- function(stats_df,
                                     title_text,
                                     factor1,
                                     factor2,
                                     line_colors,
                                     base_fill,
                                     show_value_labels,
                                     base_size,
                                     nested_posthoc = NULL,
                                     y_limits = NULL) {

  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)
  
  group_levels <- if (is.factor(stats_df[[factor2]])) {
    levels(stats_df[[factor2]])
  } else {
    unique(as.character(stats_df[[factor2]]))
  }
  group_levels <- group_levels[!is.na(group_levels)]
  palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
  dodge <- position_dodge(width = 0.7)
  
  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean, fill = !!sym(factor2))) +
    geom_col(position = dodge, width = 0.6, alpha = 0.85) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.2,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", fill = factor2, title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    ) +
    scale_fill_manual(values = palette)

  expand_scale <- is.null(y_limits)

  if (isTRUE(show_value_labels)) {
    plot_obj <- add_grouped_bar_value_labels(
      plot_obj, stats_df, factor1, factor2,
      format_numeric_labels, dodge, base_size,
      expand_scale = expand_scale
    )
  }

  if (!is.null(nested_posthoc)) {
    plot_obj <- add_nested_significance_annotations(
      plot_obj, stats_df, factor1, factor2, nested_posthoc,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Add value labels to single-factor barplots
# ===============================================================

add_grouped_bar_value_labels <- function(plot_obj,
                                         stats_df,
                                         factor1,
                                         factor2,
                                         format_numeric_labels,
                                         dodge,
                                         base_size,
                                         expand_scale = TRUE) {
  label_df <- stats_df |>
    dplyr::mutate(
      .se = dplyr::coalesce(se, 0),
      label_text = format_numeric_labels(mean),
      label_y = ifelse(mean >= 0, mean + .se, mean - .se),
      label_vjust = ifelse(mean >= 0, -0.4, 1.2)
    )

  plot_obj <- plot_obj +
    geom_text(
      data = label_df,
      aes(
        x = !!sym(factor1),
        y = label_y,
        label = label_text,
        vjust = label_vjust,
        fill = NULL,
        group = !!sym(factor2)
      ),
      position = dodge,
      color = "gray20",
      size = compute_label_text_size(base_size),
      fontface = "bold",
      inherit.aes = FALSE
    )

  if (isTRUE(expand_scale)) {
    plot_obj <- plot_obj + scale_y_continuous(expand = expansion(mult = c(0.05, 0.12)))
  }

  plot_obj
}

# ===============================================================
# 🔹 Helper: Significance annotation preparation & drawing
# ===============================================================

#### Section: Lineplot Construction ####

