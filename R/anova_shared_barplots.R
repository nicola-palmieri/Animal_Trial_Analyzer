#### Section: Barplot Construction ####

plot_anova_barplot_meanse <- function(data,
                                      info,
                                      layout_values = list(),
                                      line_colors = NULL,
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
      posthoc_all
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

compute_barplot_shared_limits <- function(context,
                                          data,
                                          factor1,
                                          factor2,
                                          posthoc_all = NULL) {
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

  limits <- expand_axis_limits(combined, lower_mult = 0.05, upper_mult = 0.12)
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
    prep <- prepare_reference_significance_labels(stats_df, factor1, posthoc_entry)
  } else {
    prep <- prepare_nested_reference_significance_labels(stats_df, factor1, factor2, nested_posthoc)
  }

  if (!is.null(prep) && !is.null(prep$max_y) && is.finite(prep$max_y)) {
    max_val <- max(max_val, prep$max_y)
  }

  c(rng[1], max_val)
}


expand_axis_limits <- function(range_vals, lower_mult = 0.05, upper_mult = 0.12) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) return(range_vals)
  span <- diff(range_vals)
  if (!is.finite(span) || span == 0) {
    span <- max(1, abs(range_vals[2]))
  }
  c(range_vals[1] - span * lower_mult, range_vals[2] + span * upper_mult)
}


ensure_barplot_zero_baseline <- function(range_vals) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) {
    return(range_vals)
  }
  
  lower <- range_vals[1]
  if (is.na(lower)) return(range_vals)
  
  # Keep barplots anchored at zero to avoid negative baselines when no annotations
  range_vals[1] <- 0
  range_vals
}


build_bar_plot_panel <- function(stats_df,
                                 title_text,
                                 factor1,
                                 factor2,
                                 line_colors,
                                 base_fill,
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
    base_size,
    nested_posthoc,
    y_limits = y_limits
  )
}


build_single_factor_barplot <- function(stats_df,
                                        title_text,
                                        factor1,
                                        base_fill,
                                        base_size,
                                        posthoc_entry,
                                        y_limits = NULL) {

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

  if (!is.null(posthoc_entry)) {
    plot_obj <- add_reference_significance_labels(
      plot_obj, stats_df, factor1, posthoc_entry,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

build_two_factor_barplot <- function(stats_df,
                                     title_text,
                                     factor1,
                                     factor2,
                                     line_colors,
                                     base_fill,
                                     base_size,
                                     nested_posthoc = NULL,
                                     y_limits = NULL) {
  
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

  if (!is.null(nested_posthoc)) {
    plot_obj <- add_nested_significance_labels(
      plot_obj, stats_df, factor1, factor2, nested_posthoc,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}


#### Section: Significance Annotation System ####

clean_posthoc_p_values <- function(df) {
  df$p.value <- as.character(df$p.value)
  df$p.value <- gsub("[[:space:]]", "", df$p.value)
  df$p.value <- gsub("^<\.?0*", "0.", df$p.value)
  df$p.value <- suppressWarnings(as.numeric(df$p.value))
  df
}

extract_posthoc_table <- function(entry) {
  if (is.null(entry)) return(NULL)
  if (is.data.frame(entry)) return(entry)

  if (is.list(entry) && !is.null(entry$table) && is.data.frame(entry$table)) {
    return(entry$table)
  }

  entry
}

reference_level <- function(vec) {
  levs <- levels(vec)
  if (!is.null(levs) && length(levs) > 0) {
    return(levs[1])
  }
  uniq <- unique(as.character(vec))
  if (length(uniq) > 0) uniq[1] else NULL
}

stars_for_p <- function(p) {
  dplyr::case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE ~ ""
  )
}

prepare_reference_significance_labels <- function(stats_df, factor1, posthoc_entry) {
  posthoc_tbl <- extract_posthoc_table(posthoc_entry)
  if (is.null(posthoc_tbl) || !is.data.frame(posthoc_tbl)) return(NULL)
  if (!"contrast" %in% names(posthoc_tbl) || !"p.value" %in% names(posthoc_tbl)) return(NULL)

  ref <- reference_level(stats_df[[factor1]])
  if (is.null(ref) || is.na(ref)) return(NULL)

  df <- posthoc_tbl
  if ("Factor" %in% names(df)) {
    df <- dplyr::filter(df, .data$Factor == factor1)
  }
  if (nrow(df) == 0) return(NULL)

  df <- clean_posthoc_p_values(df)

  levs <- levels(stats_df[[factor1]])
  if (is.null(levs)) levs <- unique(as.character(stats_df[[factor1]]))
  levs <- levs[levs != ref]
  if (length(levs) == 0) return(NULL)

  bar_heights <- stats_df$mean + stats_df$se
  names(bar_heights) <- as.character(stats_df[[factor1]])
  max_height <- max(bar_heights, na.rm = TRUE)
  offset <- if (is.finite(max_height) && max_height != 0) abs(max_height) * 0.08 else 0.1

  find_p <- function(level) {
    rows <- df$contrast %in% c(paste(level, "-", ref), paste(ref, "-", level))
    if (!any(rows)) return(NA_real_)
    df$p.value[which(rows)[1]]
  }

  label_data <- lapply(levs, function(lv) {
    pval <- find_p(lv)
    if (is.na(pval) || pval >= 0.05) return(NULL)
    y_base <- bar_heights[lv]
    if (!is.finite(y_base)) return(NULL)
    tibble::tibble(
      !!factor1 := factor(lv, levels = levels(stats_df[[factor1]])),
      y_position = y_base + offset,
      annotations = stars_for_p(pval)
    )
  }) |>
    purrr::compact() |>
    dplyr::bind_rows()

  if (nrow(label_data) == 0) return(NULL)

  list(
    data = label_data,
    max_y = max(label_data$y_position, na.rm = TRUE) * 1.05
  )
}

add_reference_significance_labels <- function(plot_obj,
                                              stats_df,
                                              factor1,
                                              posthoc_entry,
                                              allow_scale_expansion = TRUE) {
  prep <- prepare_reference_significance_labels(stats_df, factor1, posthoc_entry)
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj +
    geom_text(
      data = prep$data,
      aes(x = !!sym(factor1), y = y_position, label = annotations),
      vjust = 0,
      fontface = "bold",
      color = "#b91c1c",
      size = 3.8
    )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.12)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}

prepare_nested_reference_significance_labels <- function(stats_df,
                                                         factor1,
                                                         factor2,
                                                         nested_posthoc,
                                                         dodge_width = 0.7) {
  nested_name <- paste0(factor2, "_within_", factor1)

  # Accept both a flat data.frame or a list entry
  df <- extract_posthoc_table(nested_posthoc)
  if (is.null(df)) return(NULL)

  if (is.data.frame(df) && "Factor" %in% names(df)) {
    df <- dplyr::filter(df, .data$Factor == nested_name)
  } else if (is.list(nested_posthoc) && nested_name %in% names(nested_posthoc)) {
    df <- extract_posthoc_table(nested_posthoc[[nested_name]])
  }

  if (is.null(df)) return(NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!all(c("contrast", "p.value", factor1) %in% names(df))) return(NULL)

  df <- clean_posthoc_p_values(df)

  lev1 <- levels(stats_df[[factor1]])
  lev2 <- levels(stats_df[[factor2]])
  if (is.null(lev1)) lev1 <- unique(as.character(stats_df[[factor1]]))
  if (is.null(lev2)) lev2 <- unique(as.character(stats_df[[factor2]]))
  if (length(lev2) < 2) return(NULL)

  ref2 <- lev2[1]

  bar_heights <- stats_df |>
    dplyr::mutate(height = mean + se) |>
    dplyr::select(all_of(c(factor1, factor2)), height)

  max_height <- max(bar_heights$height, na.rm = TRUE)
  offset <- if (is.finite(max_height) && max_height != 0) abs(max_height) * 0.08 else 0.1

  label_data <- list()

  for (lvl1 in lev1) {
    local_df <- dplyr::filter(df, .data[[factor1]] == lvl1)
    if (nrow(local_df) == 0) next

    for (lvl2 in lev2[-1]) {
      contrasts <- c(paste(lvl2, "-", ref2), paste(ref2, "-", lvl2))
      row <- dplyr::filter(local_df, .data$contrast %in% contrasts)
      if (nrow(row) == 0) next
      pval <- row$p.value[1]
      if (is.na(pval) || pval >= 0.05) next

      height_row <- dplyr::filter(bar_heights, .data[[factor1]] == lvl1, .data[[factor2]] == lvl2)
      if (nrow(height_row) == 0 || !is.finite(height_row$height[1])) next

      label_data[[length(label_data) + 1]] <- tibble::tibble(
        !!factor1 := factor(lvl1, levels = levels(stats_df[[factor1]])),
        !!factor2 := factor(lvl2, levels = levels(stats_df[[factor2]])),
        y_position = height_row$height[1] + offset,
        annotations = stars_for_p(pval)
      )
    }
  }

  label_data <- purrr::compact(label_data)
  if (length(label_data) == 0) return(NULL)
  label_data <- dplyr::bind_rows(label_data)

  list(
    data = label_data,
    max_y = max(label_data$y_position, na.rm = TRUE) * 1.05,
    dodge_width = dodge_width
  )
}

add_nested_significance_labels <- function(plot_obj,
                                           stats_df,
                                           factor1,
                                           factor2,
                                           nested_posthoc,
                                           dodge_width = 0.7,
                                           allow_scale_expansion = TRUE) {
  prep <- prepare_nested_reference_significance_labels(
    stats_df, factor1, factor2, nested_posthoc, dodge_width
  )
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj +
    geom_text(
      data = prep$data,
      aes(
        x = !!sym(factor1),
        y = y_position,
        label = annotations,
        group = !!sym(factor2)
      ),
      position = position_dodge(width = prep$dodge_width),
      vjust = 0,
      fontface = "bold",
      color = "#b91c1c",
      size = 3.8
    )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.12)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}
