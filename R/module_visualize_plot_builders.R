# ===============================================================
# 🎨 Visualization Plot Builders
# ===============================================================

build_descriptive_plots <- function(summary_info) {
  data <- summary_info()

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

    list(
      data = tidy,
      has_group = has_group,
      group_label = group_label
    )
  }

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
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank()
      )
  }

  plots <- list(
    cv = build_bar_plot(tidy_metric(data$cv, "cv"), "CV (%)"),
    outliers = build_bar_plot(tidy_metric(data$outliers, "outliers"), "Outlier Count"),
    missing = build_bar_plot(tidy_metric(data$missing, "missing"), "Missing (%)")
  )

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  patchwork::wrap_plots(plots, ncol = 1)
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

      layout <- compute_layout(
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

  resp_layout <- compute_layout(
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
