# ===============================================================
# 🎨 Visualization Plot Builders
# ===============================================================

build_descriptive_plots <- function(summary_info) {
  # summary_info is a list containing elements like skim, cv, outliers, shapiro, missing
  data <- summary_info()
  
  plots <- list()
  
  # Example 1 — CV% barplot
  if (!is.null(data$cv)) {
    cv_long <- data$cv |> tidyr::pivot_longer(
      cols = -1,
      names_to = "variable",
      values_to = "cv"
    )
    cv_long$variable <- gsub("^cv_", "", cv_long$variable)
    p_cv <- ggplot(cv_long, aes(x = variable, y = cv, fill = .data[[1]])) +
      geom_col(position = position_dodge()) +
      theme_minimal(base_size = 14) +
      labs(y = "CV (%)", x = NULL, fill = names(data$cv)[1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["cv"]] <- p_cv
  }
  
  # Example 2 — Outlier counts
  if (!is.null(data$outliers)) {
    out_long <- data$outliers |> tidyr::pivot_longer(
      cols = -1,
      names_to = "variable",
      values_to = "count"
    )
    out_long$variable <- gsub("^outliers_", "", out_long$variable)
    p_out <- ggplot(out_long, aes(x = variable, y = count, fill = .data[[1]])) +
      geom_col(position = position_dodge()) +
      theme_minimal(base_size = 14) +
      labs(y = "Outlier Count", x = NULL, fill = names(data$outliers)[1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["outliers"]] <- p_out
  }
  
  # Example 3 — Missingness
  if (!is.null(data$missing)) {
    miss_long <- data$missing |> tidyr::pivot_longer(
      cols = -1,
      names_to = "variable",
      values_to = "missing"
    )
    miss_long$variable <- gsub("^missing_", "", miss_long$variable)
    p_miss <- ggplot(miss_long, aes(x = variable, y = missing, fill = .data[[1]])) +
      geom_col(position = position_dodge()) +
      theme_minimal(base_size = 14) +
      labs(y = "Missing (%)", x = NULL, fill = names(data$missing)[1]) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plots[["missing"]] <- p_miss
  }
  
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
