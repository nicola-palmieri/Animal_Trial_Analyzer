# ===============================================================
# 🎨 Shared plot theming utilities
# ===============================================================

# Consistent minimalist theme for Table Analyzer plots.
ta_plot_theme <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1f2d3d"),
      plot.subtitle = ggplot2::element_text(color = "#4b5563"),
      plot.caption = ggplot2::element_text(color = "#6b7280", size = ggplot2::rel(0.85)),
      axis.title = ggplot2::element_text(face = "plain", color = "#111827"),
      axis.text = ggplot2::element_text(color = "#1f2937"),
      panel.grid.major = ggplot2::element_line(color = "#e5e7eb"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "#e5ecf6", color = NA),
      strip.text = ggplot2::element_text(face = "bold", color = "#111827"),
      legend.title = ggplot2::element_text(face = "plain"),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

# Theme for plots that intentionally hide axes while keeping typographic harmony.
ta_plot_theme_void <- function(base_size = 12) {
  ggplot2::theme_void(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", color = "#1f2d3d"),
      plot.subtitle = ggplot2::element_text(color = "#4b5563"),
      plot.caption = ggplot2::element_text(color = "#6b7280", size = ggplot2::rel(0.85)),
      strip.text = ggplot2::element_text(face = "bold", color = "#111827"),
      legend.title = ggplot2::element_text(face = "plain"),
      legend.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
