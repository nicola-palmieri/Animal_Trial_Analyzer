# Styled version of ggpairs matching ANOVA plot aesthetics
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
      panel.grid.major.y = element_line(color = "gray85"),
      plot.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(color = "black")
    )
}
