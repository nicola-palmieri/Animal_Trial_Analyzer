# ===============================================================
# 🧩 PCA Helpers — Biplot Builder
# ===============================================================

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
    plot_data$`..label..` <- as.character(plot_data[[label_var]])
  } else {
    label_var <- NULL
  }

  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))

  g <- ggplot(plot_data, aes_mapping) +
    geom_point(
      size = 3,
      shape = if (is.null(shape_var)) "\U0001F413" else NULL,
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
      aes(label = `..label..`),
      size = label_size,
      max.overlaps = Inf,
      min.segment.length = 0,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2
    )
  }

  g
}
