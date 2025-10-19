# ===============================================================
# 🧩 PCA Helpers — Biplot Builder
# ===============================================================

build_pca_biplot <- function(pca_obj, data, color_var = NULL, shape_var = NULL) {
  stopifnot(!is.null(pca_obj$x))
  
  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores)[1:2] <- c("PC1", "PC2")
  
  if (!is.null(data) && nrow(data) == nrow(scores)) {
    plot_data <- cbind(scores, data)
  } else {
    plot_data <- scores
  }
  
  aes_mapping <- aes(x = PC1, y = PC2)
  if (!is.null(color_var)) aes_mapping <- modifyList(aes_mapping, aes(color = .data[[color_var]]))
  if (!is.null(shape_var)) aes_mapping <- modifyList(aes_mapping, aes(shape = .data[[shape_var]]))
  
  ggplot(plot_data, aes_mapping) +
    geom_point(size = 3) +
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
}
