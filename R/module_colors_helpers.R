# ===============================================================
# 🎨 Color palette helpers
# ===============================================================

get_primary_color <- function() {
  basic_color_palette[1]
}

get_palette_for_n <- function(n) {
  if (is.null(n)) return(character(0))
  n <- suppressWarnings(as.integer(n[1]))
  if (is.na(n) || n <= 0) return(character(0))
  idx <- seq_len(min(n, length(basic_color_palette)))
  basic_color_palette[idx]
}

sanitize_palette_levels <- function(values) {
  if (is.null(values)) return(character(0))
  if (is.factor(values)) {
    levels_vec <- as.character(levels(values))
  } else {
    levels_vec <- as.character(values)
  }
  levels_vec <- trimws(levels_vec)
  levels_vec <- levels_vec[!is.na(levels_vec) & nzchar(levels_vec)]
  if (!is.factor(values)) {
    levels_vec <- levels_vec[!duplicated(levels_vec)]
  }
  if (length(levels_vec) == 0) return(character(0))
  levels_vec
}

get_palette_for_levels <- function(levels_vec) {
  if (is.null(levels_vec)) return(character(0))
  levels_vec <- trimws(as.character(levels_vec))
  levels_vec <- levels_vec[!is.na(levels_vec) & nzchar(levels_vec)]
  if (length(levels_vec) == 0) return(character(0))
  levels_vec <- levels_vec[!duplicated(levels_vec)]
  colors <- get_palette_for_n(length(levels_vec))
  if (length(colors) == 0) return(colors)
  names(colors) <- levels_vec[seq_along(colors)]
  colors
}

get_palette_for_values <- function(values) {
  levels_vec <- sanitize_palette_levels(values)

  if (length(levels_vec) == 0) {
    raw_values <- as.character(values)
    raw_values <- trimws(raw_values)
    raw_values <- raw_values[!is.na(raw_values)]
    if (length(raw_values) == 0) return(character(0))
    levels_vec <- raw_values[!duplicated(raw_values)]
  }

  colors <- get_palette_for_n(length(levels_vec))
  if (length(colors) == 0) return(colors)
  names(colors) <- levels_vec[seq_along(colors)]
  colors
}
