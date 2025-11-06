# ===============================================================
# 🎨 UI helper to assign colors per level of a factor
# ===============================================================

render_color_inputs <- function(ns, data, color_var) {
  if (is.null(color_var) || color_var == "None") return(NULL)
  if (!color_var %in% names(data())) return(NULL)

  values <- data()[[color_var]]
  lvls <- if (is.factor(values)) levels(values) else unique(as.character(values))
  lvls <- lvls[!is.na(lvls)]
  default_palette <- rep(basic_color_palette, length.out = length(lvls))

  tagList(
    br(),
    h5(paste("Colors for", color_var)),
    lapply(seq_along(lvls), function(i) {
      selected <- default_palette[i]
      tags$div(
        style = "margin-bottom: 8px;",
        tags$label(lvls[i], style = "display:block; margin-bottom: 4px;"),
        color_dropdown_input(
          ns,
          id = paste0("col_", color_var, "_", i),
          palette = basic_color_palette,
          ncol = 4,
          selected = selected
        )
      )
    })
  )
}

resolve_single_color <- function(custom = NULL) {
  if (!is.null(custom) && length(custom) > 0) {
    candidate <- unname(custom[[1]])
    if (!is.null(candidate) && nzchar(candidate)) {
      return(candidate)
    }
  }
  basic_color_palette[1]
}

resolve_palette_for_levels <- function(levels, custom = NULL) {
  if (is.null(levels) || length(levels) == 0) {
    return(resolve_single_color())
  }

  unique_levels <- unique(as.character(levels))
  palette_size <- length(basic_color_palette)
  n_levels <- length(unique_levels)

  if (n_levels > palette_size) {
    stop(
      sprintf(
        "Palette can assign at most %d groups but received %d levels.",
        palette_size,
        n_levels
      ),
      call. = FALSE
    )
  }

  if (!is.null(custom) && length(custom) > 0) {
    if (!is.null(names(custom))) {
      ordered <- custom[unique_levels]
      if (all(!is.na(ordered))) {
        return(ordered)
      }
    } else if (length(custom) >= n_levels) {
      return(stats::setNames(custom[seq_len(n_levels)], unique_levels))
    }
  }

  stats::setNames(basic_color_palette[seq_len(n_levels)], unique_levels)
}
