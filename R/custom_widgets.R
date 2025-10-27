# ===============================================================
# 🎨 Compact dropdown-style color picker (4x4 grid)
# ===============================================================

# ---- Palette ----
basic_color_palette <- c(
  "steelblue", "red", "green", "blue",
  "orange", "purple", "brown", "gold",
  "pink", "cyan", "magenta", "yellow",
  "black", "gray", "darkgreen", "darkred"
)

# ---- UI Helper ----
color_dropdown_input <- function(ns, id = "color_choice", palette = basic_color_palette,
                                 ncol = 4, selected = NULL) {
  selected_color <- if (is.null(selected)) palette[1] else selected

  tagList(
    tags$style(HTML(sprintf("
      .color-dropdown {
        position: relative;
        display: inline-block;
        width: 150px;
        user-select: none;
      }
      .color-dropdown-button {
        width: 100%%;
        height: 32px;
        border: 1px solid #ccc;
        border-radius: 4px;
        cursor: pointer;
      }
      .color-dropdown-grid {
        display: none;
        position: absolute;
        top: 36px;
        left: 0;
        z-index: 999;
        background: white;
        border: 1px solid #ccc;
        border-radius: 4px;
        padding: 4px;
        display: grid;
        grid-template-columns: repeat(%d, 28px);
        gap: 2px;
      }
      .color-cell {
        width: 26px; height: 26px;
        border-radius: 4px;
        cursor: pointer;
        border: 1px solid #ccc;
      }
      .color-cell:hover {
        transform: scale(1.1);
      }
    ", ncol))),
    tags$div(
      class = "color-dropdown",
      tags$div(
        id = ns(paste0(id, "_button")),
        class = "color-dropdown-button",
        style = sprintf("background-color:%s;", selected_color)
      ),
      tags$div(
        id = ns(paste0(id, "_grid")),
        class = "color-dropdown-grid",
        lapply(palette, function(col) {
          tags$div(
            class = "color-cell",
            title = col,
            style = sprintf("background-color:%s;", col),
            onclick = sprintf("
              $('#%s_button').css('background-color','%s');
              $('#%s_grid').hide();
              Shiny.setInputValue('%s','%s',{priority:'event'});
            ", ns(id), col, ns(id), ns(id), col)
          )
        })
      )
    ),
    tags$script(HTML(sprintf("
      $('#%s_button').on('click', function(e){
        e.stopPropagation();
        var grid = $('#%s_grid');
        $('.color-dropdown-grid').not(grid).hide();
        grid.toggle();
      });
      $(document).on('click', function(){
        $('.color-dropdown-grid').hide();
      });
      Shiny.setInputValue('%s','%s',{priority:'event'});
    ", ns(id), ns(id), ns(id), selected_color)))
  )
}
