# ===============================================================
# 🎨 Compact dropdown-style color picker (4x4 grid)
# ===============================================================

# ---- Palette ----
basic_color_palette <- c(
  "steelblue" = "#4682B4",
  "red"       = "#FF0000",
  "green"     = "#008000",  # ✅ web green (not ggplot's neon)
  "blue"      = "#0000FF",
  "orange"    = "#FFA500",
  "purple"    = "#800080",
  "brown"     = "#A52A2A",
  "gold"      = "#FFD700",
  "pink"      = "#FF69B4",
  "cyan"      = "#00FFFF",
  "magenta"   = "#FF00FF",
  "yellow"    = "#FFFF00",
  "black"     = "#000000",
  "gray"      = "#808080",
  "darkgreen" = "#006400",
  "darkred"   = "#8B0000"
)

# ---- UI Helper ----
color_dropdown_input <- function(ns, id = "color_choice", palette = basic_color_palette,
                                 ncol = 4, selected = NULL) {
  selected_color <- if (is.null(selected)) palette[1] else selected

  cell_size <- 26
  gap_size <- 4
  padding_size <- 3
  button_size <- cell_size + (2 * padding_size)
  button_height <- button_size
  dropdown_width <- cell_size * ncol + gap_size * (ncol - 1) + 2 * padding_size
  dropdown_top <- button_height + 4

  tagList(
    tags$style(HTML(sprintf("
        .color-dropdown {
          position: relative;
          display: inline-block;
          user-select: none;
          width: %dpx;
        }
        .color-dropdown-button {
          width: 100%%;
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: %dpx;
          background-color: #fff;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          cursor: pointer;
          box-shadow: 0 1px 2px rgba(0,0,0,0.05);
        }
        .color-dropdown-swatch {
          flex: 0 0 %dpx;
          width: %dpx;
          height: %dpx;
          border-radius: 4px;
          border: 1px solid #ccc;
          box-sizing: border-box;
        }
        .color-dropdown-grid {
          display: none;
          position: absolute;
          top: %dpx;
          left: 0;
          z-index: 999;
          background: #fff;
          border: 1px solid #ccc;
          border-radius: 6px;
          padding: %dpx;
          box-shadow: 0 4px 12px rgba(0,0,0,0.08);
          width: %dpx;
          grid-template-columns: repeat(%d, %dpx);
          gap: %dpx;
          box-sizing: border-box;
        }
        .color-dropdown-grid.open {
          display: grid;
        }
        .color-cell {
          width: %dpx;
          height: %dpx;
          border-radius: 4px;
          cursor: pointer;
          border: 1px solid #ccc;
          box-sizing: border-box;
          transition: transform 0.1s ease;
        }
        .color-cell:hover {
          transform: scale(1.05);
        }
      ",
      button_size, padding_size, cell_size, cell_size, cell_size,
      dropdown_top, padding_size, dropdown_width, ncol, cell_size, gap_size,
      cell_size, cell_size
    ))),
    tags$div(
      class = "color-dropdown",
      tags$div(
        id = ns(paste0(id, "_button")),
        class = "color-dropdown-button",
        tags$span(
          class = "color-dropdown-swatch",
          style = sprintf("background-color:%s;", selected_color)
        )
      ),
      tags$div(
        id = ns(paste0(id, "_grid")),
        class = "color-dropdown-grid",
        lapply(names(palette), function(col_name) {
          hex <- palette[[col_name]]
          tags$div(
            class = "color-cell",
            title = col_name,  # tooltip shows readable color name
            style = sprintf("background-color:%s;", hex),
            onclick = sprintf(
              "
      var button = $('#%s_button');
      button.find('.color-dropdown-swatch').css('background-color','%s');
      $('#%s_grid').removeClass('open');
      Shiny.setInputValue('%s','%s',{priority:'event'});
      ",
              ns(id), hex, ns(id), ns(id), hex
            )
          )
        })
        
      )
    ),
    tags$script(HTML(sprintf("
        $('#%s_button').on('click', function(e){
          e.stopPropagation();
          var grid = $('#%s_grid');
          $('.color-dropdown-grid').not(grid).removeClass('open');
          grid.toggleClass('open');
        });
        $(document).on('click', function(){
          $('.color-dropdown-grid').removeClass('open');
        });
        Shiny.setInputValue('%s','%s',{priority:'event'});
      ",
      ns(id), ns(id), ns(id), selected_color
    )))
  )
}

# ===============================================================
# 🛈 Consistent tooltip helper for UI widgets
# ===============================================================

with_help_tooltip <- function(widget, text) {
  tags$div(
    class = "ta-help-tooltip",
    title = text,
    widget
  )
}
