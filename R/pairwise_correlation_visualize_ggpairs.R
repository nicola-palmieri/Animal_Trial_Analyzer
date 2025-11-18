# ===============================================================
# 🧪 Pairwise Correlation — GGPairs Visualization Module
# ===============================================================

pairwise_correlation_visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6, with_help_tooltip(
        numericInput(ns("plot_width"),  "Subplot width (px)",  800, 200, 2000, 50),
        "Set the width in pixels for each panel of the correlation matrix."
      )),
      column(6, with_help_tooltip(
        numericInput(ns("plot_height"), "Subplot height (px)", 600, 200, 2000, 50),
        "Set the height in pixels for each panel of the correlation matrix."
      ))
    ),
    plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of panels to use when multiple strata are plotted.",
      cols_help = "Choose how many columns of panels to use when multiple strata are plotted."
    ),
    fluidRow(
      column(6, add_color_customization_ui(ns, multi_group = TRUE)),
      column(6, base_size_ui(
        ns,
        default = 11,
        help_text = "Adjust the base font size used for the correlation plot."
      ))
    ),
    hr(),
    with_help_tooltip(
      downloadButton(ns("download_plot"), "Download Plot", style = "width: 100%;"),
      "Save the current correlation figure as an image file."
    )
  )
}


pairwise_correlation_visualize_ggpairs_server <- function(
    id, filtered_data, correlation_info
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ---- Helpers -------------------------------------------------------------
    sanitize <- function(x, default) {
      v <- suppressWarnings(as.numeric(x))
      if (is.na(v) || length(v) == 0) default else v
    }
    
    # ---- Reactives -----------------------------------------------------------
    df <- reactive(filtered_data())
    
    group_var <- reactive({
      info <- correlation_info()
      g <- resolve_reactive(info$group_var)
      if (is.null(info) || is.null(g) || g == "" || g == "None") return(NULL)
      dat <- df()
      if (!is.data.frame(dat) || !g %in% names(dat)) return(NULL)
      g
    })
    
    strata_order <- reactive({
      info <- correlation_info()
      so <- resolve_reactive(info$strata_order)
      if (is.null(so)) return(NULL)
      so <- as.character(so)
      so[nzchar(so)]
    })
    
    custom_colors <- add_color_customization_server(
      ns,
      input, output,
      data = df,
      color_var_reactive = group_var,
      multi_group = TRUE,
      level_order_reactive = strata_order
    )
    
    grid <- plot_grid_server("plot_grid")
    base_size <- base_size_server(input = input, default = 11)
    
    # ---- Unified state() -----------------------------------------------------
    state <- reactive({
      list(
        data      = df(),
        info      = correlation_info(),
        group_var = group_var(),
        strata_order = strata_order(),
        colors    = custom_colors(),
        base_size = base_size(),
        plot_w    = sanitize(input$plot_width, 800),
        plot_h    = sanitize(input$plot_height, 600),
        rows      = grid$rows(),
        cols      = grid$cols()
      )
    })
    
    # ---- Build one ggpairs plot ---------------------------------------------
    build_ggpairs_plot <- function(data, color, title = NULL, base_size) {
      numeric_cols <- data[, vapply(data, is.numeric, logical(1)), drop = FALSE]
      numeric_cols <- numeric_cols[, colSums(!is.na(numeric_cols)) > 0, drop = FALSE]
      validate(need(ncol(numeric_cols) >= 2, "Need ≥2 numeric columns."))
      
      p <- GGally::ggpairs(
        numeric_cols,
        progress = FALSE,
        upper = list(continuous = GGally::wrap("cor", size = 4, colour = color)),
        lower = list(continuous = GGally::wrap("points", alpha = 0.6, colour = color, size = 1.5)),
        diag  = list(continuous = GGally::wrap("densityDiag", fill = color, alpha = 0.4))
      ) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold", size = 9),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank()
        )
      
      if (!is.null(title)) p <- p + ggplot2::labs(title = title)
      
      # wrap ggmatrix into a real ggplot object
      gtable <- GGally::ggmatrix_gtable(p)
      ggplot2::ggplot() +
        ggplot2::theme_void() +
        ggplot2::annotation_custom(
          grob = gtable,
          xmin = -Inf, xmax = Inf,
          ymin = -Inf, ymax = Inf
        )
    }
    
    # ---- Unified compute plot_info() -----------------------------------------
    plot_info <- reactive({
      s <- state()
      dat <- s$data
      info <- s$info
      
      validate(need(!is.null(dat) && nrow(dat) > 0, "No data available."))
      validate(need(!is.null(info), "Correlation info missing."))
      
      results <- resolve_reactive(info$results)
      validate(need(!is.null(results), "Run the correlation analysis."))
      
      selected <- resolve_reactive(results$selected_vars)
      if (is.null(selected) || length(selected) < 2)
        selected <- names(dat)[vapply(dat, is.numeric, logical(1))]
      
      validate(need(length(selected) >= 2, "Need ≥2 numeric variables."))
      
      gvar <- s$group_var
      
      # --- No strata → single big plot ---------------------------------------
      if (is.null(gvar)) {
        pd <- dat[, selected, drop = FALSE]
        col <- resolve_single_color(s$colors)
        plot <- build_ggpairs_plot(pd, col, title = NULL, base_size = s$base_size)
        defaults <- compute_default_grid(1L)
        return(list(
          plot = plot,
          layout = list(rows = defaults$rows, cols = defaults$cols),
          warning = NULL,
          defaults = defaults
        ))
      }
      
      # --- Strata case --------------------------------------------------------
      lvls <- names(results$matrices)
      if (!length(lvls)) lvls <- unique(as.character(dat[[gvar]]))
      if (length(s$strata_order)) lvls <- s$strata_order[s$strata_order %in% lvls]
      lvls <- lvls[nzchar(lvls)]
      validate(need(length(lvls) > 0, "No valid strata levels."))
      
      palette <- resolve_palette_for_levels(lvls, custom = s$colors)
      
      plot_list <- lapply(lvls, function(lvl) {
        rows <- as.character(dat[[gvar]]) == lvl
        sub <- dat[rows, selected, drop = FALSE]
        if (!nrow(sub)) return(NULL)
        build_ggpairs_plot(sub, palette[[lvl]], title = lvl, base_size = s$base_size)
      })
      
      plot_list <- Filter(Negate(is.null), plot_list)
      validate(need(length(plot_list) > 0, "No data for strata."))
      
      n_panels <- length(plot_list)
      defaults <- compute_default_grid(n_panels)
      
      layout <- basic_grid_layout(
        rows = s$rows,
        cols = s$cols,
        default_rows = defaults$rows,
        default_cols = defaults$cols
      )
      layout <- adjust_grid_layout(n_panels, layout)
      val <- validate_grid(n_panels, layout$nrow, layout$ncol)
      
      combined <- NULL
      if (isTRUE(val$valid)) {
        combined <- patchwork::wrap_plots(plot_list,
                                          nrow = layout$nrow,
                                          ncol = layout$ncol
        )
      }

      list(
        plot = combined,
        layout = list(rows = layout$nrow, cols = layout$ncol),
        warning = val$message,
        defaults = defaults
      )
    })
    
    # ---- Unified caching ------------------------------------------------------
    cached_plot   <- reactiveVal(NULL)
    cached_layout <- reactiveVal(NULL)
    cached_key    <- reactiveVal(NULL)
    
    observe({
      s <- state()
      key <- paste(
        digest::digest(s$data, algo = "xxhash64"),
        s$group_var,
        s$strata_order,
        s$rows, s$cols,
        s$colors,
        s$base_size,
        s$plot_w, s$plot_h,
        sep = "_"
      )

      if (!identical(key, cached_key())) {
        info <- plot_info()
        if (!is.null(info$warning)) {
          cached_plot(NULL)
          cached_layout(NULL)
          cached_key(key)
          return()
        }

        if (!is.null(info$plot)) {
          cached_plot(info$plot)
          cached_layout(info$layout)
          cached_key(key)
        }
      }
    })

    observeEvent(plot_info(), {
      info <- plot_info()
      apply_grid_defaults_if_empty(input, session, "plot_grid", info$defaults)
    }, ignoreNULL = TRUE)

    # ---- Unified sizing -------------------------------------------------------
    plot_dimensions <- reactive({
      lay <- cached_layout()
      if (is.null(lay)) return(list(width = 800, height = 600))
      
      list(
        width  = max(200, state()$plot_w * lay$cols),
        height = max(200, state()$plot_h * lay$rows)
      )
    })
    
    # ---- Outputs --------------------------------------------------------------
    output$plot <- renderPlot({
      p <- cached_plot()
      validate(need(!is.null(p), "Plot not ready"))
      print(p)
    },
    width  = function() plot_dimensions()$width,
    height = function() plot_dimensions()$height,
    res = 96)
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("pairwise_correlation_ggpairs_", Sys.Date(), ".png"),
      content = function(file) {
        p <- cached_plot()
        lay <- cached_layout()
        validate(need(!is.null(p), "No plot"))
        validate(need(!is.null(lay), "No layout"))
        dims <- plot_dimensions()
        ggplot2::ggsave(
          file, p, dpi = 300,
          width = dims$width / 96,
          height = dims$height / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )

    outputOptions(output, "plot", suspendWhenHidden = TRUE)

    list(
      warning = reactive({
        info <- plot_info()
        if (is.null(info)) NULL else info$warning
      }),
      plot = reactive(cached_plot()),
      width = reactive(plot_dimensions()$width),
      height = reactive(plot_dimensions()$height)
    )
  })
}

