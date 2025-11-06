# ===============================================================
# 🧪 Prototype App — Interactive Grid Layout Demo
# ===============================================================

library(shiny)
library(ggplot2)
library(patchwork)

# ---- Helper functions ----
compute_default_grid <- function(n) {
  n <- max(1L, as.integer(n))
  rows <- ceiling(sqrt(n))
  cols <- ceiling(n / rows)
  list(rows = rows, cols = cols)
}

validate_grid <- function(n_items, rows, cols) {
  n_items <- max(1L, as.integer(n_items))
  rows <- max(1L, as.integer(rows))
  cols <- max(1L, as.integer(cols))
  
  too_small <- rows * cols < n_items
  empty_row <- n_items <= (rows - 1L) * cols
  empty_col <- n_items <= rows * (cols - 1L)
  too_large <- (!too_small) && (empty_row || empty_col)
  
  if (too_small)
    return(list(valid = FALSE,
                message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)))
  if (too_large)
    return(list(valid = FALSE,
                message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)))
  list(valid = TRUE, message = NULL)
}

# ---- Sample data generator ----
make_example_plots <- function(n) {
  lapply(seq_len(n), function(i) {
    ggplot(data.frame(x = rnorm(100)), aes(x)) +
      geom_histogram(bins = 20, fill = "#3182bd", color = "white") +
      theme_minimal(base_size = 10) +
      ggtitle(paste("Plot", i))
  })
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("🧮 Grid Layout Prototype"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      sliderInput("n_plots", "Number of subplots:", min = 1, max = 20, value = 9, step = 1),
      hr(),
      numericInput("rows", "Rows:", value = NA, min = 1, max = 10, step = 1),
      numericInput("cols", "Columns:", value = NA, min = 1, max = 10, step = 1),
      helpText("Change rows/cols to test warnings."),
      actionButton("apply", "Apply layout"),
      hr(),
      p("Optimal layout is auto-computed at start or when you reset.")
    ),
    mainPanel(
      uiOutput("warning"),
      plotOutput("grid_plot", height = "auto")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # Precompute optimal grid on start and when number of plots changes
  observeEvent(input$n_plots, {
    g <- compute_default_grid(input$n_plots)
    updateNumericInput(session, "rows", value = g$rows)
    updateNumericInput(session, "cols", value = g$cols)
  }, ignoreInit = FALSE)
  
  # Combine subplots into patchwork layout
  output$grid_plot <- renderPlot({
    req(input$apply)
    n <- input$n_plots
    val <- validate_grid(n, input$rows, input$cols)
    
    if (!val$valid) {
      output$warning <- renderUI(div(class = "alert alert-warning", val$message))
      return(NULL)
    }
    
    output$warning <- renderUI(NULL)
    plots <- make_example_plots(n)
    layout <- wrap_plots(plots, nrow = input$rows, ncol = input$cols)
    layout
  },
  width = function() input$cols * 250,
  height = function() input$rows * 200,
  res = 96)
}

shinyApp(ui, server)
