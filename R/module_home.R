# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "home-wrapper px-3",
      div(
        class = "hero text-center mx-auto",
        h1("Welcome to Table Analyzer"),
        p(
          class = "lead text-muted",
          "Turn your tabular data into publication-ready tables and plots in a single afternoon."
        ),
        br(),
        div(
          class = "home-steps",
          fluidRow(
            class = "g-4 justify-content-center",
            column(
              width = 3,
              div(
                icon("upload", class = "fa-2x text-primary mb-2"),
                h5("1. Upload"),
                p("Bring in spreadsheets with ease.")
              )
            ),
            column(
              width = 3,
              div(
                icon("filter", class = "fa-2x text-primary mb-2"),
                h5("2. Filter"),
                p("Refine rows and columns to spotlight what's important.")
              )
            ),
            column(
              width = 3,
              div(
                icon("chart-line", class = "fa-2x text-primary mb-2"),
                h5("3. Analyze"),
                p("Run summaries and models tailored to your dataset.")
              )
            ),
            column(
              width = 3,
              div(
                icon("chart-area", class = "fa-2x text-primary mb-2"),
                h5("4. Visualize"),
                p("Create polished plots to communicate key findings.")
              )
            )
          )
        ),
        br(),
        tags$hr(class = "my-4"),
        p(
          em("Developed by Nicola Palmieri"),
          class = "text-muted small"
        )
      )
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder for future home page interactivity
  })
}
