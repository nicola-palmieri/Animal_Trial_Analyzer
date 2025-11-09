# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================


library(bslib)
library(dplyr)
library(DT)
library(emmeans)
library(flextable)
library(GGally)
library(ggsignif)
library(ggplot2)
library(lmerTest)
library(officer)
library(patchwork)
library(readxl)
library(shiny)
library(shinyjqui)
library(skimr)
library(tidyr)
library(zoo)

options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- navbarPage(
  title = tagList(icon("table"), "Table Analyzer"),
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  
  # ---- Custom CSS (copied from website) ----
  header = tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 100%; margin: auto; }
      .hero {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9f5ff 100%);
        border-radius: 16px;
        padding: 40px 24px;
        margin-top: 20px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }
      h1, h2, h3 { margin-top: 0.4rem; }
      .section { margin-top: 18px; }
      .card { border-radius: 16px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); }
      .nav-tabs > li > a { font-weight: 500; }
      .empty-state { max-width: 420px; margin-left: auto; margin-right: auto; }
      .empty-state-icon { font-size: 3rem; line-height: 1; }
      .empty-state h4 { font-weight: 600; }
      .ta-help-tooltip { cursor: help; display: inline-block; width: 100%; }
      .home-wrapper {
        min-height: calc(100vh - 180px);
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .home-wrapper .hero {
        max-width: 760px;
        width: 100%;
      }
      .home-steps h5 {
        font-weight: 600;
      }
      .home-steps p {
        margin-bottom: 0;
        color: #6c757d;
      }
    "))
  ),

  tabPanel(
    title = tagList(icon("home"), " Home"),
    fluidPage(
      div(
        class = "home-wrapper px-3",
        div(
          class = "hero text-center mx-auto",
          h1("Welcome to Table Analyzer"),
          p(
            class = "lead text-muted",
            "Explore, clean, and interpret tabular data in a calm, guided workspace."
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
                  p("Bring in spreadsheets or CSV files with ease.")
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
            em("Developed by the Data Insights Lab for transparent, reproducible analysis."),
            class = "text-muted small"
          )
        )
      )
    )
  ),

  tabPanel(
    title = tagList(icon("upload"), " Upload"),
    fluidPage(upload_ui("upload"))
  ),
  tabPanel(
    title = tagList(icon("filter"), " Filter"),
    fluidPage(filter_ui("filter"))
  ),
  tabPanel(
    title = tagList(icon("chart-line"), " Analyze"),
    fluidPage(analysis_ui("analysis"))
  ),
  tabPanel(
    title = tagList(icon("chart-area"), " Visualize"),
    fluidPage(visualize_ui("visualize"))
  ),
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  uploaded  <- upload_server("upload")
  filtered  <- filter_server("filter", uploaded)
  analyzed  <- analysis_server("analysis", filtered)
  visualize_server("visualize", filtered, analyzed)
}

# ---------------------------------------------------------------
# LAUNCH
# ---------------------------------------------------------------
shinyApp(ui, server)
