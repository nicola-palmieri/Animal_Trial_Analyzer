# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================


library(bslib)
library(digest)
library(dplyr)
library(DT)
library(emmeans)
library(fitdistrplus)
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
      pre.shiny-text-output {
        white-space: pre;
        overflow-x: auto;
        font-family: Fira Mono, Source Code Pro, Monaco, monospace;
        font-size: 0.9rem;
      }
    "))
  ),

  tabPanel(
    title = tagList(icon("home"), " Home"),
    home_ui("home")
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
  home_server("home")
  uploaded  <- upload_server("upload")
  filtered  <- filter_server("filter", uploaded)
  analyzed  <- analysis_server("analysis", filtered)
  visualize_server("visualize", filtered, analyzed)
}

# ---------------------------------------------------------------
# LAUNCH
# ---------------------------------------------------------------
shinyApp(ui, server)
