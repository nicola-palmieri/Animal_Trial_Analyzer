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

theme_set(theme_get() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()))

options(shiny.autoreload = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)

is_shinyapps_io <- function() {
  env_vars <- c("SHINYAPPS_ACCOUNT", "SHINYAPPS_SERVER", "SHINYAPPS_URL")
  any(nzchar(Sys.getenv(env_vars)))
}

if (is_shinyapps_io()) {
  # Ensure long prints are not truncated on shinyapps.io, which may use a smaller max.print.
  options(max.print = 1e5)
}

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- navbarPage(
  title = tagList(icon("table"), "Table Analyzer"),
  id = "main_nav",
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Manrope"),
    primary = "#3056D3",
    secondary = "#0F172A",
    success = "#16A34A",
    light = "#F8FAFF"
  ),

  # ---- Custom CSS ----
  header = tags$head(
    tags$style(HTML("
      body {
        background: radial-gradient(circle at 15% 20%, rgba(48,86,211,0.06), transparent 35%),
                    radial-gradient(circle at 80% 0%, rgba(22,163,74,0.06), transparent 30%),
                    #f5f7fb;
        color: #0f172a;
      }
      .navbar {
        border-bottom: 1px solid #e5e7eb;
        box-shadow: 0 8px 30px rgba(15, 23, 42, 0.04);
        background: rgba(255,255,255,0.92);
        backdrop-filter: blur(8px);
      }
      .navbar .navbar-brand, .navbar-nav > li > a {
        font-weight: 700;
        letter-spacing: 0.01em;
      }
      .container-fluid { max-width: 1180px; margin: auto; }
      .hero {
        background: linear-gradient(135deg, #f8faff 0%, #e6edff 100%);
        border-radius: 18px;
        padding: 48px 32px;
        margin-top: 24px;
        box-shadow: 0 12px 30px rgba(15, 23, 42, 0.08);
      }
      h1, h2, h3, h4, h5 { margin-top: 0.4rem; color: #0f172a; }
      h4 { font-weight: 700; }
      p.lead { color: #475569; }
      .section { margin-top: 18px; }
      .card, .well, .ta-panel {
        border-radius: 16px;
        box-shadow: 0 12px 30px rgba(15, 23, 42, 0.06);
        border: 1px solid #e5e7eb;
        background: #ffffff;
      }
      .well { padding: 20px 18px; }
      .ta-panel { padding: 20px 24px; }
      .nav-tabs > li > a { font-weight: 600; color: #0f172a; }
      .nav-tabs > li.active > a, .nav-tabs > li > a:focus, .nav-tabs > li > a:hover {
        background: #f8faff;
        border-color: #d9e2ff;
        color: #1d4ed8;
      }
      .empty-state { max-width: 520px; margin-left: auto; margin-right: auto; }
      .empty-state-icon { font-size: 3rem; line-height: 1; }
      .empty-state h4 { font-weight: 700; }
      .ta-help-tooltip { cursor: help; display: inline-block; width: 100%; }
      .home-wrapper {
        min-height: calc(100vh - 180px);
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .home-wrapper .hero {
        max-width: 780px;
        width: 100%;
      }
      .home-steps h5 {
        font-weight: 700;
      }
      .home-steps p {
        margin-bottom: 0;
        color: #64748b;
      }
      .btn-primary {
        border-radius: 12px;
        box-shadow: 0 10px 20px rgba(48,86,211,0.25);
        transition: transform 0.1s ease, box-shadow 0.15s ease;
      }
      .btn-primary:hover {
        transform: translateY(-1px);
        box-shadow: 0 15px 30px rgba(48,86,211,0.3);
      }
      .form-control, .selectize-input {
        border-radius: 12px;
        border-color: #e2e8f0;
        box-shadow: none;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #3056d3;
        box-shadow: 0 0 0 3px rgba(48,86,211,0.12);
      }
      .dataTables_wrapper .dataTables_filter input,
      .dataTables_wrapper .dataTables_length select {
        border-radius: 10px;
        border: 1px solid #e2e8f0;
        padding: 6px 10px;
      }
      .table > thead > tr > th {
        background: #f8faff;
        color: #1f2937;
        border-bottom: 1px solid #e2e8f0;
      }
      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: #f8fbff;
      }
      pre.shiny-text-output {
        white-space: pre;
        overflow-x: auto;
        font-family: Fira Mono, Source Code Pro, Monaco, monospace;
        font-size: 0.9rem;
        background: #0f172a;
        color: #e2e8f0;
        border-radius: 12px;
        padding: 16px;
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
