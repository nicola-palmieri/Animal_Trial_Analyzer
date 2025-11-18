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
    heading_font = font_google("Inter", weight = 700),
    primary = "#2563eb",
    secondary = "#0ea5e9",
    success = "#16a34a",
    warning = "#f59e0b"
  ),
  
  # ---- Custom CSS (copied from website) ----
  header = tags$head(
    tags$style(HTML(" 
      body {
        background: radial-gradient(circle at 20% 20%, rgba(37,99,235,0.08), transparent 25%),
                    radial-gradient(circle at 80% 0%, rgba(14,165,233,0.08), transparent 20%),
                    #f5f7fb;
      }
      .container-fluid { max-width: 1200px; margin: auto; }
      .navbar-default {
        background: linear-gradient(90deg, #1d4ed8 0%, #2563eb 50%, #0ea5e9 100%);
        border: none;
        box-shadow: 0 6px 18px rgba(0,0,0,0.08);
      }
      .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {
        color: #f8fafc !important;
        font-weight: 600;
      }
      .navbar-default .navbar-nav > .active > a,
      .navbar-default .navbar-nav > li > a:focus,
      .navbar-default .navbar-nav > li > a:hover {
        background: rgba(255,255,255,0.12) !important;
        color: #fff !important;
      }
      .hero {
        background: linear-gradient(135deg, #f8fafc 0%, #e0f2fe 100%);
        border-radius: 20px;
        padding: 48px 32px;
        margin-top: 24px;
        box-shadow: 0 14px 30px rgba(0,0,0,0.08);
      }
      h1, h2, h3 { margin-top: 0.4rem; }
      h4 { font-weight: 700; color: #0f172a; }
      p.lead { color: #475569; }
      .section { margin-top: 18px; }
      .card {
        border-radius: 16px;
        box-shadow: 0 10px 24px rgba(15,23,42,0.08);
        border: 1px solid #e2e8f0;
      }
      .ta-panel {
        background: #fff;
        border-radius: 18px;
        border: 1px solid #e2e8f0;
        box-shadow: 0 10px 24px rgba(15,23,42,0.08);
        padding: 22px;
      }
      .ta-panel h4 { margin-top: 4px; }
      .nav-tabs > li > a { font-weight: 600; }
      .nav > li > a { padding: 14px 18px; border-radius: 12px; }
      .empty-state { max-width: 420px; margin-left: auto; margin-right: auto; }
      .empty-state-icon { font-size: 3rem; line-height: 1; }
      .empty-state h4 { font-weight: 700; color: #0f172a; }
      .ta-help-tooltip { cursor: help; display: inline-block; width: 100%; }
      .home-wrapper {
        min-height: calc(100vh - 180px);
        display: flex;
        align-items: center;
        justify-content: center;
      }
      .home-wrapper .hero {
        max-width: 820px;
        width: 100%;
      }
      .home-steps h5 {
        font-weight: 700;
      }
      .home-steps p {
        margin-bottom: 0;
        color: #6b7280;
      }
      .home-steps .fa-2x { color: #2563eb; }
      .ta-badge {
        display: inline-flex;
        align-items: center;
        gap: 6px;
        padding: 6px 12px;
        border-radius: 999px;
        background: rgba(37,99,235,0.12);
        color: #1e3a8a;
        font-weight: 600;
        font-size: 0.9rem;
      }
      pre.shiny-text-output {
        white-space: pre;
        overflow-x: auto;
        font-family: Fira Mono, Source Code Pro, Monaco, monospace;
        font-size: 0.9rem;
        background: #0f172a;
        color: #e2e8f0;
        padding: 12px;
        border-radius: 12px;
      }
      .well { background: #f8fafc; border-radius: 12px; }
      .form-control, .selectize-input, .selectize-dropdown-content {
        border-radius: 12px !important;
      }
      .btn-primary {
        background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%);
        border: none;
        box-shadow: 0 10px 18px rgba(37,99,235,0.35);
      }
      .btn-default {
        border-radius: 10px;
      }
      .table { border-radius: 12px; overflow: hidden; }
      .dataTables_wrapper .dataTables_filter input { border-radius: 12px; }
      .help-block, .text-muted { color: #64748b; }
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
