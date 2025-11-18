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
    primary = "#0b7285",
    secondary = "#6c757d",
    success = "#2fb344",
    info = "#1098ad",
    warning = "#f08c00",
    danger = "#c92a2a",
    base_font = font_google("Inter", 500),
    heading_font = font_google("Plus Jakarta Sans", 600),
    code_font = font_google("Fira Code")
  ),
  
    # ---- Custom CSS (copied from website) ----
    header = tags$head(
      tags$style(HTML("\
        body {
          background: radial-gradient(circle at 20% 20%, rgba(16, 152, 173, 0.08), transparent 35%),
                      radial-gradient(circle at 80% 10%, rgba(247, 162, 6, 0.06), transparent 30%),
                      #f8f9fb;
          color: #1f2d3d;
          min-height: 100vh;
        }
        .container-fluid { max-width: 1280px; margin: auto; }
        .navbar.navbar-default {
          border: none;
          box-shadow: 0 8px 20px rgba(15, 23, 42, 0.08);
          background: linear-gradient(135deg, #ffffff 0%, #f3f7fb 100%);
        }
        .navbar-brand { font-weight: 700; letter-spacing: -0.02em; }
        .navbar-nav > li > a { padding: 16px 18px; font-weight: 600; }
        .navbar-nav > .active > a,
        .navbar-nav > .active > a:focus,
        .navbar-nav > .active > a:hover {
          background: transparent;
          border-bottom: 3px solid #0b7285;
          color: #0b7285;
        }
        .nav > li > a:hover { color: #0b7285; }
        .hero {
          background: linear-gradient(145deg, #ffffff 0%, #f2f8fb 100%);
          border-radius: 20px;
          padding: 48px 32px;
          margin-top: 20px;
          box-shadow: 0 16px 36px rgba(12, 32, 63, 0.08);
          border: 1px solid rgba(12, 32, 63, 0.06);
        }
        h1, h2, h3 { margin-top: 0.4rem; color: #0f172a; }
        .section { margin-top: 18px; }
        .card, .well {
          border-radius: 18px;
          border: 1px solid rgba(12, 32, 63, 0.08);
          box-shadow: 0 12px 28px rgba(15, 23, 42, 0.06);
          background: #ffffff;
        }
        .nav-tabs { border-bottom: 1px solid rgba(15, 23, 42, 0.08); }
        .nav-tabs > li > a { font-weight: 600; color: #4b5563; padding: 12px 18px; }
        .nav-tabs > li > a:hover { border-color: transparent; color: #0b7285; }
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          color: #0b7285;
          border: none;
          border-bottom: 3px solid #0b7285;
          background: transparent;
        }
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
          max-width: 780px;
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
        .form-control, .selectize-input, .selectize-control.single .selectize-input {
          border-radius: 12px;
          border: 1px solid rgba(15, 23, 42, 0.12);
          box-shadow: none;
          padding: 10px 12px;
          transition: all 0.2s ease;
        }
        .form-control:focus, .selectize-input.focus {
          border-color: #0b7285;
          box-shadow: 0 0 0 4px rgba(11, 114, 133, 0.15);
        }
        .btn-primary {
          border-radius: 12px;
          border: none;
          box-shadow: 0 10px 24px rgba(11, 114, 133, 0.3);
          padding: 12px 16px;
          font-weight: 700;
        }
        .btn-default, .btn-secondary {
          border-radius: 12px;
          border: 1px solid rgba(15, 23, 42, 0.08);
          background: #ffffff;
          font-weight: 600;
          color: #0f172a;
        }
        .btn-default:hover, .btn-secondary:hover { color: #0b7285; border-color: rgba(11, 114, 133, 0.4); }
        .panel-heading { border-bottom: 1px solid rgba(15, 23, 42, 0.08); font-weight: 600; }
        .well { background: linear-gradient(180deg, #ffffff 0%, #f7fbfd 100%); }
        .tab-content { padding-top: 12px; }
        table.dataTable thead th { background: #f8fafc; font-weight: 700; }
        table.dataTable tbody tr:hover { background-color: rgba(11, 114, 133, 0.05); }
        .table > thead > tr > th { border-bottom: 2px solid rgba(15, 23, 42, 0.08); }
        .badge { padding: 8px 12px; border-radius: 999px; font-weight: 700; }
        .ta-card-title {
          display: flex;
          align-items: center;
          gap: 10px;
          font-weight: 700;
          color: #0f172a;
        }
        .ta-faint { color: #6b7280; }
        .home-actions { display: flex; gap: 12px; justify-content: center; flex-wrap: wrap; }
        .home-actions .btn { min-width: 180px; }
      "))
    ),

  tabPanel(
    title = tagList(icon("home"), " Home"),
    value = "home",
    home_ui("home")
  ),

  tabPanel(
    title = tagList(icon("upload"), " Upload"),
    value = "upload",
    fluidPage(upload_ui("upload"))
  ),
  tabPanel(
    title = tagList(icon("filter"), " Filter"),
    value = "filter",
    fluidPage(filter_ui("filter"))
  ),
  tabPanel(
    title = tagList(icon("chart-line"), " Analyze"),
    value = "analysis",
    fluidPage(analysis_ui("analysis"))
  ),
  tabPanel(
    title = tagList(icon("chart-area"), " Visualize"),
    value = "visualize",
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
