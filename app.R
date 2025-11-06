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
  title = "📊 Table Analyzer",
  id = "main_nav",
  theme = bs_theme(bootswatch = "flatly"),
  
  # ---- Custom CSS (copied from website) ----
  header = tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 100%; margin: auto; }
      .hero {
        background: linear-gradient(135deg, #f8f9fa 0%, #e9f5ff 100%);
        border-radius: 16px; padding: 20px 24px;
      }
      h1, h2, h3 { margin-top: 0.4rem; }
      .section { margin-top: 18px; }
      .card { border-radius: 16px; box-shadow: 0 4px 10px rgba(0,0,0,0.05); }
      .nav-tabs > li > a { font-weight: 500; }
    "))
  ),
  
  tabPanel(
    "1️⃣ Upload",
    fluidPage(upload_ui("upload"))
  ),
  tabPanel(
    "2️⃣ Filter",
    fluidPage(filter_ui("filter"))
  ),
  tabPanel(
    "3️⃣ Analyze",
    fluidPage(analysis_ui("analysis"))
  ),
  tabPanel(
    "4️⃣ Visualize",
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
