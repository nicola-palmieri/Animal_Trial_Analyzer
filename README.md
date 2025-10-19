---
output:
  html_document: default
  pdf_document: default
---

# 🧪 Animal Trial Analyzer — Veterinarian Guide

The **Animal Trial Analyzer** is a point-and-click Shiny application that helps veterinarians interpret intervention trials without having to write R code. Upload routine monitoring spreadsheets, focus on the animals of interest, and produce shareable summaries, statistical reports, and publication-ready graphics in minutes.

---

## 🐾 Key capabilities

- Accepts **Excel workbooks** in either long or wide format and converts them automatically for analysis.
- Filters cohorts by **species, treatment, time point, health status, or any other column** before modelling.
- Runs core analyses used in veterinary field trials:
  - One-way or two-way ANOVA with optional Tukey post-hoc comparisons.
  - Linear models for continuous covariates.
  - Linear mixed models with a random intercept (e.g., repeated measures within animal or pen).
  - Pairwise correlation matrices for multivariate biomarker panels.
- Generates mean ± standard error plots that respect the model structure and stratification you choose.
- Exports results to **DOCX reports** and high-resolution **PNG figures** for case files or publications.

---

## 🚀 Getting started

1. **Open the app**
   - If you received a hosted link, open it in a modern browser (Chrome, Firefox, Edge, or Safari).
   - To run locally, open `app.R` in RStudio and click **Run App** (requires R 4.2+ and the packages listed in `app.R`).
2. **Review the example templates** in `data/toy_animal_trial_data_long.xlsx` and `data/toy_animal_trial_data_wide.xlsx` to see the expected structure.
3. Keep your workbook handy—the app guides you through four numbered tabs.

---

## 📋 Prepare your workbook

| Column | Purpose | Example |
| --- | --- | --- |
| `animal_id` | Unique identifier | `Cow-101` |
| `treatment_group` | Allocation or protocol | `Vaccine A` |
| `time_point` | Sampling moment | `Day 14` |
| `outcome_measure` | Primary outcome | `EPG` |
| Additional outcomes | Any numeric measurements (e.g., FAMACHA, weight, serology) | `3.5` |
| Optional notes | Clinical observations | `Mild swelling at injection site` |

- **Long format**: each row is one measurement for one animal at one time.
- **Wide format**: two header rows (outcome name + replicate). The app reshapes to long format automatically.
- Use concise column names (underscores instead of spaces) and consistent spelling for factor levels (e.g., `TreatmentA`, not `tmt A`).

---

## 🧭 Guided workflow

### 1️⃣ Upload
- Choose **Long** or **Wide** layout to see the corresponding template preview.
- Upload an Excel workbook (`.xlsx`, `.xls`, or `.xlsm`) and pick the worksheet to analyse.
- The app validates the file and shows a 5-row preview so you can confirm column names and data types.

### 2️⃣ Filter
- Select the columns you want to filter (e.g., `species`, `treatment_group`, `time_point`).
- For numeric fields, set minimum/maximum ranges; for factors or logical fields, tick the levels to keep.
- The filtered table updates instantly and is passed to every analysis tab.

### 3️⃣ Analyze
Pick the statistical tool that fits your study design:

| Tool | When to use it | Outputs |
| --- | --- | --- |
| **One-way ANOVA** | One categorical factor (e.g., treatment group) | Type III ANOVA table, Tukey post-hoc comparisons, stratified results if desired |
| **Two-way ANOVA** | Two categorical factors (e.g., treatment × time) | Type III ANOVA table, interactions, optional stratification |
| **Linear Model (LM)** | Continuous covariates or multiple fixed effects | Model summary, Type III ANOVA table, residual & Q-Q plots |
| **Linear Mixed Model (LMM)** | Random intercept for clustered data (e.g., repeated measures per animal) | Type III ANOVA, intraclass correlation, diagnostics, DOCX export |
| **Pairwise Correlation** | Explore relationships among multiple numeric outcomes | `GGally::ggpairs` matrix with scatter plots, correlation coefficients, and density curves |

Common features:
- Select one or multiple response variables. If multiple are chosen, the app automatically loops through them.
- **Advanced options** let you stratify the analysis (e.g., run separate models per herd). Up to 10 strata are supported.
- Each response (and each stratum, if used) gets its own tab with the model output and a **Download Results** button that saves a Word (`.docx`) report.
- ANOVA modules also provide a **Download All Results** button to bundle every response/stratum into a single document.

### 4️⃣ Visualize
- Produces mean ± standard error plots that mirror the model structure (factors, responses, and strata).
- Adjust subplot dimensions, grid rows, and columns to organise multiple responses or strata.
- For correlation analyses, the visualization tab displays the full pairwise matrix and allows resizing.
- Export the final figure as a **300 dpi PNG** ready for inclusion in clinical reports.

---

## 📦 Exports and reporting

- **Model reports**: Every analysis tab has download buttons that generate Word documents with ANOVA tables, post-hoc comparisons, model summaries, and diagnostics.
- **Figures**: The visualization tab saves PNG images sized according to your layout settings (default 300 × 200 px per panel).
- File names automatically include the date so you can archive runs by trial or cohort.

---

## 🛠 Troubleshooting

| Issue | What to check | Suggested fix |
| --- | --- | --- |
| Upload fails | File is not Excel or has hidden password protection | Save as `.xlsx` without protection and retry |
| Columns missing | Header spelling differs between sheets or includes trailing spaces | Standardise headings before upload; use `janitor::clean_names()` style naming |
| Filters remove all rows | Selected ranges or levels exclude every record | Reset filters or widen numeric bounds |
| Model errors | Not enough observations per group or singular random-effects structure | Reduce model complexity, ensure each level has data |
| Flat plots | Identical values across all animals or strata | Verify data entry and consider transforming the outcome |

---

## ✅ Best practices for veterinary teams

- Pair quantitative outputs with **clinical notes** to interpret biological relevance.
- Ensure each treatment group has sufficient sample size before running post-hoc tests.
- When using mixed models, confirm the random effect (e.g., `animal_id`) reflects the actual grouping in your protocol.
- Save exported DOCX and PNG files to your case management system to maintain a clear audit trail.
- Collaborate with a statistician for complex designs (e.g., repeated measures with unequal spacing or non-normal outcomes).

---

## 💬 Need help?

- Reach out to the project maintainer with questions or feature requests.
- The full R source code lives in this repository—feel free to review or adapt it for your clinic’s SOPs.

Thank you for using the **Animal Trial Analyzer** to support evidence-based animal health decisions!
