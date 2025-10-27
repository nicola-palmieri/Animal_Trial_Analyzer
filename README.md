# 🧪 Table Analyzer

The **Table Analyzer** is a Shiny dashboard that guides veterinary teams from raw monitoring spreadsheets to exploratory statistics, downloadable model summaries, and publication-ready visualisations. The code in this repository reflects the current production app and is ready to run locally in R.

---

## 📌 Project status

- **Maturity:** Stable for routine use in-house. Feature work is ongoing, but the current UI/UX mirrors the deployed tool.
- **Deployment:** No hosted instance is shipped in this repository—run locally via `app.R` or publish to your own Shiny Server / Posit Connect deployment.
- **Maintenance:** Issues and feature requests are handled on a rolling basis. Contributions are welcome through pull requests.

---

## 🚀 Quick start

### Requirements

- R 4.2 or newer.
- The CRAN packages loaded in [`app.R`](app.R): `bslib`, `dplyr`, `DT`, `flextable`, `GGally`, `ggplot2`, `lmerTest`, `officer`, `patchwork`, `shiny`, `shinyjqui`, `skimr`, and `tidyr`.

Install any missing packages with:

```r
install.packages(c(
  "bslib", "dplyr", "DT", "flextable", "GGally", "ggplot2", "lmerTest",
  "officer", "patchwork", "shiny", "shinyjqui", "skimr", "tidyr"
))
```

### Run the app locally

1. Open the project in RStudio or start an R session in the repository root.
2. Source `app.R` or click **Run App** in RStudio.
3. Navigate the four guided tabs (`Upload → Filter → Analyze → Visualize`).

Sample workbooks in [`data/`](data) demonstrate the expected long- and wide-format inputs.

---

## 🧭 Guided workflow

### 1️⃣ Upload

- Supports Excel workbooks (`.xlsx`, `.xls`, `.xlsm`).
- Provides templates for long and two-row wide layouts before you upload.
- Displays a 5-row preview with detected column types so you can verify the import.

### 2️⃣ Filter

- Filter by any column (categorical, numeric, logical).
- Apply range filters, include/exclude factor levels, and keep notes columns for context.
- The filtered dataset is shared with every downstream analysis module.

### 3️⃣ Analyze

Select from the following analysis modules—each ships with bespoke configuration inputs and downloadable reports:

| Module | Typical use case | Key outputs |
| --- | --- | --- |
| **Descriptive Statistics** | Quick cohort summaries before modelling | Flextable summary tables, missingness and distribution diagnostics |
| **One-way ANOVA** | Compare one categorical factor | Type III ANOVA, Tukey post-hoc tests, per-response DOCX exports |
| **Two-way ANOVA** | Factorial designs (e.g. treatment × time) | Type III ANOVA, interaction plots, combined DOCX exports |
| **Linear Model (LM)** | Continuous covariates or additive fixed effects | Model summary, ANOVA table, diagnostic plots |
| **Linear Mixed Model (LMM)** | Repeated measures or clustered designs | `lmerTest` fit, intraclass correlation, diagnostics, Word export |
| **Pairwise Correlation** | Explore multivariate biomarker panels | `GGally::ggpairs` matrix with scatter, density, and correlation cells |
| **Principal Component Analysis (PCA)** | Dimensionality reduction and outlier checks | Scree plots, loadings tables, biplots |

Advanced options allow response batching, stratified analyses (up to 10 strata), manual factor ordering, and templated report downloads for each response/stratum combination.

### 4️⃣ Visualize

- Generates mean ± standard error plots that respect the selected model structure and strata.
- Automatically adapts layout controls for ANOVA grids, correlation matrices, and PCA biplots.
- Lets you override panel dimensions and grid arrangements before downloading high-resolution PNG files.

---

## 📋 Preparing your workbook

| Column | Purpose | Example |
| --- | --- | --- |
| `animal_id` | Unique identifier | `Cow-101` |
| `treatment_group` | Allocation or protocol | `Vaccine A` |
| `time_point` | Sampling moment | `Day 14` |
| `outcome_measure` | Primary outcome | `EPG` |
| Additional outcomes | Numeric measurements (e.g., FAMACHA, weight, serology) | `3.5` |
| Optional notes | Clinical observations | `Mild swelling at injection site` |

- **Long format:** one row per measurement, per animal, per time point.
- **Wide format:** two header rows (outcome name + replicate); the app reshapes to long format for you.
- Use consistent naming (underscores over spaces) and harmonise factor levels to avoid filtering mismatches.

---

## 📦 Outputs

- **Word reports (`.docx`)** for every model tab, capturing statistical tables, diagnostics, and stratified summaries.
- **PNG figures** sized according to your chosen layout (300 dpi by default) for inclusion in reports or presentations.
- Auto-generated filenames include the analysis type, response variable, and date stamp for easy archiving.

---

## 🛠 Troubleshooting & tips

| Issue | What to check | Suggested fix |
| --- | --- | --- |
| Upload fails | File is not Excel or is password protected | Save as `.xlsx` without protection and retry |
| Columns missing | Header spelling differs or trailing spaces exist | Normalise headers (e.g., `janitor::clean_names()`) |
| Filters remove all rows | Filters exclude every record | Reset filters or widen numeric bounds |
| Model errors | Groups have too few observations or random effects are singular | Simplify the model or collect more data per level |
| Flat plots | Little variation in the selected outcome | Verify data entry or consider transformations |

- Pair quantitative outputs with clinical notes to interpret biological relevance.
- Confirm random-effect identifiers (e.g., `animal_id`) match the true grouping structure before running LMMs.
- Save exported documents to your case management system for audit trails.

---

## 🤝 Contributing

1. Fork the repository and create a feature branch.
2. Update or add tests if you modify analysis logic.
3. Open a pull request describing the change and attach screenshots when altering UI elements.

---

## 💬 Support

Open an issue if you encounter bugs or would like to propose enhancements. For deployment questions, mention your target environment (e.g., Shiny Server, Posit Connect) so we can point you toward the relevant configuration notes.

Thank you for using the **Animal Trial Analyzer** to support evidence-based animal health decisions!

