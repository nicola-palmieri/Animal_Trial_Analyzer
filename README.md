# 📊 Table Analyzer (R/Shiny)

Table Analyzer is a modular R/Shiny application that walks researchers from raw spreadsheets to publication-ready figures and tables. The current release streamlines each step of the workflow with dedicated modules for uploading, filtering, modeling, and visualization.

---

## ✨ Highlights

- **Excel-native uploads**
  - Accepts long-format workbooks or wide-format plates with two header rows (response × replicate). Wide sheets are reshaped automatically and validated for duplicate measurements.
  - Bundled demo datasets illustrate both layouts and can be loaded instantly from the UI.
  - Ambiguous numeric columns (≤10 distinct values) can be re-typed as categorical factors directly in the upload panel, and all character columns are ordered automatically with `janitor::clean_names()` preprocessing.
- **Interactive filtering**
  - Choose any subset of columns, then refine rows with auto-generated range sliders (numeric), checkboxes (logical), or multi-select pickers (categorical).
  - The filtered preview updates live and feeds downstream modules.
- **Analysis hub**
  - Modules: Descriptive statistics, One-way ANOVA, Two-way ANOVA, Linear Model (LM), Linear Mixed Model (LMM), Pairwise Correlation, and Principal Component Analysis (PCA).
  - LM/LMM support **multi-response fitting**, optional covariates, two-way interaction toggles, random-intercept selection (LMM), and per-analysis stratification (run the same model within each level of a factor).
  - Each analysis reports formulas, tidy summaries, Type-III ANOVA tables, and downloadable `.docx` reports (LM/LMM) with formatted coefficients, random-effects variance, and ICC.
- **Visualization gallery**
  - Dedicated panels mirror the active analysis: descriptive dashboards, PCA biplots with optional loadings, correlation pair grids (`GGally::ggpairs`), and ANOVA effect plots.
  - Built-in color palettes can be customized per grouping level.
- **Reproducibility first**
  - Model formulas and factor level orders are always explicit.
  - Stratified analyses repeat the full pipeline per subgroup, and download bundles include every table shown in the UI.

---

## 🧭 App workflow

1. **Upload** (Tab “1️⃣ Upload”)
   - Select the example dataset or upload Excel workbooks (`.xlsx`, `.xls`, `.xlsm`).
   - For wide layouts, Table Analyzer reshapes the sheet to tidy long format after reconciling multi-row headers.
   - Review validation messages and the live preview before proceeding.
2. **Filter** (Tab “2️⃣ Filter”)
   - Pick the columns you care about and adjust numeric ranges or factor selections to create the analysis-ready subset.
3. **Analyze** (Tab “3️⃣ Analyze”)
   - Choose a module and configure responses, predictors, covariates, interactions, stratification, and (for LMM) random intercepts.
   - Click **Show results** to run the model; export everything with **Download all results**.
4. **Visualize** (Tab “4️⃣ Visualize”)
   - Explore plots tailored to the active analysis, including multi-panel layouts for stratified fits and customizable color themes.

---

## 🔧 Installation & local launch

```r
# Install core packages (run once)
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "patchwork",
  "DT", "GGally", "skimr", "emmeans", "lmerTest", "car",
  "flextable", "officer", "zoo", "shinyjqui", "janitor"
  # Optional: ggrepel for PCA loadings labels
))

# Launch the app from the repository root
shiny::runApp(".")
```

The app auto-sources all modules from the `R/` directory, bumps the upload size limit to 200 MB, and enables Shiny autoreload for development.

---

## 📂 Data expectations

- Numeric responses should be stored in numeric columns.
- Factors can be provided as factors or characters; level order controls in the Analysis tab set the reference.
- Missing values are accepted—modules fall back to complete-case subsets where necessary.
- **Stratification** is optional but available across modules; for best readability, keep stratum levels to ≲10.

---

## 📦 Exports & reporting

- Every module exposes a “Download all results” button that bundles the text outputs currently displayed.
- LM/LMM exports generate Word (`.docx`) reports with ANOVA tables, model coefficients, random-effects variance (if applicable), and ICC summaries.
- PCA, correlation, and descriptive visuals can be saved via each plot’s built-in download controls.

---

## 🧪 Development notes

- Regression exports rely on `flextable` and `officer`; install these packages to avoid runtime errors.
- Wide-format ingestion is safeguarded by unit tests in `tests/test_convert_wide_to_long.R`. Run them with:
  ```bash
  Rscript tests/test_convert_wide_to_long.R
  ```
- Helper scripts in `dev/` illustrate layout prototypes and can be sourced during development, but are not required for production use.

---

## 📝 License

MIT (or update with your project’s chosen license).

---

## 🙏 Acknowledgments

Built by the Table Analyzer team. Inspired by best practices for transparent statistical reporting and reproducible research.
