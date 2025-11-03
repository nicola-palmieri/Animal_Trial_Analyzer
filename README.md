# 📊 Table Analyzer (R/Shiny)

Table Analyzer is a modular R/Shiny application for end‑to‑end data analysis and visualization. It emphasizes **modern statistical practice** (e.g., PCA and **linear mixed models** instead of restrictive repeated‑measures ANOVA), enables **parallel analysis of multiple response variables**, supports **stratification** (analyze the same model within subgroups), and exports **publication‑ready tables and plots**.

- **Why it’s useful**
  - Uses **PCA** and **LMM** to model complex designs and correlations within experimental units.
  - **Parallel responses**: fit the same design across many numeric outcomes at once.
  - **Stratification**: automatically repeat analyses within selected subgroups (e.g., *Batch*, *Animal*, *Plate*).
  - **Reproducible**: formulas are shown in‑app; level ordering and contrasts are explicit; downloadable Word tables with formatted p‑values.
  - **Publication‑ready**: beautiful plots and journal‑style tables are one click away.

---

## 🔧 Installation

```r
# In R
install.packages(c(
  "shiny", "bslib", "dplyr", "tidyr", "ggplot2", "patchwork",
  "DT", "GGally", "skimr", "emmeans", "lmerTest", "car",
  "flextable", "officer", "zoo", "shinyjqui"
  # ggrepel is used via ggrepel::geom_text_repel in the PCA plot; install if needed:
  # "ggrepel"
))

# Run the app (repository root)
shiny::runApp(".")
```

The app sources all module files from the `R/` directory at startup.

---

## 🚀 Quick start

1. **Upload** a tidy table (CSV/TSV/Excel) or use the demo dataset.
2. **Filter** rows and columns of interest.
3. **Analyze** using:
   - Descriptive Statistics
   - One‑way ANOVA
   - Two‑way ANOVA
   - Linear Model (LM)
   - Linear Mixed Model (LMM)
   - Pairwise Correlation
   - Principal Component Analysis (PCA)
4. **Visualize** results (pairwise matrices, PCA biplots, descriptive plots).
5. **Export**: Download results as text or as a Word (`.docx`) report with journal‑style tables.

---

## 📂 Data expectations

- Numeric responses must be in numeric columns.
- Categorical predictors (factors) should be factors or character columns.
- Missing values are tolerated; modules handle complete‑case subsets where required.
- **Stratification**: choose one factor (max ~10 levels recommended by the UI) to run the same analysis within each level.

---

## 🧭 Workflow & UI

- **1️⃣ Upload**: choose example or upload your file; large uploads supported (max request size increases).
- **2️⃣ Filter**: subset columns/rows interactively.
- **3️⃣ Analyze**: pick an analysis type. Most modules support:
  - **Multiple responses**: select several numeric columns at once.
  - **Stratification**: run per‑level analyses (with order control).
  - **Level order** controls ensure the factor reference level is explicit.
- **4️⃣ Visualize**: dedicated plotting panels per analysis (pairwise correlation, PCA biplots, descriptive summaries).
- **Downloads**: each module provides “Download all results” buttons (text or Word).

---

## 🧪 Methods (what the app runs under the hood)

Below are the exact R functions/packages/parameters used by each analysis. This section is intended for peer reviewers and readers replicating your results.

### Descriptive Statistics
- **Summaries** computed with:
  - `skimr::skim()` for a compact overview of variables (grouped by stratum when set).
  - **Coefficient of Variation (CV %)** per numeric column: `100 * sd(x, na.rm=TRUE) / mean(x, na.rm=TRUE)` using `dplyr::summarise()`.
  - **Outlier counts** per numeric column using the 1.5×IQR rule on quartiles from `stats::quantile()`.
  - **Missingness (%)** per numeric column: `100 * mean(is.na(x))`.
  - **Normality** via `stats::shapiro.test(x)$p.value` (computed per stratum when applicable).
- **Outputs**: Printed text summary sections; downloadable text file.

### Pairwise Correlation
- Pairwise matrix plotted with **`GGally::ggpairs()`**:
  - `upper = GGally::wrap("cor", size = 4, colour/color = <palette color>)`
  - `lower = GGally::wrap("points", alpha = 0.6, size = 1.5, colour/color = <palette color>)`
  - `diag  = GGally::wrap("densityDiag", fill = <palette color>, alpha = 0.4)`
- Grid export with `ggplot2::ggsave(..., dpi = 300)`.

### Principal Component Analysis (PCA)
- Model: `stats::prcomp(X, center = TRUE, scale. = TRUE)` on complete rows of the selected numeric variables.
- Printed outputs include `summary(prcomp_obj)`, **rotation** (loadings), and **explained variance** (%).
- Visualization: a **biplot** built via `ggplot2` using the first two PCs; optional loadings as arrows with labels via `ggrepel::geom_text_repel`.
- **Stratified PCA**: when a stratum is selected, the PCA is fit within each subgroup independently and reported per‑level.

### One‑way ANOVA
- Per‑response model: `stats::aov(y ~ group, data)`.
- **Type‑III ANOVA table** for display/post‑hoc: `car::Anova(model, type = 3)` with contrasts temporarily set to sum‑to‑zero (`options(contrasts = c("contr.sum","contr.poly"))` during table prep).
- **Post‑hoc** pairwise comparisons: `emmeans::emmeans(model, specs = "group")` + `emmeans::contrast(..., method = "revpairwise", adjust = "tukey")`.
- **Stratification**: fits the same ANOVA within each selected stratum (level ordering is respected).

### Two‑way ANOVA
- Per‑response model: `stats::aov(y ~ A * B, data)` if both factors chosen (includes main effects and interaction). When only one factor is available, it reduces to one‑way.
- **Type‑III ANOVA**: `car::Anova(model, type = 3)` with sum contrasts for interpretability of main effects in presence of interactions.
- **Post‑hoc** for each factor via `emmeans` (Tukey‑adjusted, `revpairwise`).

### Linear Model (LM)
- Fit: `stats::lm(y ~ fixed + covariates + interactions, data)`.
- **Type‑III** ANOVA for fixed effects: `car::Anova(model, type = 3)`.
- Displayed outputs include `summary(model)` (coefficients) and the Type‑III table.
- **Multiple responses** and **stratification** supported.

### Linear Mixed Model (LMM)
- Fit: `lmerTest::lmer(y ~ fixed + covariates + interactions + (1|Random), data)` (single random intercept via formula `(1|...)`).
- **Type‑III** for fixed effects: `anova(model, type = 3)` (from `lmerTest`).
- **Random‑effects variance** summary: `lme4::VarCorr(model)`.
- **Intraclass Correlation (ICC)** reported from variance components: for each grouping factor, `ICC = var_random / (var_random + var_residual)`.
- **Multiple responses** and **stratification** supported.

> **Notes on contrasts and reference levels**
>
> - For ANOVA/LM displays, Type‑III tables are produced with **sum contrasts** to make main effects interpretable. Factor **level order** controls in the UI set the reference level (first level = reference).

---

## 🖼️ Visualization modules

- **Pairwise correlation**: scatter/density/correlation panels (see above).
- **PCA biplot**: points colored/shaped/labeled by selected categorical variables; optional loadings arrows and labels; adjustable plot size; multi‑panel layout for stratified results.
- **Descriptive plots**: categorical barplots, numeric histograms/densities, boxplots, CV %, outlier counts, and missingness % views; layouts composed using `patchwork::wrap_plots`.

---

## 🧾 Reporting & Export

- **Text outputs**: Each module provides a “Download all results” text file (e.g., combined ANOVA tables, PCA summaries).
- **Word reports (`.docx`)** for LM/LMM:
  - Built with **`officer` + `flextable`** (journal style; bold p‑values; auto‑fit width).
  - Sections include **Type‑III ANOVA**, **Model coefficients**, **Random‑effects & ICC** (for LMM), and a footer indicating generation date.
  - Example call in code path: `write_lm_docx(model, file, subtitle = "Stratum: ...")`.

---

## ✳️ Reproducibility checklist

- **Model formulas** are printed in the UI (`y ~ ...`).  
- **Factor level order** is explicitly selectable and used in modeling.  
- **Contrasts** for Type‑III tables use sum‑to‑zero during table preparation.  
- **Stratification** repeats the entire workflow within each selected level.  
- **Multiple responses**: identical design is fit across all chosen numeric outcomes.  
- **Downloadables** include all the summaries/tables needed for peer review.

---

## 🧪 Citing the app

If you use Table Analyzer in your work, please cite the repository URL in your Methods and provide the exact module names you used (e.g., “Linear Mixed Model with (1|Animal) random intercept; Type‑III ANOVA; Tukey‑adjusted pairwise contrasts via `emmeans`”).

**Example**:
> Analyses were performed in Table Analyzer (R/Shiny app, v\<commit\>) using:
> PCA (`prcomp`, centered & scaled), One‑way ANOVA (`aov` with Type‑III from `car` and Tukey contrasts via `emmeans`), Linear Mixed Models (`lmerTest::lmer` with (1|Subject) random intercept and Type‑III tests), and pairwise correlation (`GGally::ggpairs`).

---

## 📦 Packages

Core packages: `shiny`, `bslib`, `dplyr`, `tidyr`, `ggplot2`, `patchwork`, `DT`, `GGally`, `skimr`, `emmeans`, `lmerTest`, `car`, `flextable`, `officer`, `zoo`, `shinyjqui`  
Optional: `ggrepel` (loadings labels in PCA biplot via namespaced call).

---

## 📝 License

MIT (or fill in your project’s license).

---

## 🙏 Acknowledgments

Built by the Table Analyzer team. Inspired by best practices for transparent statistical reporting and reproducible research.
