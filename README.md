---
output:
  pdf_document: default
  html_document: default
---
# 🧪 Animal Trial Analyzer — Veterinarian Guide

Welcome to the **Animal Trial Analyzer**, a web-based Shiny application that helps veterinarians and animal health researchers evaluate intervention outcomes with confidence. No installation is required — simply open the app online and start analyzing your data.

---

## 🚀 Getting Started

### Access the App
The Animal Trial Analyzer is hosted on **[shinyapps.io](https://)** — just open the provided project link in your browser.  
No R or software setup is needed.

### Supported Browsers
Use a modern web browser such as **Chrome**, **Firefox**, **Edge**, or **Safari**.

---

## 📊 Preparing Your Data

Prepare a spreadsheet (Excel or CSV) with these recommended columns:

| Column | Description | Example |
| --- | --- | --- |
| `animal_id` | Unique ID for each animal | `Cow-101` |
| `treatment_group` | Treatment or control group | `Vaccine A`, `Placebo` |
| `time_point` | Observation time (day, week, etc.) | `Day 14` |
| `outcome_measure` | Primary measurement (e.g., antibody titer, weight) | `125` |
| `secondary_measure` *(optional)* | Additional outcomes (e.g., temperature) | `38.5` |
| `notes` *(optional)* | Veterinary observations | `Mild swelling at injection site` |

> 💡 **Tip:** Keep column names short and consistent. Avoid spaces or special characters other than underscores.

---

## 🧩 Using the App

### 1️⃣ Upload
- Go to the **Upload** tab.
- Choose your file (Excel or CSV).
- Preview and confirm that columns were detected correctly.
- Choose whether your data is in **long** or **wide** format — the app automatically reshapes as needed.

### 2️⃣ Filter
- Select specific treatments, species, or time points.
- Exclude animals or outliers as needed.

### 3️⃣ Analyze
Run statistical analyses directly in your browser:

**One-Way ANOVA**
- Compare a single outcome across treatment groups.

**Two-Way ANOVA**
- Study effects of two factors (e.g., treatment × time).

**Summary Statistics & Diagnostics**
- View group means, residuals, and assumption checks.

### 4️⃣ Visualize
- Generate boxplots, bar charts, and line graphs.
- Facet by treatment, time, or outcome.
- Export high-resolution figures for publications or reports.

---

## 📈 Interpreting Results

- **P-values:** Default threshold is *p* < 0.05.
- **Effect size:** Use alongside p-values for biological relevance.
- **Diagnostics:** Check residual and normality plots for model fit.

> 🩺 Combine statistical insights with clinical observations for a complete interpretation.

---

## ⚠️ Troubleshooting

| Problem | Possible Cause | Solution |
| --- | --- | --- |
| Data doesn’t upload | Unsupported format | Save as CSV or XLSX |
| Missing factor levels | Inconsistent group names | Standardize names before upload |
| Unexpected results | Violated model assumptions | Check diagnostics, consider transformation |

---

## 🧠 Best Practices

- Keep detailed clinical notes alongside numeric data.  
- Check group labels regularly for consistency.  
- Collaborate with statisticians for complex designs.

---

## 💬 Support

- For help, feedback, or feature requests, contact the project maintainer.  
- The full R source code is available for reference, but **no installation is needed** to use the web app.

---

Thank you for using the **Animal Trial Analyzer** to advance animal health and research!