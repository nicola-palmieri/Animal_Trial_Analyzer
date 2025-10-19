# ===============================================================
# 🧩 Helpers for LM
# ===============================================================

write_lm_docx <- function(model, file) {

  # Create new document
  doc <- read_docx()
  
  # ---- Title ----
  dep_var <- all.vars(formula(model))[1]
  title_text <- paste("Linear Model Results —", dep_var)
  doc <- body_add_fpar(
    doc,
    fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12)))
  )
  
  # Add spacing
  doc <- body_add_par(doc, "", style = "Normal")
  
  # ==========================================================
  # 🔹 ANOVA (Type III)
  # ==========================================================
  
  doc <- body_add_fpar(
    doc,
    fpar(ftext("ANOVA (Type III)", prop = fp_text(bold = TRUE)))
  )
  
  doc <- body_add_par(doc, "", style = "Normal")
  
  anova_tbl <- as.data.frame(car::Anova(model, type = 3))
  anova_tbl <- tibble::rownames_to_column(anova_tbl, "Effect")
  
  # Round numeric columns
  for (col in names(anova_tbl)) {
    if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 3)
  }
  
  ft_anova <- flextable(anova_tbl)
  ft_anova <- bold(ft_anova, part = "header")
  ft_anova <- set_table_properties(ft_anova, width = .9, layout = "autofit")
  ft_anova <- theme_vanilla(ft_anova)
  ft_anova <- fontsize(ft_anova, size = 10, part = "all")
  doc <- body_add_flextable(doc, ft_anova)
  
  # Add spacing
  doc <- body_add_par(doc, "", style = "Normal")
  
  # ==========================================================
  # 🔹 Model Coefficients
  # ==========================================================
  
  doc <- body_add_fpar(
    doc,
    fpar(ftext("Model Coefficients", prop = fp_text(bold = TRUE)))
  )
  
  doc <- body_add_par(doc, "", style = "Normal")
  
  coef_tbl <- as.data.frame(summary(model)$coefficients)
  coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
  names(coef_tbl) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
  
  for (col in names(coef_tbl)) {
    if (is.numeric(coef_tbl[[col]])) coef_tbl[[col]] <- round(coef_tbl[[col]], 4)
  }
  
  ft_coef <- flextable(coef_tbl)
  ft_coef <- bold(ft_coef, part = "header")
  ft_coef <- set_table_properties(ft_coef, width = .9, layout = "autofit")
  ft_coef <- theme_vanilla(ft_coef)
  ft_coef <- fontsize(ft_coef, size = 10, part = "all")
  doc <- body_add_flextable(doc, ft_coef)
  
  # ==========================================================
  # 🔹 Footer
  # ==========================================================
  
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_par(doc, "Significance level: p < 0.05 (bold values).", style = "Normal")
  
  # Write file
  print(doc, target = file)
}

