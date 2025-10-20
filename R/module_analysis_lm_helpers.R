# ===============================================================
# 🧩 Helpers for LM
# ===============================================================

write_lm_docx <- function(model, file) {

  # Create new document
  doc <- read_docx()
  is_lmm <- inherits(model, "merMod")

  # ---- Title ----
  dep_var <- all.vars(formula(model))[1]
  title_text <- sprintf(
    "%s Results — %s",
    if (is_lmm) "Linear Mixed Model" else "Linear Model",
    dep_var
  )
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

  if (is_lmm) {
    anova_tbl <- as.data.frame(anova(model, type = 3))
  } else {
    anova_tbl <- as.data.frame(car::Anova(model, type = 3))
  }
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
  # 🔹 Random effects & ICC (LMM only)
  # ==========================================================
  if (is_lmm) {
    doc <- body_add_fpar(
      doc,
      fpar(ftext("Random Effects", prop = fp_text(bold = TRUE)))
    )
    doc <- body_add_par(doc, "", style = "Normal")
    
    rand_df <- as.data.frame(lme4::VarCorr(model))
    if (nrow(rand_df) > 0) {
      rand_df <- rand_df[, c("grp", "var1", "var2", "vcov", "sdcor"), drop = FALSE]
      rand_df$var2 <- ifelse(is.na(rand_df$var2), "-", rand_df$var2)
      names(rand_df) <- c("Grouping", "Effect 1", "Effect 2", "Variance", "Std. Dev.")
      for (col in c("Variance", "Std. Dev.")) {
        rand_df[[col]] <- round(rand_df[[col]], 3)
      }
      ft_rand <- flextable(rand_df)
      ft_rand <- bold(ft_rand, part = "header")
      ft_rand <- set_table_properties(ft_rand, width = .9, layout = "autofit")
      ft_rand <- theme_vanilla(ft_rand)
      ft_rand <- fontsize(ft_rand, size = 10, part = "all")
      doc <- body_add_flextable(doc, ft_rand)
    } else {
      doc <- body_add_par(doc, "No random-effect variance components were estimated.", style = "Normal")
    }
    
    if (exists("compute_icc") && is.function(compute_icc)) {
      icc_df <- compute_icc(model)
    } else {
      icc_df <- NULL
    }
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      doc <- body_add_par(doc, "", style = "Normal")
      doc <- body_add_fpar(
        doc,
        fpar(ftext("Intraclass Correlation (ICC)", prop = fp_text(bold = TRUE)))
      )
      doc <- body_add_par(doc, "", style = "Normal")
      icc_df$ICC <- round(icc_df$ICC, 3)
      icc_ft <- flextable(icc_df)
      icc_ft <- bold(icc_ft, part = "header")
      icc_ft <- set_table_properties(icc_ft, width = .5, layout = "autofit")
      icc_ft <- theme_vanilla(icc_ft)
      icc_ft <- fontsize(icc_ft, size = 10, part = "all")
      doc <- body_add_flextable(doc, icc_ft)
    }
    
    doc <- body_add_par(doc, "", style = "Normal")
  }
  
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
  names(coef_tbl)[1] <- "Term"
  names(coef_tbl) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|t|)", names(coef_tbl))
  
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
