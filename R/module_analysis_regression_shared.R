# ===============================================================
# 🔧 Shared helpers for LM/LMM (UI + server utilities)
# ===============================================================

# --- Type detection (robust for Excel imports) ---
reg_detect_types <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  fac_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  list(num = num_vars, fac = fac_vars)
}

# --- Variable selectors UI (optionally expose a random effect) ---
reg_variable_selectors_ui <- function(ns, types, allow_random = FALSE) {
  out <- list(
    selectInput(ns("dep"), "Response variable (numeric):", choices = types$num),
    selectInput(ns("fixed"), "Categorical predictors:", choices = types$fac, multiple = TRUE),
    selectInput(ns("covar"), "Numeric predictors:", choices = types$num, multiple = TRUE)
  )
  if (allow_random) {
    out <- c(out, list(
      selectInput(ns("random"), "Random effect (categorical):", choices = types$fac, selected = NULL)
    ))
  }
  do.call(tagList, out)
}

# --- 2-way interaction checkbox list (categorical × categorical only) ---
reg_interactions_ui <- function(ns, fixed, fac_vars) {
  if (is.null(fixed) || length(fixed) < 2) return(NULL)
  cats_only <- intersect(fixed, fac_vars)
  if (length(cats_only) < 2) return(NULL)
  pairs <- combn(cats_only, 2, simplify = FALSE)
  pair_labels <- vapply(pairs, function(p) paste(p, collapse = " × "), character(1))
  pair_values <- vapply(pairs, function(p) paste(p, collapse = ":"), character(1))
  checkboxGroupInput(
    ns("interactions"),
    label = "Add 2-way interactions (optional):",
    choices = stats::setNames(pair_values, pair_labels)
  )
}

# --- Compose RHS terms (fixed + covar + interactions + optional random) ---
reg_compose_rhs <- function(fixed, covar, interactions, random = NULL, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  rhs <- character(0)
  if (!is.null(fixed) && length(fixed) > 0) rhs <- c(rhs, fixed)
  if (!is.null(covar) && length(covar) > 0) rhs <- c(rhs, covar)
  if (!is.null(interactions) && length(interactions) > 0) rhs <- c(rhs, interactions)
  if (engine == "lmm" && !is.null(random) && nzchar(random)) {
    rhs <- c(rhs, paste0("(1|", random, ")"))
  }
  rhs
}

# --- Formula preview UI ---
reg_formula_preview_ui <- function(ns, dep, rhs) {
  if (is.null(dep) || !nzchar(dep)) return(NULL)
  form_txt <- if (length(rhs) == 0) paste(dep, "~ 1") else paste(dep, "~", paste(rhs, collapse = " + "))
  wellPanel(
    strong("Model formula: "),
    code(form_txt)
  )
}

# --- Fit model (LM vs LMM) ---
reg_fit_model <- function(dep, rhs, data, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  form <- as.formula(if (length(rhs) == 0) paste(dep, "~ 1") else paste(dep, "~", paste(rhs, collapse = " + ")))
  if (engine == "lm") {
    lm(form, data = data)
  } else {
    # LMM: lme4 + lmerTest for p-values
    lmerTest::lmer(form, data = data)
  }
}

# --- LM summary output ---
reg_display_lm_summary <- function(m) {
  aout <- capture.output(car::Anova(m, type = 3))
  signif_idx <- grep("^Signif\\. codes", aout)
  if (length(signif_idx) > 0) {
    remove_idx <- c(signif_idx - 1, signif_idx)
    aout <- aout[-remove_idx]
  }
  cat(paste(aout, collapse = "\n"), "\n\n")
  
  sout <- capture.output(summary(m))
  start <- grep("^Residuals:", sout)[1]
  stop  <- grep("^Signif\\. codes", sout)[1]
  if (!is.na(start)) {
    if (!is.na(stop)) sout <- sout[start:(stop - 2)]
    else sout <- sout[start:length(sout)]
  }
  cat(paste(sout, collapse = "\n"))
}

# --- LMM summary output ---
reg_display_lmm_summary <- function(m) {
  aout <- capture.output(anova(m, type = 3))
  cat(paste(aout, collapse = "\n"), "\n\n")
  
  sout <- capture.output(summary(m))
  start <- grep("^Scaled residuals:", sout)[1]
  stop  <- grep("^Correlation of Fixed Effects:", sout)[1]
  if (!is.na(start)) {
    if (!is.na(stop)) sout <- sout[start:(stop - 1)]
    else sout <- sout[start:length(sout)]
  }
  
  icc_df <- compute_icc(m)
  if (!is.null(icc_df) && nrow(icc_df) > 0) {
    icc_line <- paste(paste0("ICC (", icc_df$Group, "): ", icc_df$ICC), collapse = "; ")
    random_idx <- grep("^Random effects:", sout)[1]
    if (!is.na(random_idx)) sout <- append(sout, paste0("\n", icc_line), after = random_idx + 4)
    else sout <- c(sout, icc_line)
  }
  cat(paste(sout, collapse = "\n"))
}

# ===============================================================
# 🧾 Publication-ready DOCX Export for LM and LMM
# ===============================================================

write_lm_docx <- function(model, file) {
  library(flextable)
  library(officer)
  library(dplyr)
  library(car)
  library(lme4)

  # Determine model type
  is_lmm <- inherits(model, "merMod")
  dep_var <- all.vars(formula(model))[1]

  # Helper for consistent table formatting
  format_table <- function(df, bold_p = TRUE) {
    ft <- flextable(df)
    ft <- fontsize(ft, part = "all", size = 10)
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- color(ft, part = "header", color = "black")
    ft <- align(ft, align = "center", part = "all")
    ft <- border_remove(ft)
    black <- fp_border(color = "black", width = 1)
    ft <- border(ft, part = "header", border.top = black)
    ft <- border(ft, part = "header", border.bottom = black)
    if (nrow(df) > 0) {
      ft <- border(ft, i = nrow(df), part = "body", border.bottom = black)
    }

    # Bold significant p-values
    if (bold_p) {
      p_cols <- names(df)[grepl("Pr", names(df), fixed = TRUE)]
      for (pcol in p_cols) {
        if (is.numeric(df[[pcol]]) || all(grepl("^[0-9.<]+$", df[[pcol]]))) {
          sig_rows <- suppressWarnings(which(as.numeric(df[[pcol]]) < 0.05))
          if (length(sig_rows) == 0) {
            # handle formatted p-values like "<0.001"
            sig_rows <- grep("<0\\.0*1", df[[pcol]])
          }
          if (length(sig_rows) > 0 && pcol %in% ft$col_keys) {
            ft <- bold(ft, i = sig_rows, j = pcol, bold = TRUE)
          }
        }
      }
    }

    ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
    ft
  }

  # Create new Word document
  doc <- read_docx()

  # ---- Title ----
  title_text <- sprintf(
    "%s Results — %s",
    if (is_lmm) "Linear Mixed Model" else "Linear Model",
    dep_var
  )
  doc <- body_add_fpar(doc, fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12))))
  doc <- body_add_par(doc, "")

  # ==========================================================
  # 🔹 ANOVA (Type III)
  # ==========================================================
  doc <- body_add_fpar(doc, fpar(ftext("ANOVA (Type III)", prop = fp_text(bold = TRUE))))
  doc <- body_add_par(doc, "")

  if (is_lmm) {
    anova_tbl <- as.data.frame(anova(model, type = 3))
  } else {
    anova_tbl <- as.data.frame(car::Anova(model, type = 3))
  }
  anova_tbl <- tibble::rownames_to_column(anova_tbl, "Effect")

  # Round numeric columns and format p-values
  for (col in names(anova_tbl)) {
    if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 3)
  }
  p_col <- grep("^Pr", names(anova_tbl), value = TRUE)
  if (length(p_col) > 0) {
    colnames(anova_tbl)[colnames(anova_tbl) == p_col[1]] <- "Pr(>F)"
  }

  ft_anova <- format_table(anova_tbl)
  doc <- body_add_flextable(doc, ft_anova)
  doc <- body_add_par(doc, "")

  # ==========================================================
  # 🔹 Random Effects & ICC (LMM only)
  # ==========================================================
  if (is_lmm) {
    # ---- Random Effects ----
    doc <- body_add_fpar(doc, fpar(ftext("Random Effects", prop = fp_text(bold = TRUE))))
    doc <- body_add_par(doc, "")

    rand_df <- as.data.frame(lme4::VarCorr(model))
    if (nrow(rand_df) > 0) {
      rand_df <- rand_df[, c("grp", "var1", "var2", "vcov", "sdcor"), drop = FALSE]
      rand_df$var2 <- ifelse(is.na(rand_df$var2), "-", rand_df$var2)
      names(rand_df) <- c("Grouping", "Effect 1", "Effect 2", "Variance", "Std. Dev.")
      rand_df$Variance <- round(rand_df$Variance, 3)
      rand_df$`Std. Dev.` <- round(rand_df$`Std. Dev.`, 3)
      ft_rand <- format_table(rand_df, bold_p = FALSE)
      doc <- body_add_flextable(doc, ft_rand)
    } else {
      doc <- body_add_par(doc, "No random-effect variance components were estimated.", style = "Normal")
    }

    # ---- ICC ----
    if (exists("compute_icc") && is.function(compute_icc)) {
      icc_df <- compute_icc(model)
    } else {
      icc_df <- NULL
    }
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      doc <- body_add_par(doc, "")
      doc <- body_add_fpar(doc, fpar(ftext("Intraclass Correlation (ICC)", prop = fp_text(bold = TRUE))))
      doc <- body_add_par(doc, "")
      icc_df$ICC <- round(icc_df$ICC, 3)
      ft_icc <- format_table(icc_df, bold_p = FALSE)
      doc <- body_add_flextable(doc, ft_icc)
    }

    doc <- body_add_par(doc, "")
  }

  # ==========================================================
  # 🔹 Model Coefficients
  # ==========================================================
  doc <- body_add_fpar(doc, fpar(ftext("Model Coefficients", prop = fp_text(bold = TRUE))))
  doc <- body_add_par(doc, "")

  coef_tbl <- as.data.frame(summary(model)$coefficients)
  coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
  names(coef_tbl)[1] <- "Term"
  names(coef_tbl) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|t|)", names(coef_tbl))

  for (col in names(coef_tbl)) {
    if (is.numeric(coef_tbl[[col]])) coef_tbl[[col]] <- round(coef_tbl[[col]], 4)
  }

  ft_coef <- format_table(coef_tbl)
  doc <- body_add_flextable(doc, ft_coef)

  # ==========================================================
  # 🔹 Footer
  # ==========================================================
  doc <- body_add_par(doc, "")
  doc <- body_add_par(doc, "Significance level: p < 0.05 (bold values).", style = "Normal")
  doc <- body_add_par(doc, sprintf("Generated by Animal Trial Analyzer on %s", Sys.Date()))

  # Save file
  print(doc, target = file)
}

# ===============================================================
# 🧾 Helper: format regression table in journal style
# ===============================================================
format_regression_table <- function(df, bold_p = TRUE) {
  
  ft <- flextable(df)
  ft <- fontsize(ft, part = "all", size = 10)
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- color(ft, part = "header", color = "black")
  ft <- align(ft, align = "center", part = "all")
  ft <- border_remove(ft)
  
  black <- fp_border(color = "black", width = 1)
  ft <- border(ft, part = "header", border.top = black)
  ft <- border(ft, part = "header", border.bottom = black)
  if (nrow(df) > 0) {
    ft <- border(ft, i = nrow(df), part = "body", border.bottom = black)
  }

  if (bold_p && "Pr(>F)" %in% names(df)) {
    sig_rows <- which(df[["Pr(>F)"]] < 0.05)
    if (length(sig_rows) > 0) ft <- bold(ft, i = sig_rows, j = "Pr(>F)", bold = TRUE)
  }
  if (bold_p && "Pr(>|t|)" %in% names(df)) {
    sig_rows <- which(df[["Pr(>|t|)"]] < 0.05)
    if (length(sig_rows) > 0) ft <- bold(ft, i = sig_rows, j = "Pr(>|t|)", bold = TRUE)
  }

  ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
  ft
}