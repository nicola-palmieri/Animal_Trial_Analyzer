library(testthat)
library(dplyr)
source(testthat::test_path("..", "R", "anova_shared.R"))


test_that("prepare_stratified_anova sets factor levels and handles single strata", {
  df <- data.frame(
    response = c(1, 2, 3, 4),
    group = c("High", "High", "Low", "Low"),
    strata = rep("S1", 4),
    stringsAsFactors = FALSE
  )

  prep <- prepare_stratified_anova(
    df,
    responses = "response",
    model = "oneway_anova",
    factor1_var = "group",
    factor1_order = c("Low", "High"),
    stratification = list(var = "strata", levels = "S1")
  )

  expect_equal(levels(prep$data_used$group), c("Low", "High"))
  expect_equal(prep$strata$levels, "S1")
  expect_true(all(names(prep$models) %in% c("S1")))
})


test_that("prepare_anova_outputs returns rounded tables", {
  df <- data.frame(
    response = c(1, 2, 1.5, 2.5),
    group = factor(c("A", "A", "B", "B"))
  )

  model <- aov(response ~ group, data = df)
  outputs <- prepare_anova_outputs(model, factor_names = "group")

  expect_s3_class(outputs$anova_table, "data.frame")
  expect_true("p.value" %in% names(outputs$anova_table))
  expect_true(is.numeric(outputs$anova_table$p.value))
  expect_true(all(!is.na(outputs$anova_significant)))
})
