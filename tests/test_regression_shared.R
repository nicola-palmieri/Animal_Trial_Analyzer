library(testthat)
source("R/regression_analysis_shared.R")

test_that("reg_detect_types identifies numeric and categorical variables across ranges", {
  df <- data.frame(
    response = c(-1e308, 0, 1e308),
    group = factor(c("a", "b", "a")),
    label = c("x", "y", "z"),
    stringsAsFactors = FALSE
  )

  types <- reg_detect_types(df)

  expect_setequal(types$num, "response")
  expect_setequal(types$fac, c("group", "label"))
})


test_that("reg_compose_rhs builds correct formula parts for lm and lmm", {
  fixed <- c("group", "treatment")
  covar <- "age"
  interactions <- "group:treatment"

  expect_equal(
    reg_compose_rhs(fixed, covar, interactions, random = NULL, engine = "lm"),
    c(fixed, covar, interactions)
  )

  expect_equal(
    reg_compose_rhs(fixed, covar, interactions, random = "site", engine = "lmm"),
    c(fixed, covar, interactions, "(1|site)")
  )
})


test_that("tidy_regression_model returns metrics and coefficients for lm", {
  df <- data.frame(
    y = c(1, 2, 3, 4),
    x = c(0, 1, 0, 1)
  )

  model <- lm(y ~ x, data = df)
  tidy <- tidy_regression_model(model, engine = "lm")

  expect_true("summary" %in% names(tidy))
  expect_true("effects" %in% names(tidy))
  expect_s3_class(tidy$summary, "data.frame")
  expect_true(all(c("metric", "value") %in% names(tidy$effects$metrics)))
  expect_true("Effect" %in% names(tidy$effects$anova))
})
