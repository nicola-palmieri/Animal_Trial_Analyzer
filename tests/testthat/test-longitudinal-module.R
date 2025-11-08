library(testthat)

sample_longitudinal <- tibble::tibble(
  subject_id = rep(paste0("S", 1:10), each = 4),
  visit_day = rep(c(0, 7, 14, 28), times = 10),
  treatment = rep(rep(c("A", "B"), each = 2), times = 10),
  outcome = rnorm(40)
)

non_time_data <- tibble::tibble(
  subject = rep(1:5, each = 2),
  group = rep(c("X", "Y"), times = 5),
  value = rnorm(10)
)

test_that("detection prioritizes visit_day as time", {
  det <- longitudinal_detect_variables(sample_longitudinal)
  expect_equal(det$default_time, "visit_day")
  expect_equal(det$default_subject, "subject_id")
  expect_equal(det$default_treatment, "treatment")
  expect_true("outcome" %in% det$response_choices)
})

test_that("detection falls back to numeric when no time-like variable", {
  det <- longitudinal_detect_variables(non_time_data)
  expect_true(det$default_time %in% names(non_time_data))
  expect_true(det$default_subject %in% names(non_time_data))
})

test_that("rhs builder adds random slope when requested", {
  rhs <- longitudinal_build_rhs(
    time_var = "visit_day",
    treatment_var = "treatment",
    include_interaction = TRUE,
    method = "lmm",
    subject_var = "subject_id",
    random_slope = TRUE
  )
  expect_true("visit_day" %in% rhs)
  expect_true("treatment" %in% rhs)
  expect_true("visit_day:treatment" %in% rhs)
  expect_true("(1 + visit_day|subject_id)" %in% rhs)
})

test_that("rhs builder omits random term for gee", {
  rhs <- longitudinal_build_rhs(
    time_var = "visit_day",
    treatment_var = "treatment",
    include_interaction = FALSE,
    method = "gee",
    subject_var = "subject_id",
    random_slope = TRUE
  )
  expect_false(any(grepl("\\|", rhs, fixed = TRUE)))
})

test_that("gee fitting requires subject identifier", {
  expect_error(
    longitudinal_fit_model(
      df = sample_longitudinal,
      response = "outcome",
      rhs_terms = c("visit_day", "treatment"),
      method = "gee",
      subject_var = NULL
    ),
    "Subject identifier is required"
  )
})
