# Tests for the outcome analysis in 05_outcomes.R. These need survival,
# sandwich, and lmtest; the suite skips cleanly if any are missing.

source(file.path("..", "..", "R", "05_outcomes.R"))
source(file.path("..", "..", "dev", "make_synthetic_outcomes.R"))

# A small synthetic matched cohort with full-matching weights and subclasses.
make_matched <- function(n_inmate = 20, n_ctrl = 60, seed = 7) {
  set.seed(seed)
  n <- n_inmate + n_ctrl
  tibble::tibble(
    study_id = sprintf("PID%04d", seq_len(n)),
    inmate   = factor(c(rep("Inmate", n_inmate), rep("Non-inmate", n_ctrl)),
                      levels = c("Non-inmate", "Inmate")),
    weights  = runif(n, 0.3, 3),
    subclass = factor(sample(seq_len(n_inmate), n, replace = TRUE))
  )
}

test_that("outcomes derive correctly from abstraction dates", {
  abstraction <- tibble::tibble(
    study_id        = c("PID0001", "PID0002", "PID0003"),
    index_proc_date = as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")),
    index_dc_date   = as.Date(c("2020-01-03", "2020-01-03", "2020-01-03")),
    death_date      = as.Date(c(NA, "2020-07-01", NA)),
    last_alive_date = as.Date(c("2021-01-01", NA, "2021-01-01")),
    major_amp_date  = as.Date(c("2020-04-01", NA, NA)),
    major_reint_date= as.Date(c(NA, NA, NA)),
    readmit_1yr     = c(1, 0, 0),
    smoke_fu        = c("current", "never", "former"),
    statin_active_fu= c("yes", "no", "yes"),
    first_fu_date   = as.Date(c("2020-02-01", "2020-02-15", "2020-01-20")),
    n_fu_visits_1yr = c(3, 1, 4)
  )
  crosswalk <- abstraction["study_id"]  # dates already in abstraction here

  o <- build_outcomes_from_abstraction(abstraction, crosswalk)
  # Patient 1: amputation before any death -> MALE (status 1) at ~91 days.
  expect_equal(o$male_status[1], 1L)
  expect_equal(o$os_event, c(0L, 1L, 0L))
  # Patient 2: died with no MALE -> competing death (status 2).
  expect_equal(o$male_status[2], 2L)
  # days_to_first_fu measured from discharge.
  expect_equal(o$days_to_first_fu[1], as.numeric(as.Date("2020-02-01") - as.Date("2020-01-03")))
})

test_that("run_outcomes returns a tidy effect table and writes files", {
  skip_if_not_installed("survival")
  skip_if_not_installed("sandwich")
  skip_if_not_installed("lmtest")

  matched  <- make_matched()
  outcomes <- make_synthetic_outcomes(matched)
  tmp_out  <- file.path(tempdir(), paste0("o_", as.integer(runif(1, 1, 1e8))))

  res <- run_outcomes(matched, outcomes, out_dir = tmp_out)

  expect_true(all(c("outcome", "effect", "estimate", "conf.low", "conf.high")
                  %in% names(res$results)))
  expect_gte(nrow(res$results), 6)
  expect_true(all(is.finite(res$results$estimate)))
  # CIs ordered.
  expect_true(all(res$results$conf.low <= res$results$conf.high))
  for (f in c("outcome_results.csv", "survival_1yr.csv",
              "male_cif_1yr.csv", "km_survival.png")) {
    expect_true(file.exists(file.path(tmp_out, f)), info = f)
  }
})

test_that("the hazard ratio is positive and finite", {
  skip_if_not_installed("survival")

  matched  <- make_matched()
  outcomes <- make_synthetic_outcomes(matched)
  tmp_out  <- file.path(tempdir(), paste0("o_", as.integer(runif(1, 1, 1e8))))
  res <- run_outcomes(matched, outcomes, out_dir = tmp_out)
  hr <- res$results$estimate[res$results$effect == "Hazard ratio"]
  expect_true(is.finite(hr) && hr > 0)
})
