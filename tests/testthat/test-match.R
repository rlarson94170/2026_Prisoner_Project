# Tests for the matching step in 04_match.R. These need MatchIt, cobalt, and
# tableone; the suite skips cleanly if they are not installed.

make_analytic <- function(n_inmate = 15, n_ctrl = 150, seed = 1) {
  set.seed(seed)
  gen <- function(n, shift = 0) {
    tibble::tibble(
      age              = round(rnorm(n, 64 + shift, 9)),
      current_smoker   = rbinom(n, 1, 0.45 + 0.1 * (shift != 0)),
      diabetes_any     = rbinom(n, 1, 0.5),
      diabetes_insulin = rbinom(n, 1, 0.2),
      dialysis         = rbinom(n, 1, 0.1),
      coronary_disease = rbinom(n, 1, 0.4),
      chf_symptomatic  = rbinom(n, 1, 0.2),
      copd_treated     = rbinom(n, 1, 0.25),
      hypertension     = rbinom(n, 1, 0.8),
      statin           = rbinom(n, 1, 0.75),
      ace_arb          = rbinom(n, 1, 0.5),
      antiplatelet     = rbinom(n, 1, 0.7),
      anticoagulant    = rbinom(n, 1, 0.15),
      prior_ipsi_revasc= rbinom(n, 1, 0.3),
      prior_amputation = rbinom(n, 1, 0.15),
      bmi              = round(rnorm(n, 27, 4), 1),
      race          = sample(c("White", "Black", "Other/Unknown"), n, TRUE),
      ambulation    = sample(c("Ambulatory", "Assisted", "Non-ambulatory"), n, TRUE),
      urgency       = sample(c("Elective", "Urgent/Emergent"), n, TRUE),
      limb_severity = factor(sample(c("Claudication", "Rest pain", "Tissue loss"),
                                    n, TRUE),
                             levels = c("Asymptomatic", "Claudication",
                                        "Rest pain", "Tissue loss"))
    )
  }
  dplyr::bind_rows(
    gen(n_inmate, shift = 2) %>% dplyr::mutate(inmate = "Inmate"),
    gen(n_ctrl,   shift = 0) %>% dplyr::mutate(inmate = "Non-inmate")
  ) %>%
    dplyr::mutate(
      inmate   = factor(inmate, levels = c("Non-inmate", "Inmate")),
      race     = factor(race),
      ambulation = factor(ambulation),
      urgency  = factor(urgency),
      study_id = sprintf("PID%04d", dplyr::row_number())
    )
}

test_that("matching respects the ratio and matches without replacement", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("cobalt")
  skip_if_not_installed("tableone")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  res <- run_matching(analytic, ratio = 3, caliper = 0.5, out_dir = tmp_out)
  m <- res$matched

  n_inm <- sum(m$inmate == "Inmate")
  n_ctl <- sum(m$inmate == "Non-inmate")
  expect_gt(n_inm, 0)
  expect_lte(n_ctl, 3 * n_inm)                 # at most 3 controls per inmate
  ctl_ids <- m$study_id[m$inmate == "Non-inmate"]
  expect_equal(anyDuplicated(ctl_ids), 0L)     # no control used twice
})

test_that("matching improves overall balance", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("cobalt")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  res <- run_matching(analytic, ratio = 3, caliper = 0.5, out_dir = tmp_out)
  bal <- res$balance$Balance
  expect_lt(max(abs(bal$Diff.Adj), na.rm = TRUE),
            max(abs(bal$Diff.Un),  na.rm = TRUE))
})

test_that("matching writes all expected output files", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("cobalt")
  skip_if_not_installed("tableone")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  run_matching(analytic, ratio = 3, caliper = 0.5, out_dir = tmp_out)
  for (f in c("balance_table.txt", "love_plot.png",
              "table1_matched.csv", "matched_cohort.rds")) {
    expect_true(file.exists(file.path(tmp_out, f)), info = f)
  }
})
