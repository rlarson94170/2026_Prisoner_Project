# Tests for the matching step in 04_match.R. These need MatchIt, optmatch,
# cobalt, and (for a weighted Table 1) survey; the suite skips cleanly if any
# are missing.

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
    ) %>%
      dplyr::mutate(
        clti = factor(
          dplyr::if_else(limb_severity %in% c("Rest pain", "Tissue loss"),
                         "CLTI", "Claudication"),
          levels = c("Claudication", "CLTI"))
      )
  }
  dplyr::bind_rows(
    gen(n_inmate, shift = 2) %>% dplyr::mutate(inmate = "Inmate"),
    gen(n_ctrl,   shift = 0) %>% dplyr::mutate(inmate = "Non-inmate")
  ) %>%
    dplyr::mutate(
      inmate     = factor(inmate, levels = c("Non-inmate", "Inmate")),
      race       = factor(race),
      ambulation = factor(ambulation),
      urgency    = factor(urgency),
      study_id   = sprintf("PID%04d", dplyr::row_number())
    )
}

test_that("full matching retains all inmates and reuses no control", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("optmatch")
  skip_if_not_installed("cobalt")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  res <- run_matching(analytic, out_dir = tmp_out)
  m <- res$matched

  # Full matching keeps every treated unit.
  expect_equal(sum(m$inmate == "Inmate"), sum(analytic$inmate == "Inmate"))
  # Each control sits in exactly one subclass, so IDs stay unique.
  ctl_ids <- m$study_id[m$inmate == "Non-inmate"]
  expect_equal(anyDuplicated(ctl_ids), 0L)
})

test_that("matching improves balance on the matching covariates", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("optmatch")
  skip_if_not_installed("cobalt")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  res <- run_matching(analytic, out_dir = tmp_out)
  b <- res$balance_matched$Balance
  expect_lt(max(abs(b$Diff.Adj), na.rm = TRUE),
            max(abs(b$Diff.Un),  na.rm = TRUE))
  expect_type(res$max_smd_matched, "double")
})

test_that("matching writes all expected output files", {
  skip_if_not_installed("MatchIt")
  skip_if_not_installed("optmatch")
  skip_if_not_installed("cobalt")
  skip_if_not_installed("tableone")

  analytic <- make_analytic()
  tmp_out <- file.path(tempdir(), paste0("m_", as.integer(runif(1, 1, 1e8))))
  run_matching(analytic, out_dir = tmp_out)
  for (f in c("balance_table.txt", "love_plot.png",
              "table1_matched.csv", "matched_cohort.rds")) {
    expect_true(file.exists(file.path(tmp_out, f)), info = f)
  }
})
