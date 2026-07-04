# ---------------------------------------------------------------------------
# dev/make_synthetic_outcomes.R
# Generates a plausible, entirely fake outcomes file so 05_outcomes.R can be
# exercised before the real chart abstraction exists. The numbers mean nothing;
# they only have the right shape and column names (the de-identified outcomes
# schema documented in R/05_outcomes.R).
#
# As a script it reads output/matched_cohort.rds and writes
# output/synthetic_outcomes.csv. The function is also sourced by the tests.
# ---------------------------------------------------------------------------

make_synthetic_outcomes <- function(matched, seed = 42) {
  set.seed(seed)
  n   <- nrow(matched)
  inm <- as.integer(matched$inmate == "Inmate")

  # Survival: modest, similar hazards by group (the "similarity" hypothesis).
  os_time_raw <- round(rexp(n, rate = 1 / 900) + 30)
  os_event    <- rbinom(n, 1, 0.18)
  os_time     <- pmin(os_time_raw, 365 * 3)

  # MALE with competing death.
  male_status <- sample(0:2, n, replace = TRUE, prob = c(0.72, 0.18, 0.10))
  male_time   <- round(rexp(n, rate = 1 / 700) + 20)
  male_time   <- pmin(male_time, os_time)

  tibble::tibble(
    study_id          = matched$study_id,
    os_time           = os_time,
    os_event          = os_event,
    male_time         = male_time,
    male_status       = male_status,
    readmit_1yr       = rbinom(n, 1, 0.25),
    current_smoker_fu = rbinom(n, 1, ifelse(inm == 1, 0.15, 0.30)),  # lower in inmates
    statin_adherent   = rbinom(n, 1, ifelse(inm == 1, 0.92, 0.75)),  # higher in inmates
    days_to_first_fu  = round(rgamma(n, shape = 2,
                              scale = ifelse(inm == 1, 35, 20))),     # longer in inmates
    n_fu_visits_1yr   = rpois(n, lambda = ifelse(inm == 1, 2.2, 3.1)) # fewer in inmates
  )
}

# Run as a script.
if (sys.nframe() == 0) {
  suppressPackageStartupMessages({ library(tibble); library(readr) })
  mc_path <- here::here("output", "matched_cohort.rds")
  if (!file.exists(mc_path)) {
    stop("output/matched_cohort.rds not found. Run run_all.R first.", call. = FALSE)
  }
  matched <- readRDS(mc_path)
  out <- make_synthetic_outcomes(matched)
  readr::write_csv(out, here::here("output", "synthetic_outcomes.csv"))
  message("Wrote output/synthetic_outcomes.csv (", nrow(out), " rows).")
}
