# ---------------------------------------------------------------------------
# run_outcomes.R
# Runs the outcome analysis on the matched cohort. Until the real abstraction
# exists, it uses a synthetic outcomes file so the whole path is exercisable.
#
# From the repository root:  source("run_outcomes.R")
#
# When the real, de-identified outcomes file is ready, point `outcomes_path` at
# it (a CSV with the schema documented in R/05_outcomes.R) and rerun.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(readr); library(ggplot2)
})
source(here::here("R", "05_outcomes.R"))

matched_path  <- here::here("output", "matched_cohort.rds")
outcomes_path <- here::here("output", "synthetic_outcomes.csv")  # <- swap for real file

if (!file.exists(matched_path)) {
  stop("Run run_all.R first to create output/matched_cohort.rds.", call. = FALSE)
}
matched <- readRDS(matched_path)

# Generate the synthetic outcomes file on demand if it isn't there yet.
if (!file.exists(outcomes_path)) {
  source(here::here("dev", "make_synthetic_outcomes.R"))
  readr::write_csv(make_synthetic_outcomes(matched), outcomes_path)
  message("Created a synthetic outcomes file for demonstration: ", outcomes_path)
}

outcomes <- readr::read_csv(outcomes_path, show_col_types = FALSE)

# Doubly-robust adjustment: covariate-adjust the outcome models for the
# covariates that validate.R flags as residually imbalanced (|SMD| 0.10-0.25).
# Keep this list short to avoid overfitting the small number of events.
#
# PRE-SPECIFIED 2026-08-06, from the validation run on the final 658-patient
# supported cohort (39 inmates, 78 matched controls, max |SMD| 0.160). Fixed
# before any outcome data existed, so this is a pre-specified choice rather than
# one made after seeing results. Do not revise it once abstraction has begun
# without recording why.
#
# The earlier default was prior_ipsi_revasc, which balanced out once the
# propensity model was corrected for common support and is no longer an
# offender.
adjust_vars <- c("clti", "chf_symptomatic", "race")

# Sanity check: the adjustment set should still be the covariates validate.R
# flags. If a re-run of run_all.R changes the matched set, re-read
# output/validation_report.txt and update the list above deliberately.
missing_adj <- setdiff(adjust_vars, names(matched))
if (length(missing_adj)) {
  warning("adjust_vars not present in the matched cohort: ",
          paste(missing_adj, collapse = ", "),
          ". Check that the matching spec has not changed.", call. = FALSE)
}

res <- run_outcomes(matched, outcomes, adjust_vars = adjust_vars)

message("\nOutcome results:")
print(res$results)
message("\nOutputs written to ", here::here("output"), ":")
message(" - outcome_results.csv, survival_1yr.csv, male_cif_1yr.csv, km_survival.png")
