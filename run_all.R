# ---------------------------------------------------------------------------
# run_all.R
# Runs the whole pipeline from the raw registry export to the matched cohort.
# From the repository root:  source("run_all.R")
#
# Nothing written here contains PHI: the analytic dataset is de-identified and
# all outputs land in the git-ignored output/ folder. The study-ID crosswalk is
# the only file linking to MRNs and it stays in the git-ignored private/ folder.
# ---------------------------------------------------------------------------

source(here::here("R", "00_setup.R"))
source(here::here("R", "01_import.R"))
source(here::here("R", "02_recode.R"))
source(here::here("R", "03_cohort.R"))
source(here::here("R", "04_match.R"))

raw      <- import_raw()
proc     <- recode_registry(raw)
cohort   <- build_cohort(proc)
result   <- run_matching(cohort$analytic, drop_vars = cohort$degenerate_vars)

message("\nPipeline complete.")
message("Cohort flow:")
print(cohort$flow)
message("\nOutputs written to: ", here::here("output"))
message(" - cohort_flow.csv")
message(" - balance_table.txt")
message(" - love_plot.png")
message(" - table1_matched.csv")
message(" - matched_cohort.rds  (de-identified)")
