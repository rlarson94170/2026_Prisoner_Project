# ---------------------------------------------------------------------------
# run_artifacts.R
# Builds everything downstream of the match: the v1 linkage, the prefilled
# abstraction workbook, the REDCap import file, and the CONSORT diagram.
#
# From the repository root:  source("run_artifacts.R")
# In RStudio: open this file and press the Source button (Cmd/Ctrl+Shift+S).
#
# Run this AFTER run_all.R, and after validate.R has passed. The dev/ scripts
# only define functions when sourced; this driver calls them in the order they
# depend on each other. The v1 linkage has to exist before the workbook is
# built, because the workbook carries the 2020 data over.
#
# Outputs (all git-ignored):
#   private/v1_linkage.rds, private/v1_linkage.csv
#   private/abstraction_workbook_prefilled.xlsx   <- contains MRNs
#   private/redcap_import.csv                     <- contains MRNs
#   output/consort_diagram.svg
# ---------------------------------------------------------------------------

source(here::here("R", "00_setup.R"))

matched_path <- here::here("output", "matched_cohort.rds")
if (!file.exists(matched_path)) {
  stop("output/matched_cohort.rds not found. Run run_all.R first.", call. = FALSE)
}

source(here::here("dev", "link_v1_abstraction.R"))
source(here::here("dev", "make_abstraction_workbook.R"))
source(here::here("dev", "make_redcap_import.R"))
source(here::here("dev", "make_consort_diagram.R"))

## ---- 1. Link the 2020 abstraction to the current matched set --------------
message("\n=== 1/4  Linking the 2020 abstraction ===")
res <- link_v1_abstraction()

message("\nReuse against the current matched cohort:")
print(as.data.frame(res$summary))

n_reusable <- sum(res$link$v1_status == "reusable_same_index")
n_complete <- sum(res$link$v1_1yr_complete)
message(sprintf(
  "\n%d of %d matched patients already have 2020 data at the same index procedure; %d of those have a full one-year window.",
  n_reusable, nrow(res$link), n_complete))

## ---- 2. Abstraction workbook, with the v1 carry-over ----------------------
message("\n=== 2/4  Abstraction workbook ===")
# openxlsx emits ~23 harmless 'one argument not used by format' warnings from
# its dropdown writer. Muffle just those so real problems stay visible.
withCallingHandlers(
  generate_abstraction_workbook(v1_link = res$link),
  warning = function(w) {
    if (grepl("argument not used by format", conditionMessage(w))) {
      invokeRestart("muffleWarning")
    }
  }
)

## ---- 3. REDCap import file ------------------------------------------------
message("\n=== 3/4  REDCap import ===")
generate_redcap_import(v1_link = res$link)

## ---- 4. CONSORT diagram ---------------------------------------------------
message("\n=== 4/4  CONSORT diagram ===")
generate_consort_diagram()

message("\nAll artifacts rebuilt.")
message("Reminder: update the REDCap project before importing. The presentation ",
        "field gained a third option (0, Asymptomatic), so a project built from ",
        "the old dictionary will reject those records. New project -> import ",
        "redcap/REDCap_Outcome_Abstraction_Project.xml; existing project -> ",
        "Designer -> Data Dictionary -> upload ",
        "redcap/REDCap_Outcome_Abstraction_DataDictionary.csv.")
