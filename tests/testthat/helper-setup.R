# ---------------------------------------------------------------------------
# helper-setup.R
# Sources the pipeline functions under test and builds synthetic registry data.
# No real data or config.R is required: every fixture here is fabricated, so
# the suite runs on any machine and in CI with nothing sensitive present.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(readr)
})

# testthat runs with the working directory set to tests/testthat, so the
# pipeline lives two levels up in R/.
.r_dir <- file.path("..", "..", "R")
source(file.path(.r_dir, "utils.R"))
source(file.path(.r_dir, "02_recode.R"))
source(file.path(.r_dir, "03_cohort.R"))
source(file.path(.r_dir, "04_match.R"))

# Build one synthetic procedure row with sensible defaults; override any field
# by name. Column names match the real registry export exactly.
make_proc <- function(mrn, admit, proc = admit, inmate = "0", sex = "1",
                      hybrid = FALSE, sev_r = "10", sev_l = "0",
                      age = 65, ...) {
  row <- tibble::tibble(
    `MRN`                        = as.character(mrn),
    `Admit Date`                 = as.Date(admit),
    `Procedure Date`             = as.Date(proc),
    `Discharge Date`             = as.Date(proc) + 2,
    `Date of Death`              = as.Date(NA),
    `SSDI Date of Death`         = as.Date(NA),
    `Inmate`                     = inmate,
    `Birth Sex`                  = sex,
    `Age at Procedure (years)`   = age,
    `BMI`                        = 27,
    `Height Cm`                  = 175,
    `Weight Kg`                  = 82,
    `Race`                       = "5",
    `Urgency`                    = "1",
    `Ambulation`                 = "1",
    `CAD`                        = "0",
    `Prior CABG`                 = "0",
    `Prior PCI`                  = "0",
    `CHF`                        = "0",
    `COPD`                       = "0",
    `Diabetes`                   = "0",
    `Dialysis`                   = "0",
    `Hypertension`               = "0",
    `Smoking`                    = "1",
    `Pre-op Statin`              = "1",
    `Pre-op ACE-Inhibitor/ARB`   = "0",
    `Pre-op ASA`                 = "1",
    `Pre-op Antiplatelet Drugs`  = "0",
    `Pre Chronic Anticoagulant`  = "0",
    `Lg Art Byp,Endarterectomy,PVI` = "0",
    `Prior Amp (Leg, Foot, Toe)` = "0",
    `Leg Symptoms Right`         = sev_r,
    `Leg Symptoms Left`          = sev_l,
    `CON1 -Concomitant infra bypass` = if (hybrid) "1" else "0"
  )
  ov <- list(...)
  for (nm in names(ov)) row[[nm]] <- ov[[nm]]
  row
}

# Assemble a registry-like data frame from a list of make_proc() rows.
make_raw <- function(...) dplyr::bind_rows(...)
