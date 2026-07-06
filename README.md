# Prisoner PAD analysis

<!-- Replace OWNER/REPO with your GitHub path once pushed, e.g. rlarson/prisoner-pad-analysis -->
[![R tests](https://github.com/OWNER/REPO/actions/workflows/R-tests.yaml/badge.svg)](https://github.com/OWNER/REPO/actions/workflows/R-tests.yaml)

Reproducible R pipeline for the study of endovascular outcomes in incarcerated
versus non-incarcerated patients with peripheral artery disease. The code takes
the institutional VQI PVI registry export, applies the pre-specified cohort
rules, and builds a propensity-matched comparison group for downstream outcome
analysis.

See `SAP_Prisoner_PAD_v0.1.docx` (in the project Output folder) for the
statistical analysis plan this code implements.

## HIPAA and PHI

This repository is designed to be shared on GitHub. **No PHI is stored in the
repository, and none is hard-coded in any script.**

- The raw registry file is never committed. Its location is read from a local
  `config.R` that is git-ignored. Copy `config.example.R` to `config.R` and set
  the path on your own machine.
- The `data/`, `output/`, and `private/` folders are git-ignored. Nothing that
  could contain patient identifiers or dates leaves your machine.
- The analytic dataset the pipeline builds is de-identified: each admission gets
  a study ID, and the study-ID-to-MRN crosswalk is written only to the
  git-ignored `private/` folder.
- Cohort rules (including the handling of patients treated in both states) are
  applied by logic, never by listing medical record numbers.

Before pushing, run `git status` and confirm no data files, no `config.R`, and
no `private/` contents are staged.

## Cohort rules (implemented in `R/03_cohort.R`)

- Purely endovascular only. Procedures with a concomitant open bypass (hybrid)
  are excluded.
- Male patients only. Every inmate in the registry is male, so the comparison is
  restricted to men and sex drops out as a matching variable.
- Unit of analysis is the hospital admission. Within an admission, only the first
  procedure is the index event.
- One index admission per patient. For inmates it is the first admission during
  incarceration; for non-inmates the first qualifying admission.
- Patients treated both as inmates and non-inmates are classified as inmates,
  and their non-inmate admissions are removed from the control pool.

## Matching (`R/04_match.R`)

Optimal 2:1 matching on the propensity score. Each inmate is matched to two
distinct controls chosen to minimize the total propensity-score distance, which
bounds the control set (up to 76 controls for 38 inmates, ~114 charts) so the
chart abstraction is feasible. The ratio is a parameter of `run_matching()`.

With 38 inmates and 13 correlated covariates, a bounded match cannot force every
covariate below 0.10 the way full matching can. This is a doubly-robust matched
design: we require adequate balance (standardized mean difference < 0.25, with
< 0.10 as ideal) and then covariate-adjust any residual imbalance in the outcome
models. `validate.R` reports which covariates sit between 0.10 and 0.25, and you
pass those to `run_outcomes(adjust_vars = ...)`.

The matching covariates are age, current smoking, diabetes, dialysis, CLTI
(limb severity collapsed to chronic limb-threatening ischemia versus not),
coronary disease, symptomatic CHF, urgency, treated COPD, prior ipsilateral
revascularization, ambulation, race, and BMI.

Baseline medications (statin, ACE/ARB, antiplatelet, insulin) are deliberately
**not** matched on. Higher preoperative use of these agents in inmates is part
of the incarceration effect under study (supervised administration), so they are
reported as baseline differences rather than balanced away. They appear in the
balance table and Table 1 for description only.

## How to run

1. Install R (>= 4.2) and the packages listed in `R/00_setup.R`.
2. Copy `config.example.R` to `config.R` and set `RAW_DATA_PATH` to your local
   copy of `2016_2024_PVI_DATA.xlsx`.
3. From the repository root:

   ```r
   source("run_all.R")
   ```

Outputs (Table 1, Love plot, matched cohort) are written to the git-ignored
`output/` folder.

## Outcome analysis (`R/05_outcomes.R`)

Placeholder until the chart abstraction is done. It takes the matched cohort and
a de-identified outcomes file (one row per study ID, day-count intervals and
event flags, no calendar dates) and produces the SAP comparisons: overall
survival (Kaplan-Meier plus weighted Cox hazard ratio), MALE with death as a
competing risk (cumulative incidence plus a Fine-Gray subdistribution hazard
ratio), readmission, current smoking, and statin adherence as risk differences,
and follow-up access as a mean difference in days and a visit rate ratio. Every
estimate carries a 95% CI from cluster-robust standard errors on the matched
sets.

Run it against a generated synthetic outcomes file so the path is exercisable
now:

```r
source("run_all.R")       # builds output/matched_cohort.rds
source("run_outcomes.R")  # makes synthetic outcomes if none exist, then analyzes
```

`build_outcomes_from_abstraction()` in `R/05_outcomes.R` converts the completed
abstraction workbook plus the private crosswalk into that de-identified outcomes
file. When the real data is ready, point `outcomes_path` in `run_outcomes.R` at
it and rerun.

## Prefilled abstraction workbook

`dev/make_abstraction_workbook.R` builds an Excel workbook preloaded with the
matched patients so abstractors only fill in the chart-based fields. The known
reference columns (study ID, MRN, index dates, presentation, any registry death
date) are prefilled and shaded; the chart-based columns are blank with dropdowns.

```r
source("run_all.R")                       # builds the matched cohort + crosswalk
source("dev/make_abstraction_workbook.R") # loads the functions
generate_abstraction_workbook()           # writes the prefilled workbook
```

It writes `private/abstraction_workbook_prefilled.xlsx`. That file holds MRNs and
dates, so it stays in the git-ignored `private/` folder and must never be
committed. Requires the `openxlsx` package.

## REDCap abstraction (alternative to the Excel workbook)

For institutions that use REDCap, the abstraction can be entered on a single
validated web form instead of a spreadsheet. Files live in `redcap/`.

- `REDCap_Outcome_Abstraction_Project.xml` — upload at project creation ("Upload
  a REDCap project XML file"). Builds the whole instrument: 48 fields in 8
  sections, date and integer validation, dropdowns and yes/no fields, 16
  branching-logic rules (e.g. a death date only appears when vital status is
  dead), required fields, the MRN flagged as an identifier, and the prefilled
  reference fields marked `@READONLY`.
- `REDCap_Outcome_Abstraction_DataDictionary.csv` — the same instrument as a
  REDCap Data Dictionary. If the XML ever hiccups on your REDCap version, create
  an empty project and upload this under Designer -> Data Dictionary instead.

Neither file contains PHI. Test-import into a scratch project first to confirm
it renders the way you want.

To preload the matched patients so abstractors only fill chart-based fields:

```r
source("dev/make_redcap_import.R")
generate_redcap_import()   # writes private/redcap_import.csv (MRNs -> keep off GitHub)
```

Then in REDCap use the Data Import Tool to upload `private/redcap_import.csv`.

## Testing and validation

Two independent layers.

**Unit tests** run on synthetic, fabricated data, so they need no real export
and no `config.R`. They cover the code decoding, every cohort rule, the matching
mechanics, and a PHI guard that scans the source for hard-coded identifiers.

```r
# from the repository root
Rscript tests/testthat.R
```

**Runtime validation** runs the full pipeline on your local export and asserts
the invariants the analysis depends on (all inmates male, no PHI columns survive,
one row per patient, matching covariates complete, balance achieved). It writes a
pass/fail report to `output/validation_report.txt`.

```r
source("validate.R")
```

## Layout

```
config.example.R          # template; copy to config.R (git-ignored) and edit
run_all.R                 # runs the whole pipeline in order
validate.R                # runtime data-quality checks on the real cohort
run_outcomes.R            # runs the outcome analysis on the matched cohort
R/utils.R                 # shared, dependency-free helpers
R/00_setup.R              # packages, config load
R/01_import.R             # read the raw registry export
R/02_recode.R             # decode VQI numeric codes to clinical variables
R/03_cohort.R             # apply cohort rules, de-identify
R/04_match.R              # propensity matching, balance, Table 1
R/05_outcomes.R           # outcome analysis (survival, MALE, readmission, access)
dev/                      # helper scripts (synthetic outcomes, prefilled workbook, REDCap import)
redcap/                   # REDCap project XML + data dictionary (no PHI)
tests/testthat.R          # unit-test runner
tests/testthat/           # synthetic-data tests (recode, cohort, match, PHI, outcomes)
data/                     # git-ignored: place raw data here if you prefer
output/                   # git-ignored: generated tables and figures
private/                  # git-ignored: study-ID crosswalk
```
