# Prisoner PAD analysis

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

Nearest-neighbor matching on the logit of the propensity score, caliper 0.2 SD,
up to 3 controls per inmate, no replacement. Balance is judged by standardized
mean differences (target < 0.10), reported before and after matching, including
covariates that were not part of the matching model as a residual-balance check.

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
R/utils.R                 # shared, dependency-free helpers
R/00_setup.R              # packages, config load
R/01_import.R             # read the raw registry export
R/02_recode.R             # decode VQI numeric codes to clinical variables
R/03_cohort.R             # apply cohort rules, de-identify
R/04_match.R              # propensity matching, balance, Table 1
tests/testthat.R          # unit-test runner
tests/testthat/           # synthetic-data tests (recode, cohort, match, PHI)
data/                     # git-ignored: place raw data here if you prefer
output/                   # git-ignored: generated tables and figures
private/                  # git-ignored: study-ID crosswalk
```
