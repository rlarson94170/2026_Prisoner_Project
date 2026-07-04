# ---------------------------------------------------------------------------
# validate.R
# Runtime data-quality checks against the REAL cohort. Unlike the unit tests
# (which use synthetic data), this runs the full pipeline on your local export
# and asserts the invariants the analysis depends on. It needs config.R and the
# raw data, and it writes a plain-text report to the git-ignored output/ folder.
#
# From the repository root:  source("validate.R")
# ---------------------------------------------------------------------------

if (!file.exists(here::here("config.R"))) {
  message("config.R not found. Set it up first (see config.example.R), ",
          "or run the unit tests instead: Rscript tests/testthat.R")
} else {

  source(here::here("R", "00_setup.R"))
  source(here::here("R", "01_import.R"))
  source(here::here("R", "02_recode.R"))
  source(here::here("R", "03_cohort.R"))
  source(here::here("R", "04_match.R"))

  results <- list()
  check <- function(name, ok, note = "") {
    results[[length(results) + 1]] <<- list(
      name = name, ok = isTRUE(ok), note = note
    )
    message(sprintf("[%s] %s%s",
                    if (isTRUE(ok)) "PASS" else "FAIL",
                    name,
                    if (nzchar(note)) paste0(" - ", note) else ""))
  }

  raw    <- import_raw()
  proc   <- recode_registry(raw)
  cohort <- build_cohort(proc)
  analytic <- cohort$analytic

  # --- Invariants ----------------------------------------------------------
  check("Every inmate procedure in the raw export is male",
        all(as_code(raw[["Birth Sex"]])[as_code(raw[["Inmate"]]) == "1"] == "1",
            na.rm = TRUE))

  check("No hybrid procedures survive into the analytic cohort",
        !("hybrid" %in% names(analytic)))

  phi <- c("mrn", "MRN", "admit_date", "procedure_date", "discharge_date",
           "death_date", "ssdi_death_date")
  check("Analytic dataset carries no PHI columns",
        !any(phi %in% names(analytic)))

  check("One row per patient (study IDs unique)",
        anyDuplicated(analytic$study_id) == 0)

  check("Study IDs are well formed",
        all(grepl("^PID[0-9]{4}$", analytic$study_id)))

  check("Matching covariates are complete",
        all(stats::complete.cases(analytic[, match_variables])),
        note = sprintf("%d rows, %d covariates",
                       nrow(analytic), length(match_variables)))

  check("Both groups are present",
        all(c("Inmate", "Non-inmate") %in% as.character(analytic$inmate)),
        note = sprintf("inmate=%d, control-pool=%d",
                       sum(analytic$inmate == "Inmate"),
                       sum(analytic$inmate == "Non-inmate")))

  # --- Matching diagnostics ------------------------------------------------
  if (requireNamespace("MatchIt", quietly = TRUE) &&
      requireNamespace("cobalt", quietly = TRUE)) {
    res <- run_matching(analytic)

    # Pass/fail is judged on the covariates we actually match on.
    check("Matching covariates balanced after matching (max |SMD| < 0.10)",
          res$max_smd_matched < 0.10,
          note = sprintf("max |SMD| = %.3f", res$max_smd_matched))

    # Medications are reported, not balanced. Surface the biggest baseline gap
    # as information, without failing the run.
    bf <- res$balance$Balance
    med_rows <- grepl("^(statin|ace_arb|antiplatelet|anticoagulant|diabetes_insulin)",
                      rownames(bf))
    if (any(med_rows)) {
      worst <- rownames(bf)[med_rows][which.max(abs(bf$Diff.Adj[med_rows]))]
      message(sprintf("Note: largest reported baseline medication gap is %s (SMD %.3f); reported, not matched.",
                      worst, bf$Diff.Adj[med_rows][which.max(abs(bf$Diff.Adj[med_rows]))]))
    }
  } else {
    message("MatchIt/cobalt not installed; skipping matching diagnostics.")
  }

  # --- Write report --------------------------------------------------------
  report <- vapply(results, function(r) {
    sprintf("[%s] %s%s",
            if (r$ok) "PASS" else "FAIL",
            r$name,
            if (nzchar(r$note)) paste0(" - ", r$note) else "")
  }, character(1))
  n_fail <- sum(!vapply(results, `[[`, logical(1), "ok"))
  header <- c(
    "Validation report",
    paste0("Generated: ", format(Sys.time())),
    paste0("Result: ", if (n_fail == 0) "ALL CHECKS PASSED"
           else paste0(n_fail, " CHECK(S) FAILED")),
    strrep("-", 60)
  )
  writeLines(c(header, report),
             here::here("output", "validation_report.txt"))

  message("\nValidation report written to ",
          here::here("output", "validation_report.txt"))
  if (n_fail > 0) warning(n_fail, " validation check(s) failed.", call. = FALSE)
}
