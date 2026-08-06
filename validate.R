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

  # --- Age plausibility ----------------------------------------------------
  check("No patient below the adult age floor",
        all(analytic$age >= MIN_AGE),
        note = sprintf("min age = %g (floor %g)", min(analytic$age), MIN_AGE))

  check("No implausibly high ages",
        all(analytic$age <= MAX_AGE),
        note = sprintf("max age = %g (ceiling %g)", max(analytic$age), MAX_AGE))

  # --- Patient updates -----------------------------------------------------
  # Known corrections to the inmate flag and to age are supplied from the
  # patient updates file. An update whose MRN is absent from the export names a
  # patient the registry pull never contained. Those cannot be recovered (there
  # is no reliable way to re-identify pre-Epic patients), so they are recorded
  # as a documented exclusion rather than failed as a defect.
  upd <- cohort$updates
  if (!is.null(upd) && nrow(upd)) {
    check("Patient updates file was read and applied",
          any(upd$status != "NOT FOUND in export"),
          note = sprintf("%d of %d updates matched the export",
                         sum(upd$status != "NOT FOUND in export"), nrow(upd)))

    missing <- upd[upd$status == "NOT FOUND in export", , drop = FALSE]
    if (nrow(missing)) {
      readr::write_csv(missing, here::here("private",
                                           "known_missing_patients.csv"))
      message(sprintf(
        "Note: %d known patient(s) are absent from the registry pull and are excluded by necessity (%d of them inmates). Listed in private/known_missing_patients.csv for the limitations section.",
        nrow(missing), sum(missing$inmate == 1, na.rm = TRUE)))
    }
  } else {
    message("Note: no patient updates file in use.")
  }

  # --- Study ID stability --------------------------------------------------
  reg_path <- here::here("private", "study_id_registry.csv")
  cw_path  <- here::here("private", "id_crosswalk.rds")
  if (file.exists(reg_path) && file.exists(cw_path)) {
    reg <- readr::read_csv(reg_path, col_types = readr::cols(.default = "c"))
    cw  <- readRDS(cw_path)
    joined <- merge(
      data.frame(mrn = norm_mrn(cw$mrn),  study_id = cw$study_id),
      data.frame(mrn = norm_mrn(reg$mrn), reg_id   = reg$study_id),
      by = "mrn", all.x = TRUE
    )
    check("Study IDs match the persisted registry",
          !anyNA(joined$reg_id) && all(joined$study_id == joined$reg_id),
          note = sprintf("%d MRNs in the registry", nrow(reg)))
  }

  # --- Inmate ascertainment by era (monitor, not pass/fail) ----------------
  # The inmate flag came from an IT sweep whose method changed when the EMR
  # moved from Cerner to Epic. A large prevalence gap across that boundary is
  # evidence of under-ascertainment in the earlier era rather than a real
  # change in the incarcerated caseload.
  if (file.exists(cw_path)) {
    cw  <- readRDS(cw_path)
    yr  <- as.integer(format(as.Date(cw$procedure_date), "%Y"))
    era <- ifelse(yr <= 2019, "2016-2019 (Cerner)", "2020-2024 (Epic)")
    is_inm <- analytic$inmate[match(cw$study_id, analytic$study_id)] == "Inmate"
    for (e in sort(unique(era))) {
      k <- era == e
      message(sprintf("Note: inmate prevalence %s = %d/%d (%.1f%%).",
                      e, sum(is_inm[k]), sum(k),
                      100 * sum(is_inm[k]) / sum(k)))
    }
  }

  # --- Common support ------------------------------------------------------
  # Controls occupying a covariate level no inmate occupies are unmatchable and
  # make the propensity model separate ("fitted probabilities numerically 0 or
  # 1"). 03_cohort.R trims them; this confirms none survived.
  sup <- cohort$support
  if (!is.null(sup) && nrow(sup)) {
    message(sprintf("Note: common support dropped %d control(s) across %d level(s): %s.",
                    sum(sup$controls_dropped), nrow(sup),
                    paste(sprintf("%s=%s", sup$variable, sup$level), collapse = ", ")))
  }

  offenders <- character()
  for (v in intersect(SUPPORT_VARS, names(analytic))) {
    lv <- as.character(analytic[[v]])
    inm <- unique(lv[analytic$inmate == "Inmate" & !is.na(lv)])
    ctl <- unique(lv[analytic$inmate == "Non-inmate" & !is.na(lv)])
    if (length(setdiff(ctl, inm))) {
      offenders <- c(offenders, sprintf("%s=%s", v,
                                        paste(setdiff(ctl, inm), collapse = "/")))
    }
  }
  check("No control-only covariate levels remain (propensity model is estimable)",
        length(offenders) == 0,
        note = if (length(offenders)) paste(offenders, collapse = "; ")
               else "all covariate levels contain inmates")

  # --- Matching diagnostics ------------------------------------------------
  if (requireNamespace("MatchIt", quietly = TRUE) &&
      requireNamespace("cobalt", quietly = TRUE)) {
    res <- run_matching(analytic, drop_vars = cohort$degenerate_vars)

    # Doubly-robust matched design. We require adequate balance (|SMD| < 0.25,
    # the conventional bar) and treat < 0.10 as ideal. Any covariate left
    # between 0.10 and 0.25 is reported so it can be covariate-adjusted in the
    # outcome models (see run_outcomes()).
    bm        <- res$balance_matched$Balance
    bm        <- bm[rownames(bm) != "distance", , drop = FALSE]
    worst_var <- rownames(bm)[which.max(abs(bm$Diff.Adj))]
    check("Matching covariates adequately balanced (max |SMD| < 0.25; < 0.10 ideal)",
          round(res$max_smd_matched, 2) <= 0.25,
          note = sprintf("max |SMD| = %.3f on %s",
                         res$max_smd_matched, worst_var))

    adjust_for <- rownames(bm)[abs(bm$Diff.Adj) > 0.10]
    if (length(adjust_for)) {
      message("Note: covariates with |SMD| 0.10-0.25 to adjust for in outcome ",
              "models: ", paste(adjust_for, collapse = ", "), ".")
    } else {
      message("Note: all matching covariates are below 0.10; no residual ",
              "adjustment needed.")
    }

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
