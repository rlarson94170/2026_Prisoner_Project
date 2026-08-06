# ---------------------------------------------------------------------------
# 03_cohort.R
# Apply the pre-specified cohort rules and return a de-identified,
# admission-level analytic dataset (one row per patient's index admission).
#
# Rules (see the SAP):
#  1. Purely endovascular: drop hybrid procedures.
#  2. Male patients only (every inmate is male).
#  3. Adults only: age >= MIN_AGE at the procedure. The export contains a
#     handful of implausible ages (0, 5, 13) that are data-entry errors, and
#     without a floor they enter the control pool and can be matched to an
#     adult inmate.
#  4. Patients treated as both inmate and non-inmate are classified as inmate;
#     their non-inmate admissions are removed from the control pool.
#  5. Unit = admission. Within an admission keep the first (earliest) procedure.
#  6. One index admission per patient: for inmates the first admission during
#     incarceration; for non-inmates the first qualifying admission.
#
# Patient updates
# ---------------
# Two registry fields are known to be unreliable and are corrected from a
# manually maintained lookup file (see PATIENT_UPDATES_PATH in config.R):
#
#   Inmate  the flag comes from an IT sweep whose sensitivity differs by era.
#           The EMR moved from Cerner to Epic mid-study and the inmate
#           identification process changed with it, so the Cerner era is
#           under-ascertained. Pre-Epic patients cannot be re-identified
#           reliably, so the corrections we do have are applied by hand.
#
#   DOB     the export's "Age at Procedure" field is corrupt for at least one
#           patient (recorded as 0 and 5 for a man who was 72 and 77). Supplying
#           a DOB recomputes age for every one of that patient's procedures,
#           which is what keeps the adult floor from discarding a valid patient
#           over a data-entry error.
#
# Corrections live with the data, not in the code, so the list stays auditable
# and no MRN is ever hard-coded here.
#
# Study IDs
# ---------
# Study IDs are assigned from a persisted registry (private/study_id_registry.csv)
# so that an MRN keeps the same PID across re-runs. Before this change IDs were
# positional, so adding a single patient renumbered the whole cohort and
# invalidated any abstraction already keyed to a PID. IDs are assigned in MRN
# order and deliberately do NOT encode exposure.
# ---------------------------------------------------------------------------

MIN_AGE  <- 18    # eligibility floor
MAX_AGE  <- 110   # implausibility ceiling (reported, not filtered)

# Registry MRNs sometimes carry a leading zero ("04221952") while other sources
# store them as numbers. Normalise before any MRN comparison or join.
norm_mrn <- function(x) sub("^0+", "", trimws(as.character(x)))

# ---------------------------------------------------------------------------
# Patient updates
#
# Reads .xlsx or .csv. Column names are matched case-insensitively and the
# spellings used in Patient Updates.xlsx are accepted directly:
#
#   MRN            required
#   Inmate         optional: yes/no, y/n, true/false, or 1/0
#   DOB            optional: date of birth, used to recompute age
#   Surgery Date   optional: scopes an Inmate correction to one procedure.
#                  Ignored for DOB, which is patient-level and always applies
#                  to every procedure for that MRN.
#   Note           optional free text, carried into the audit log
#
# Returns the updated procedure table with an attribute "updates_log" recording
# what was applied to how many rows, and what could not be matched at all.
# ---------------------------------------------------------------------------
apply_patient_updates <- function(proc, path = NULL) {

  empty_log <- tibble::tibble(
    mrn = character(), procedure_date = as.Date(character()),
    inmate = integer(), dob = as.Date(character()),
    fields = character(), status = character(), note = character()
  )

  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    attr(proc, "updates_log") <- empty_log
    return(proc)
  }

  upd <- if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    readxl::read_excel(path)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }

  # Accept either the Excel spellings or the tidy ones.
  nm <- tolower(trimws(names(upd)))
  pick <- function(...) {
    hit <- which(nm %in% c(...))
    if (length(hit)) upd[[hit[1]]] else NULL
  }
  mrn_col  <- pick("mrn")
  inm_col  <- pick("inmate")
  dob_col  <- pick("dob", "date of birth", "birth date")
  date_col <- pick("surgery date", "procedure_date", "procedure date")
  note_col <- pick("note", "notes")

  if (is.null(mrn_col)) {
    stop("Patient updates file has no MRN column: ", path, call. = FALSE)
  }

  as_flag <- function(x) {
    if (is.null(x)) return(rep(NA_integer_, length(mrn_col)))
    x <- tolower(trimws(as.character(x)))
    dplyr::case_when(
      x %in% c("1", "yes", "y", "true", "t")  ~ 1L,
      x %in% c("0", "no",  "n", "false", "f") ~ 0L,
      TRUE                                    ~ NA_integer_
    )
  }

  upd <- tibble::tibble(
    mrn            = norm_mrn(mrn_col),
    inmate         = as_flag(inm_col),
    dob            = if (is.null(dob_col))  as.Date(NA) else as.Date(dob_col),
    procedure_date = if (is.null(date_col)) as.Date(NA) else as.Date(date_col),
    note           = if (is.null(note_col)) NA_character_ else as.character(note_col)
  )

  proc <- dplyr::mutate(proc, mrn = norm_mrn(.data$mrn))

  status <- character(nrow(upd))
  fields <- character(nrow(upd))

  for (i in seq_len(nrow(upd))) {
    in_pt <- proc$mrn == upd$mrn[i]
    in_pt[is.na(in_pt)] <- FALSE

    if (!any(in_pt)) {
      status[i] <- "NOT FOUND in export"
      fields[i] <- NA_character_
      next
    }

    changed <- character(0)

    # DOB is patient-level: recompute age for every procedure of this patient.
    if (!is.na(upd$dob[i])) {
      new_age <- as.integer(floor(
        as.numeric(proc$procedure_date[in_pt] - upd$dob[i]) / 365.25
      ))
      if (!identical(proc$age[in_pt], as.numeric(new_age))) {
        changed <- c(changed, sprintf("age (%s -> %s)",
                                      paste(proc$age[in_pt], collapse = "/"),
                                      paste(new_age, collapse = "/")))
      }
      proc$age[in_pt] <- new_age
    }

    # Inmate is procedure-level when a date is supplied.
    if (!is.na(upd$inmate[i])) {
      hit <- in_pt & (is.na(upd$procedure_date[i]) |
                        proc$procedure_date == upd$procedure_date[i])
      hit[is.na(hit)] <- FALSE
      if (any(hit) && any(proc$inmate[hit] != upd$inmate[i])) {
        changed <- c(changed, sprintf("inmate -> %d", upd$inmate[i]))
      }
      proc$inmate[hit] <- upd$inmate[i]
    }

    status[i] <- sprintf("matched %d row(s)", sum(in_pt))
    fields[i] <- if (length(changed)) paste(changed, collapse = "; ") else "no change"
  }

  log <- tibble::tibble(
    mrn = upd$mrn, procedure_date = upd$procedure_date,
    inmate = upd$inmate, dob = upd$dob,
    fields = fields, status = status, note = upd$note
  )

  missing <- log$mrn[log$status == "NOT FOUND in export"]
  if (length(missing)) {
    warning(
      length(missing), " patient update(s) reference MRNs absent from the ",
      "export: ", paste(missing, collapse = ", "),
      ". Those patients are missing from the registry pull, not merely ",
      "mis-coded, so they cannot be added here.",
      call. = FALSE
    )
  }
  message("03_cohort.R: matched ", sum(log$status != "NOT FOUND in export"),
          " of ", nrow(log), " patient updates.")

  attr(proc, "updates_log") <- log
  proc
}

# ---------------------------------------------------------------------------
# Stable study IDs
#
# Reads (or creates) a persisted MRN -> study_id registry. MRNs already in the
# registry keep their ID; new MRNs are appended in sorted order continuing from
# the highest existing number. Returns a character vector aligned to `mrn`.
#
# To renumber from scratch, delete the registry file. Do NOT do that once
# abstraction has begun: PIDs are the key on the abstraction records.
# ---------------------------------------------------------------------------
assign_study_ids <- function(mrn, registry_path) {

  mrn <- norm_mrn(mrn)

  registry <- if (file.exists(registry_path)) {
    readr::read_csv(registry_path, col_types = readr::cols(.default = "c")) %>%
      dplyr::mutate(mrn = norm_mrn(.data$mrn))
  } else {
    tibble::tibble(study_id = character(), mrn = character())
  }

  new_mrns <- sort(setdiff(unique(mrn), registry$mrn))

  if (length(new_mrns)) {
    start <- if (nrow(registry)) {
      max(as.integer(sub("^PID", "", registry$study_id))) + 1L
    } else {
      1L
    }
    registry <- dplyr::bind_rows(
      registry,
      tibble::tibble(
        study_id = sprintf("PID%04d", seq(start, length.out = length(new_mrns))),
        mrn      = new_mrns
      )
    )
    dir.create(dirname(registry_path), showWarnings = FALSE, recursive = TRUE)
    readr::write_csv(registry, registry_path)
    message("03_cohort.R: assigned ", length(new_mrns),
            " new study ID(s); registry now holds ", nrow(registry), ".")
  }

  registry$study_id[match(mrn, registry$mrn)]
}

# ---------------------------------------------------------------------------

build_cohort <- function(proc,
                         out_dir          = here::here("output"),
                         private_dir      = here::here("private"),
                         min_age          = MIN_AGE,
                         updates_path     = if (exists("PATIENT_UPDATES_PATH")) {
                                              PATIENT_UPDATES_PATH
                                            } else NULL,
                         id_registry_path = file.path(private_dir,
                                                      "study_id_registry.csv")) {

  dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
  dir.create(private_dir, showWarnings = FALSE, recursive = TRUE)

  n0 <- nrow(proc)

  # Rule 0: apply known corrections to exposure and age before anything
  # depends on them (the crossover rule needs the inmate flag; the adult floor
  # needs the age).
  proc <- apply_patient_updates(proc, updates_path)
  updates_log <- attr(proc, "updates_log")
  if (!is.null(updates_log) && nrow(updates_log)) {
    readr::write_csv(updates_log,
                     file.path(private_dir, "patient_updates_log.csv"))
  }

  # Rule 2 + basic eligibility: males with an evaluable admission date.
  proc <- proc %>%
    dplyr::filter(.data$male, !is.na(.data$admit_date),
                  !is.na(.data$procedure_date))
  n_male <- nrow(proc)

  # Rule 1: purely endovascular.
  proc <- proc %>% dplyr::filter(!.data$hybrid)
  n_endo <- nrow(proc)

  # Rule 3: adults only. Ages above MAX_AGE are reported but not dropped;
  # they are rare and plausible enough to keep.
  n_implausible_high <- sum(proc$age > MAX_AGE, na.rm = TRUE)
  if (n_implausible_high > 0) {
    warning(n_implausible_high, " procedure(s) have age > ", MAX_AGE,
            "; retained but worth checking.", call. = FALSE)
  }
  dropped_minors <- proc %>%
    dplyr::filter(!is.na(.data$age), .data$age < min_age) %>%
    dplyr::distinct(.data$mrn, .data$procedure_date, .data$age)
  if (nrow(dropped_minors)) {
    readr::write_csv(dropped_minors,
                     file.path(private_dir, "excluded_under_age.csv"))
    message("03_cohort.R: dropped ", nrow(dropped_minors),
            " procedure(s) with age < ", min_age,
            " (listed in private/excluded_under_age.csv).")
  }
  proc <- proc %>% dplyr::filter(!is.na(.data$age), .data$age >= min_age)
  n_adult <- nrow(proc)

  # Rule 4: identify patients ever flagged as inmates; for those patients keep
  # only their inmate procedures (drop their non-inmate admissions). Done by
  # logic, so no MRN is ever hard-coded.
  inmate_patients <- proc %>%
    dplyr::group_by(.data$mrn) %>%
    dplyr::summarise(ever_inmate = any(.data$inmate == 1), .groups = "drop") %>%
    dplyr::filter(.data$ever_inmate) %>%
    dplyr::pull(.data$mrn)

  proc <- proc %>%
    dplyr::filter(!(.data$mrn %in% inmate_patients & .data$inmate == 0))
  n_after_crossover <- nrow(proc)

  # Rule 5: collapse each admission (MRN + admit date) to its first procedure.
  admissions <- proc %>%
    dplyr::group_by(.data$mrn, .data$admit_date) %>%
    dplyr::arrange(.data$procedure_date, .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  n_admissions <- nrow(admissions)

  # Patient-level exposure: inmate if this patient is in the inmate set.
  admissions <- admissions %>%
    dplyr::mutate(inmate = as.integer(.data$mrn %in% inmate_patients))

  # Rule 6: one index admission per patient (earliest admit date).
  index <- admissions %>%
    dplyr::group_by(.data$mrn) %>%
    dplyr::arrange(.data$admit_date, .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  # Require the covariates used for matching to be present.
  index <- index %>%
    dplyr::filter(!is.na(.data$age), !is.na(.data$limb_severity))
  n_index <- nrow(index)

  # ---- De-identify --------------------------------------------------------
  # Study IDs come from the persisted registry, so a given MRN keeps its PID
  # across re-runs. The crosswalk is written only to the git-ignored private/
  # folder; the analytic file carries no MRN and no dates.
  index <- index %>%
    dplyr::arrange(.data$mrn) %>%
    dplyr::mutate(study_id = assign_study_ids(.data$mrn, id_registry_path))

  stopifnot(!anyNA(index$study_id), anyDuplicated(index$study_id) == 0)

  crosswalk <- index %>%
    dplyr::select("study_id", "mrn", "admit_date", "procedure_date",
                  "discharge_date", "death_date", "ssdi_death_date")
  saveRDS(crosswalk, file.path(private_dir, "id_crosswalk.rds"))

  analytic <- index %>%
    dplyr::select(
      "study_id", "inmate",
      "age", "bmi", "race", "urgency", "ambulation",
      "coronary_disease", "chf_symptomatic", "copd_treated",
      "diabetes_any", "diabetes_insulin", "dialysis", "hypertension",
      "current_smoker",
      "statin", "ace_arb", "antiplatelet", "anticoagulant",
      "prior_ipsi_revasc", "prior_amputation",
      "clti", "limb_severity", "presentation"
    ) %>%
    dplyr::mutate(
      inmate = factor(.data$inmate, levels = c(0, 1),
                      labels = c("Non-inmate", "Inmate")),
      dplyr::across(where(is.logical), as.integer),
      dplyr::across(c("race", "urgency", "ambulation", "presentation"),
                    as.factor)
    )

  # ---- Cohort flow (counts only; no PHI) ----------------------------------
  flow <- tibble::tibble(
    step = c("Raw procedures",
             "Male procedures",
             "After excluding hybrids",
             paste0("After excluding age < ", min_age),
             "After dropping crossover non-inmate admissions",
             "Distinct admissions (first procedure each)",
             "Index admissions (one per patient, covariates present)"),
    n = c(n0, n_male, n_endo, n_adult, n_after_crossover, n_admissions, n_index)
  )
  readr::write_csv(flow, file.path(out_dir, "cohort_flow.csv"))

  message("03_cohort.R: analytic cohort n = ", nrow(analytic),
          " (inmate = ", sum(analytic$inmate == "Inmate"),
          ", non-inmate = ", sum(analytic$inmate == "Non-inmate"), ").")

  list(analytic = analytic, flow = flow, updates = updates_log)
}
