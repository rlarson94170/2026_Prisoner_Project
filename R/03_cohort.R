# ---------------------------------------------------------------------------
# 03_cohort.R
# Apply the pre-specified cohort rules and return a de-identified,
# admission-level analytic dataset (one row per patient's index admission).
#
# Rules (see the SAP):
#  1. Purely endovascular: drop hybrid procedures.
#  2. Male patients only (every inmate is male).
#  3. Patients treated as both inmate and non-inmate are classified as inmate;
#     their non-inmate admissions are removed from the control pool.
#  4. Unit = admission. Within an admission keep the first (earliest) procedure.
#  5. One index admission per patient: for inmates the first admission during
#     incarceration; for non-inmates the first qualifying admission.
# ---------------------------------------------------------------------------

build_cohort <- function(proc,
                         out_dir     = here::here("output"),
                         private_dir = here::here("private")) {

  dir.create(out_dir,     showWarnings = FALSE, recursive = TRUE)
  dir.create(private_dir, showWarnings = FALSE, recursive = TRUE)

  n0 <- nrow(proc)

  # Rule 2 + basic eligibility: males with an evaluable admission date.
  proc <- proc %>%
    dplyr::filter(male, !is.na(admit_date), !is.na(procedure_date))
  n_male <- nrow(proc)

  # Rule 1: purely endovascular.
  proc <- proc %>% dplyr::filter(!hybrid)
  n_endo <- nrow(proc)

  # Rule 3: identify patients ever flagged as inmates; for those patients keep
  # only their inmate procedures (drop their non-inmate admissions). Done by
  # logic, so no MRN is ever hard-coded.
  inmate_patients <- proc %>%
    dplyr::group_by(mrn) %>%
    dplyr::summarise(ever_inmate = any(inmate == 1), .groups = "drop") %>%
    dplyr::filter(ever_inmate) %>%
    dplyr::pull(mrn)

  proc <- proc %>%
    dplyr::filter(!(mrn %in% inmate_patients & inmate == 0))
  n_after_crossover <- nrow(proc)

  # Rule 4: collapse each admission (MRN + admit date) to its first procedure.
  admissions <- proc %>%
    dplyr::group_by(mrn, admit_date) %>%
    dplyr::arrange(procedure_date, .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()
  n_admissions <- nrow(admissions)

  # Patient-level exposure: inmate if this patient is in the inmate set.
  admissions <- admissions %>%
    dplyr::mutate(inmate = as.integer(mrn %in% inmate_patients))

  # Rule 5: one index admission per patient (earliest admit date).
  index <- admissions %>%
    dplyr::group_by(mrn) %>%
    dplyr::arrange(admit_date, .by_group = TRUE) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup()

  # Require the covariates used for matching to be present.
  index <- index %>%
    dplyr::filter(!is.na(age), !is.na(limb_severity))
  n_index <- nrow(index)

  # ---- De-identify --------------------------------------------------------
  # Assign a study ID, then split off identifiers. The crosswalk is written
  # only to the git-ignored private/ folder; the analytic file carries no MRN
  # and no dates.
  index <- index %>%
    dplyr::arrange(inmate, mrn) %>%
    dplyr::mutate(study_id = sprintf("PID%04d", dplyr::row_number()))

  crosswalk <- index %>%
    dplyr::select(study_id, mrn, admit_date, procedure_date,
                  discharge_date, death_date, ssdi_death_date)
  saveRDS(crosswalk, file.path(private_dir, "id_crosswalk.rds"))

  analytic <- index %>%
    dplyr::select(
      study_id, inmate,
      age, bmi, race, urgency, ambulation,
      coronary_disease, chf_symptomatic, copd_treated,
      diabetes_any, diabetes_insulin, dialysis, hypertension, current_smoker,
      statin, ace_arb, antiplatelet, anticoagulant,
      prior_ipsi_revasc, prior_amputation,
      clti, limb_severity, presentation
    ) %>%
    dplyr::mutate(
      inmate = factor(inmate, levels = c(0, 1),
                      labels = c("Non-inmate", "Inmate")),
      dplyr::across(where(is.logical), as.integer),
      dplyr::across(c(race, urgency, ambulation, presentation), as.factor)
    )

  # ---- Cohort flow (counts only; no PHI) ----------------------------------
  flow <- tibble::tibble(
    step = c("Raw procedures",
             "Male procedures",
             "After excluding hybrids",
             "After dropping crossover non-inmate admissions",
             "Distinct admissions (first procedure each)",
             "Index admissions (one per patient, covariates present)"),
    n = c(n0, n_male, n_endo, n_after_crossover, n_admissions, n_index)
  )
  readr::write_csv(flow, file.path(out_dir, "cohort_flow.csv"))

  message("03_cohort.R: analytic cohort n = ", nrow(analytic),
          " (inmate = ", sum(analytic$inmate == "Inmate"),
          ", non-inmate = ", sum(analytic$inmate == "Non-inmate"), ").")

  list(analytic = analytic, flow = flow)
}
