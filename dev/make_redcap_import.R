# ---------------------------------------------------------------------------
# dev/make_redcap_import.R
# Exports the matched cohort's reference fields as a REDCap data-import CSV, so
# the prefilled columns (study ID, MRN, inmate, index dates, presentation) are
# loaded into REDCap and abstractors only fill in the chart-based fields.
#
# Import it in REDCap via: Data Import Tool -> choose CSV -> upload.
#
# This file contains MRNs and dates, so it is written to the git-ignored
# private/ folder and must never be committed.
#
# After sourcing:
#   source("dev/make_redcap_import.R")
#   generate_redcap_import()
# ---------------------------------------------------------------------------

build_redcap_import <- function(matched, crosswalk, v1_link = NULL) {
  ref <- dplyr::left_join(
    dplyr::select(matched, dplyr::any_of(c("study_id", "inmate", "presentation"))),
    crosswalk, by = "study_id"
  )
  ymd <- function(d) ifelse(is.na(d), "", format(as.Date(d), "%Y-%m-%d"))

  out <- tibble::tibble(
    study_id         = ref$study_id,
    mrn              = as.character(ref$mrn),
    inmate           = as.integer(ref$inmate == "Inmate"),                 # 1 / 0
    index_admit_date = ymd(ref$admit_date),
    index_proc_date  = ymd(ref$procedure_date),
    index_dc_date    = ymd(ref$discharge_date),
    presentation     = .presentation_code(ref$presentation)
  )

  # Optional: carry the 2020 abstraction into REDCap. Same rules as the Excel
  # workbook. Fields whose definition is unchanged are prefilled into the answer
  # fields (editable, so the abstractor verifies rather than retypes); fields
  # whose definition changed stay empty and the 2020 free text goes into
  # v1_source_text for re-adjudication. Dates are written as Y-M-D because that
  # is what the REDCap import expects, not the M/D/Y the fields display.
  if (!is.null(v1_link)) {
    if (!exists("prefill_from_v1")) {
      stop("Source dev/link_v1_abstraction.R before passing v1_link.", call. = FALSE)
    }
    blanks <- c("vital_status", "death_date", "last_alive_date", "major_amp_date",
                "first_fu_date", "days_to_first_fu", "first_readmit_date",
                "n_readmit_1yr", "v1_source_text")
    for (nm in blanks) out[[nm]] <- NA_character_
    out$v1_reused <- 0L

    out <- prefill_from_v1(v1_link, out)
    out$v1_reused[is.na(out$v1_reused)] <- 0L

    # prefill_from_v1() formats dates M/D/Y for the Excel workbook; REDCap wants
    # Y-M-D on import.
    for (nm in c("death_date", "last_alive_date", "major_amp_date",
                 "first_fu_date", "first_readmit_date")) {
      out[[nm]] <- ifelse(is.na(out[[nm]]), NA_character_,
                          format(as.Date(out[[nm]], format = "%m/%d/%Y"),
                                 "%Y-%m-%d"))
    }
    message("build_redcap_import(): carried over 2020 data for ",
            sum(out$v1_reused == 1L), " of ", nrow(out), " records.")
  }

  out
}

# REDCap coding for the prefilled `presentation` field. Asymptomatic patients
# (limb severity 0) are a real part of this cohort - 30 of the 658 supported
# patients - so they get their own code rather than being folded into
# claudication. Keep this in step with the CodeList in
# REDCap_Outcome_Abstraction_Project.xml and the Choices column in
# REDCap_Outcome_Abstraction_DataDictionary.csv.
.presentation_code <- function(x) {
  x <- as.character(x)
  out <- dplyr::case_when(
    x == "Asymptomatic" ~ 0L,
    x == "Claudication" ~ 1L,
    x == "CLTI"         ~ 2L,
    TRUE                ~ NA_integer_
  )
  if (anyNA(out)) {
    stop("Unmapped presentation value(s): ",
         paste(unique(x[is.na(out)]), collapse = ", "),
         ". Add the code to .presentation_code(), the REDCap data dictionary ",
         "and the project XML together.", call. = FALSE)
  }
  out
}

generate_redcap_import <- function(
    matched_path   = here::here("output",  "matched_cohort.rds"),
    crosswalk_path = here::here("private", "id_crosswalk.rds"),
    out_path       = here::here("private", "redcap_import.csv"),
    v1_link        = NULL) {

  if (!file.exists(matched_path) || !file.exists(crosswalk_path)) {
    stop("Need output/matched_cohort.rds and private/id_crosswalk.rds. ",
         "Run run_all.R first.", call. = FALSE)
  }
  imp <- build_redcap_import(readRDS(matched_path), readRDS(crosswalk_path),
                             v1_link = v1_link)
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  readr::write_csv(imp, out_path, na = "")
  message("Wrote ", out_path, " (", nrow(imp),
          " records; keep this file off GitHub).")
  invisible(out_path)
}

if (sys.nframe() == 0) generate_redcap_import()
