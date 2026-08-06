# ---------------------------------------------------------------------------
# dev/link_v1_abstraction.R
#
# Link the 2020 ("version 1") chart abstraction to the current matched cohort
# so previously abstracted charts are not re-abstracted.
#
# The v1 work lives in Data/Prisoner Project version 1 Data/ and covers
# 17 inmates + 34 matched controls (48 of the 51 were actually abstracted),
# index procedures 2016-01 to 2019-12, censored 2020-03-23.
#
# Linkage key: MRN + index procedure date. MRN alone is NOT sufficient because
# the current pipeline picks one index admission per patient by rule
# (03_cohort.R rule 5), which need not be the procedure v1 abstracted.
#
# This file reads MRNs and dates. It writes only to private/. Never commit
# its outputs.
#
# Usage:
#   source("R/00_setup.R"); source("dev/link_v1_abstraction.R")
#   res <- link_v1_abstraction()
# ---------------------------------------------------------------------------

V1_DIR_DEFAULT <- file.path(
  "~", "Documents", "AI_PROJECTS", "Prisoner Project",
  "Claude Prisoner Project Folder", "Data", "Prisoner Project version 1 Data"
)

V1_CENSOR_DATE <- as.Date("2020-03-23")   # "Today" column in the v1 workbook

# The registry export stores some MRNs with a leading zero ("04221952") while
# the v1 workbooks store them as numbers. Normalise both sides before joining.
norm_mrn <- function(x) sub("^0+", "", trimws(as.character(x)))

# ---- Readers --------------------------------------------------------------

read_v1 <- function(v1_dir = V1_DIR_DEFAULT) {

  v1_dir <- path.expand(v1_dir)

  f_detail <- file.path(v1_dir, "20200323 FINAL Data v2.xlsx")
  f_readmit <- file.path(v1_dir, "Inmate:noninmate readmission data.xlsx")
  f_matched <- file.path(v1_dir, "Matched_Prisoner_NonPrisoner.xlsx")

  for (f in c(f_detail, f_readmit, f_matched)) {
    if (!file.exists(f)) stop("v1 file not found: ", f, call. = FALSE)
  }

  detail <- readxl::read_excel(f_detail, sheet = "Detailed chart w outcomes") %>%
    dplyr::rename_with(~ gsub(" ", " ", .x)) %>%   # a header holds nbsp
    dplyr::transmute(
      v1_inmate        = .data$Inmate == "Y",
      mrn              = norm_mrn(.data$MRN),
      v1_proc_date     = as.Date(.data$`Procedure date`),
      v1_admit_date    = as.Date(.data$`Admit date`),
      v1_dc_date       = as.Date(.data$`d/c date`),
      v1_reint_any     = .data$Reintervention == "Y",
      v1_n_reint       = as.integer(.data$`# of reinterventions`),
      v1_reint1_date   = as.Date(.data$`1st reinterv`),
      v1_reint2_date   = as.Date(.data$`2nd reinterv`),
      v1_first_fu_date = as.Date(.data$`1st f/u`),
      v1_last_fu_date  = as.Date(.data$`Last f/u date`),
      v1_ltf_flag      = .data$`Loss to f/u?` == "Y",
      v1_amp_any       = .data$Amputation == "Y",
      v1_amp_date      = as.Date(.data$`Amputation date`),
      v1_death_date    = as.Date(.data$`Death date`),
      v1_details       = .data$Details
    )

  readmit_raw <- readxl::read_excel(f_readmit, sheet = "Readmission", skip = 1)
  rcols <- c("#1", "#2", "#3", "#4", "#5", "#6", "#7")
  readmit <- readmit_raw %>%
    dplyr::transmute(
      mrn                = norm_mrn(.data$MRN),
      v1_proc_date       = as.Date(.data$`Procedure date`),
      v1_readmit_dates   = apply(
        dplyr::across(dplyr::all_of(rcols), ~ as.Date(.x)), 1L,
        function(x) list(sort(as.Date(x[!is.na(x)], origin = "1970-01-01")))
      ) %>% unlist(recursive = FALSE),
      v1_readmit_details = .data$Details
    )

  matched <- readxl::read_excel(f_matched) %>%
    dplyr::transmute(
      mrn            = norm_mrn(.data$mrn),
      v1_inmate      = .data$inmate == 1,
      v1_proc_date   = as.Date(.data$procedure_date)
    )

  # Derived fields that the current dictionary asks for, recomputed under the
  # current definitions rather than copied from v1's derived columns.
  detail <- detail %>%
    dplyr::left_join(dplyr::select(readmit, -"v1_proc_date"), by = "mrn") %>%
    dplyr::mutate(
      # current spec: days from DISCHARGE to first follow-up (v1 measured from
      # the procedure date, so this is recomputed, not copied)
      v1_days_to_first_fu = as.integer(.data$v1_first_fu_date - .data$v1_dc_date),
      v1_obs_days         = as.integer(V1_CENSOR_DATE - .data$v1_proc_date),
      v1_fu_days          = as.integer(.data$v1_last_fu_date - .data$v1_proc_date),
      v1_full_1yr_window  = .data$v1_obs_days >= 365L,
      v1_n_readmit_1yr    = vapply(
        seq_len(dplyr::n()),
        function(i) {
          dts <- .data$v1_readmit_dates[[i]]
          if (length(dts) == 0L) return(0L)
          sum(as.integer(dts - .data$v1_proc_date[i]) %in% 1:365)
        }, integer(1)
      ),
      v1_first_readmit_date = as.Date(vapply(
        .data$v1_readmit_dates,
        function(d) if (length(d)) as.character(min(d)) else NA_character_,
        character(1)
      ))
    )

  list(detail = detail, readmit = readmit, matched = matched)
}

# ---- Linkage --------------------------------------------------------------

#' Link v1 abstraction to the current matched cohort.
#'
#' @param matched   output/matched_cohort.rds (study_id + inmate)
#' @param crosswalk private/id_crosswalk.rds  (study_id + mrn + dates)
#' @param v1_dir    folder holding the three v1 workbooks
#' @param date_tol  days of tolerance when comparing index procedure dates
#' @return list(link, v1_unused, summary)
link_v1_abstraction <- function(
    matched    = readRDS(here::here("output", "matched_cohort.rds")),
    crosswalk  = readRDS(here::here("private", "id_crosswalk.rds")),
    v1_dir     = V1_DIR_DEFAULT,
    date_tol   = 0L,
    private_dir = here::here("private")) {

  dir.create(private_dir, showWarnings = FALSE, recursive = TRUE)
  v1 <- read_v1(v1_dir)

  cur <- matched %>%
    dplyr::select(dplyr::any_of(c("study_id", "inmate", "subclass", "weights"))) %>%
    dplyr::left_join(
      dplyr::mutate(crosswalk, mrn = norm_mrn(.data$mrn)),
      by = "study_id"
    ) %>%
    dplyr::mutate(cur_proc_date = as.Date(.data$procedure_date))

  link <- cur %>%
    dplyr::left_join(v1$detail, by = "mrn") %>%
    dplyr::mutate(
      date_gap = as.integer(abs(.data$cur_proc_date - .data$v1_proc_date)),
      v1_status = dplyr::case_when(
        is.na(.data$v1_proc_date)          ~ "not_abstracted",
        .data$date_gap <= date_tol         ~ "reusable_same_index",
        TRUE                               ~ "same_patient_other_index"
      ),
      # v1 stopped following on 2020-03-23; a reusable row still needs a full
      # year of observation for the 1-year outcomes to be complete.
      v1_1yr_complete = .data$v1_status == "reusable_same_index" &
        !is.na(.data$v1_obs_days) & .data$v1_obs_days >= 365L,
      exposure_agrees = is.na(.data$v1_inmate) |
        (.data$v1_inmate == (.data$inmate == "Inmate"))
    )

  # v1 charts that the current cohort does not use at all (wasted or
  # re-usable-if-the-match-changes).
  v1_unused <- v1$detail %>%
    dplyr::anti_join(dplyr::select(link, "mrn"), by = "mrn") %>%
    dplyr::select("mrn", "v1_inmate", "v1_proc_date")

  # v1 patients selected for matching in 2020 but never actually abstracted.
  v1_never_abstracted <- v1$matched %>%
    dplyr::anti_join(dplyr::select(v1$detail, "mrn"), by = "mrn")

  summary <- link %>%
    dplyr::count(.data$inmate, .data$v1_status, .data$v1_1yr_complete,
                 name = "n")

  out <- list(
    link                = link,
    v1_unused           = v1_unused,
    v1_never_abstracted = v1_never_abstracted,
    summary             = summary
  )

  saveRDS(out, file.path(private_dir, "v1_linkage.rds"))
  readr::write_csv(
    dplyr::select(link, "study_id", "mrn", "inmate", "cur_proc_date",
                  "v1_proc_date", "date_gap", "v1_status", "v1_1yr_complete",
                  "exposure_agrees"),
    file.path(private_dir, "v1_linkage.csv")
  )

  message("link_v1_abstraction(): ",
          sum(link$v1_status == "reusable_same_index"), " of ", nrow(link),
          " matched patients already abstracted at the same index procedure (",
          sum(link$v1_1yr_complete), " with a full 1-year window); ",
          sum(link$v1_status == "same_patient_other_index"),
          " abstracted at a different index; ",
          nrow(v1_unused), " v1 charts unused by the current match.")

  out
}

# ---- Prefill --------------------------------------------------------------

#' Prefill the abstraction workbook with everything v1 can supply.
#'
#' Only fields whose v1 definition matches the current dictionary are carried
#' over. Fields v1 recorded under a different definition (reintervention type
#' and laterality, readmission cause, loss to follow-up) are left blank and the
#' relevant v1 free text is dropped into `v1_source_text` so the abstractor can
#' re-adjudicate from the note instead of reopening the chart.
prefill_from_v1 <- function(link, workbook_df) {

  reusable <- link %>%
    dplyr::filter(.data$v1_status == "reusable_same_index") %>%
    dplyr::transmute(
      study_id,
      vital_status     = ifelse(is.na(.data$v1_death_date), "alive", "dead"),
      death_date       = format(.data$v1_death_date, "%m/%d/%Y"),
      last_alive_date  = format(.data$v1_last_fu_date, "%m/%d/%Y"),
      major_amp_date   = format(.data$v1_amp_date, "%m/%d/%Y"),
      first_fu_date    = format(.data$v1_first_fu_date, "%m/%d/%Y"),
      days_to_first_fu = .data$v1_days_to_first_fu,
      first_readmit_date = format(.data$v1_first_readmit_date, "%m/%d/%Y"),
      n_readmit_1yr    = .data$v1_n_readmit_1yr,
      v1_source_text   = paste0(
        "v1 reint: ", ifelse(.data$v1_reint_any, "Y", "N"),
        " (n=", .data$v1_n_reint, "; ",
        paste(format(.data$v1_reint1_date, "%m/%d/%Y"),
              format(.data$v1_reint2_date, "%m/%d/%Y"), sep = ", "), ")",
        " | v1 amp: ", ifelse(.data$v1_amp_any, "Y", "N"),
        " | v1 LTF: ", ifelse(.data$v1_ltf_flag, "Y", "N"),
        " | v1 obs to 2020-03-23: ", .data$v1_obs_days, "d",
        " | detail: ", dplyr::coalesce(.data$v1_details, ""),
        " | readmits: ", dplyr::coalesce(.data$v1_readmit_details, "")
      ),
      v1_reused = 1L
    )

  workbook_df %>%
    dplyr::rows_update(reusable, by = "study_id", unmatched = "ignore")
}
