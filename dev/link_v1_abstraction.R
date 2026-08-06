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

# Tolerant date coercion for the 2020 workbook.
#
# That file was maintained by hand and five cells in the follow-up columns hold
# the literal text "None" rather than being blank. readxl therefore types the
# whole column as character, and as.Date() then fails outright with "character
# string is not in a standard unambiguous format". Coerce tolerantly instead:
# real dates pass through, recognised placeholders become NA, and anything else
# becomes NA with a warning naming the offending value so a genuine typo is not
# silently discarded.
#
# Day-first formats are deliberately NOT attempted. These are US clinical
# records, so a d/m/Y guess would misparse rather than fail, which is worse.
.v1_na_strings <- c("none", "na", "n/a", "nan", "-", "--", "", "unk", "unknown",
                    "null", "?")

as_v1_date <- function(x, field = "date") {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x))         return(as.Date(x, origin = "1899-12-30"))

  chr   <- trimws(as.character(x))
  blank <- is.na(chr) | tolower(chr) %in% .v1_na_strings
  out   <- rep(as.Date(NA), length(chr))

  # Excel occasionally hands back a serial number as text.
  serial <- !blank & grepl("^[0-9]{5}(\\.[0-9]+)?$", chr)
  if (any(serial)) {
    out[serial] <- as.Date(as.numeric(chr[serial]), origin = "1899-12-30")
  }

  for (f in c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y", "%Y/%m/%d")) {
    todo <- !blank & is.na(out)
    if (!any(todo)) break
    out[todo] <- suppressWarnings(as.Date(chr[todo], format = f))
  }

  bad <- unique(chr[!blank & is.na(out)])
  if (length(bad)) {
    warning("read_v1(): could not parse ", length(bad), " distinct value(s) in ",
            field, ": ", paste(bad, collapse = ", "),
            ". Treated as missing.", call. = FALSE)
  }
  out
}

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
    # One header ("1st\u00a0reinterv") contains a non-breaking space. Write it as
    # an escape rather than a literal byte so the file stays pure ASCII and
    # parses the same under any locale.
    dplyr::rename_with(~ gsub("\u00a0", " ", .x, fixed = TRUE)) %>%
    dplyr::transmute(
      v1_inmate        = .data$Inmate == "Y",
      mrn              = norm_mrn(.data$MRN),
      v1_proc_date     = as_v1_date(.data$`Procedure date`, "Procedure date"),
      v1_admit_date    = as_v1_date(.data$`Admit date`, "Admit date"),
      v1_dc_date       = as_v1_date(.data$`d/c date`, "d/c date"),
      v1_reint_any     = .data$Reintervention == "Y",
      v1_n_reint       = as.integer(.data$`# of reinterventions`),
      v1_reint1_date   = as_v1_date(.data$`1st reinterv`, "1st reinterv"),
      v1_reint2_date   = as_v1_date(.data$`2nd reinterv`, "2nd reinterv"),
      v1_first_fu_date = as_v1_date(.data$`1st f/u`, "1st f/u"),
      v1_last_fu_date  = as_v1_date(.data$`Last f/u date`, "Last f/u date"),
      v1_ltf_flag      = .data$`Loss to f/u?` == "Y",
      v1_amp_any       = .data$Amputation == "Y",
      v1_amp_date      = as_v1_date(.data$`Amputation date`, "Amputation date"),
      v1_death_date    = as_v1_date(.data$`Death date`, "Death date"),
      v1_details       = .data$Details
    )

  readmit_raw <- readxl::read_excel(f_readmit, sheet = "Readmission", skip = 1)
  rcols <- c("#1", "#2", "#3", "#4", "#5", "#6", "#7")
  readmit <- readmit_raw %>%
    dplyr::transmute(
      mrn                = norm_mrn(.data$MRN),
      v1_proc_date       = as_v1_date(.data$`Procedure date`, "Procedure date"),
      v1_readmit_dates   = apply(
        dplyr::across(dplyr::all_of(rcols), ~ as_v1_date(.x, "readmission date")), 1L,
        function(x) list(sort(as.Date(x[!is.na(x)], origin = "1970-01-01")))
      ) %>% unlist(recursive = FALSE),
      v1_readmit_details = .data$Details
    )

  matched <- readxl::read_excel(f_matched) %>%
    dplyr::transmute(
      mrn            = norm_mrn(.data$mrn),
      v1_inmate      = .data$inmate == 1,
      v1_proc_date   = as_v1_date(.data$procedure_date, "procedure_date")
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
# Free-text digest of a v1 record. Shared by both carry-over paths below.
.v1_digest <- function(d) {
  fmt <- function(x) ifelse(is.na(x), "none", format(x, "%m/%d/%Y"))
  vital <- ifelse(
    !is.na(d$v1_death_date),
    paste0("dead ", fmt(d$v1_death_date)),
    ifelse(is.na(d$v1_last_fu_date),
           "NOT ESTABLISHED - no death record and no follow-up contact in 2020",
           paste0("alive as of ", fmt(d$v1_last_fu_date))))
  paste0(
    "v1 vital status: ", vital,
    " | v1 reint: ", ifelse(d$v1_reint_any, "Y", "N"),
    " (n=", d$v1_n_reint, "; ", fmt(d$v1_reint1_date), ", ",
    fmt(d$v1_reint2_date), ")",
    " | v1 amp: ", ifelse(d$v1_amp_any, "Y", "N"),
    " on ", fmt(d$v1_amp_date),
    " (limb not recorded in 2020)",
    " | v1 readmissions in 1y: ", d$v1_n_readmit_1yr,
    ", first ", fmt(d$v1_first_readmit_date),
    " (ALL-CAUSE incl. planned)",
    " | v1 LTF: ", ifelse(d$v1_ltf_flag, "Y", "N"),
    " | v1 obs to 2020-03-23: ", d$v1_obs_days, "d",
    " | detail: ", dplyr::coalesce(d$v1_details, ""),
    " | readmits: ", dplyr::coalesce(d$v1_readmit_details, "")
  )
}

prefill_from_v1 <- function(link, workbook_df) {

  # ---- Same index procedure: values are directly comparable ----------------
  #
  # Only fields that are BOTH definitionally unchanged AND not gated behind a
  # parent we cannot assert are written into answer cells. Three fields were
  # dropped from this list after the first REDCap import:
  #
  #   major_amp_date      shows only if major_amp = 1, and 2020 did not record
  #                       which limb was amputated, so index-limb major
  #                       amputation cannot be asserted from it.
  #   first_readmit_date  both show only if readmit_1yr = 1, which is defined
  #   n_readmit_1yr       here as an UNPLANNED readmission. The 2020 count is
  #                       all-cause and includes planned admissions.
  #
  # Writing a child field without its parent also made REDCap prompt "erase the
  # values of fields to be hidden?" every time an abstractor opened one of those
  # records - one stray click on Erase and the carry-over was gone. Their values
  # now travel in v1_source_text instead, where they inform without asserting.
  same <- dplyr::filter(link, .data$v1_status == "reusable_same_index")

  # "Alive" is only asserted where the 2020 record actually documents contact.
  # A patient with no death record AND no follow-up visit is not evidence of
  # survival, just absence of evidence - and survival is a primary outcome here,
  # so guessing it into an answer cell is the wrong default. Those records get
  # a blank vital_status and the reason in v1_source_text. Leaving it blank also
  # keeps death_date and last_alive_date correctly hidden, since both branch on
  # vital_status, and both are empty for these patients anyway.
  vital <- ifelse(
    is.na(same$v1_death_date) & is.na(same$v1_last_fu_date), NA_character_,
    ifelse(is.na(same$v1_death_date), "alive", "dead"))

  reusable <- tibble::tibble(
    study_id           = same$study_id,
    vital_status       = vital,
    death_date         = format(same$v1_death_date, "%m/%d/%Y"),
    last_alive_date    = format(same$v1_last_fu_date, "%m/%d/%Y"),
    first_fu_date      = format(same$v1_first_fu_date, "%m/%d/%Y"),
    days_to_first_fu   = as.character(same$v1_days_to_first_fu),
    v1_source_text     = .v1_digest(same)
  )

  # v1_reused means "an answer cell was prefilled", so derive it rather than
  # asserting it: a record whose 2020 row was entirely empty gets the reference
  # note but no carry-over flag.
  answer_cols <- c("vital_status", "death_date", "last_alive_date",
                   "first_fu_date", "days_to_first_fu")
  reusable$v1_reused <- as.integer(
    rowSums(!is.na(reusable[, answer_cols, drop = FALSE])) > 0)

  # ---- Same patient, DIFFERENT index procedure -----------------------------
  # Time zero differs, so nothing can be copied: every interval in the 2020
  # record is measured from the wrong day. The notes are still useful context
  # for the abstractor, who would otherwise rediscover the same history from
  # the chart. Surface them with a loud label and leave v1_reused at 0 so the
  # audit trail does not claim a carry-over that did not happen.
  oth <- dplyr::filter(link, .data$v1_status == "same_patient_other_index")
  other <- tibble::tibble(
    study_id       = oth$study_id,
    v1_source_text = paste0(
      "*** DIFFERENT INDEX - CONTEXT ONLY, DO NOT COPY VALUES *** ",
      "The 2020 review used ", format(oth$v1_proc_date, "%m/%d/%Y"),
      ", ", oth$date_gap, " days from this index (",
      format(oth$cur_proc_date, "%m/%d/%Y"),
      "), so its intervals are measured from the wrong day. ",
      .v1_digest(oth)
    ),
    v1_reused = 0L
  )

  workbook_df %>%
    dplyr::rows_update(reusable, by = "study_id", unmatched = "ignore") %>%
    dplyr::rows_update(other,    by = "study_id", unmatched = "ignore")
}
