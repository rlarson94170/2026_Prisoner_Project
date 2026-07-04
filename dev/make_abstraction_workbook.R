# ---------------------------------------------------------------------------
# dev/make_abstraction_workbook.R
# Builds an Excel abstraction workbook preloaded with the matched patients, so
# abstractors only fill in the chart-based outcome fields.
#
# The known reference columns (study ID, MRN, index dates, presentation, and any
# registry death date) are prefilled and shaded. The chart-based fields are left
# blank with dropdown validation where the value is categorical.
#
# This file contains MRNs and dates, so it is written to the git-ignored
# private/ folder and must never be committed.
#
# As a script it reads output/matched_cohort.rds and private/id_crosswalk.rds
# and writes private/abstraction_workbook_prefilled.xlsx. The build function is
# also exercised by the tests with synthetic inputs.
# ---------------------------------------------------------------------------

# Categorical chart-based fields and their allowed values (used for dropdowns).
.abstraction_choices <- list(
  index_limb          = c("R", "L"),
  vital_status        = c("alive", "dead"),
  death_source        = c("registry", "SSDI", "chart"),
  cause_of_death      = c("related", "unrelated", "unsure", "UNK"),
  major_amp           = c("0", "1"),
  major_amp_level     = c("BKA", "AKA", "through-ankle"),
  minor_amp           = c("0", "1"),
  major_reint         = c("0", "1"),
  endo_reint          = c("0", "1"),
  readmit_1yr         = c("0", "1"),
  readmit_cause       = c("vascular", "wound", "cardiac", "other", "UNK"),
  statin_rx_dc        = c("0", "1"),
  statin_active_fu    = c("yes", "no", "UNK"),
  antiplt_rx_dc       = c("0", "1"),
  antiplt_active_fu   = c("yes", "no", "UNK"),
  noncompliance_noted = c("0", "1"),
  smoke_baseline      = c("current", "former", "never"),
  smoke_fu            = c("current", "former", "never", "UNK"),
  cessation_counsel   = c("0", "1"),
  ltf                 = c("0", "1"),
  ltf_reason          = c("released", "transferred", "moved", "deceased", "UNK"),
  contralat_proc      = c("0", "1"),
  fu_source           = c("in-system", "records-request", "UNK")
)

# Ordered chart-based (to-fill) columns, grouped as in the abstraction dictionary.
.tofill_cols <- c(
  "index_limb",
  "vital_status", "death_date", "death_source", "cause_of_death", "last_alive_date",
  "major_amp", "major_amp_date", "major_amp_level", "minor_amp", "minor_amp_date",
  "major_reint", "major_reint_date", "endo_reint", "endo_reint_date", "reint_type",
  "readmit_1yr", "first_readmit_date", "readmit_cause", "n_readmit_1yr",
  "statin_rx_dc", "statin_active_fu", "antiplt_rx_dc", "antiplt_active_fu",
  "noncompliance_noted",
  "smoke_baseline", "smoke_fu", "cessation_counsel",
  "first_fu_date", "days_to_first_fu", "n_fu_visits_1yr", "missed_visits_1yr",
  "ltf", "ltf_date", "ltf_reason",
  "contralat_proc", "contralat_proc_date", "fu_source",
  "abstractor", "abstract_date", "notes"
)

# Assemble the prefilled reference block from the matched cohort + crosswalk.
build_prefill <- function(matched, crosswalk) {
  ref <- dplyr::left_join(
    dplyr::select(matched, dplyr::any_of(c("study_id", "inmate", "presentation",
                                           "current_smoker"))),
    crosswalk, by = "study_id"
  )
  fmt <- function(d) ifelse(is.na(d), NA_character_, format(as.Date(d), "%m/%d/%Y"))
  tibble::tibble(
    study_id                = ref$study_id,
    mrn                     = as.character(ref$mrn),
    inmate                  = as.integer(ref$inmate == "Inmate"),
    index_admit_date        = fmt(ref$admit_date),
    index_proc_date         = fmt(ref$procedure_date),
    index_dc_date           = fmt(ref$discharge_date),
    presentation            = as.character(ref$presentation),
    registry_current_smoker = ifelse(ref$current_smoker == 1L, "current", "not current"),
    registry_death_date     = fmt(dplyr::coalesce(ref$death_date, ref$ssdi_death_date))
  )
}

build_abstraction_workbook <- function(matched, crosswalk, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Package 'openxlsx' is required. install.packages('openxlsx')", call. = FALSE)
  }
  prefill  <- build_prefill(matched, crosswalk)
  ref_cols <- names(prefill)

  # Full data frame: prefilled reference columns, then blank to-fill columns.
  dat <- prefill
  for (nm in .tofill_cols) dat[[nm]] <- NA
  n <- nrow(dat)

  wb <- openxlsx::createWorkbook()

  ## ---- Instructions -------------------------------------------------------
  openxlsx::addWorksheet(wb, "Instructions")
  instr <- c(
    "Outcome abstraction workbook (prefilled)",
    "",
    "The shaded columns on the left are prefilled from the registry: do not edit them.",
    "They tell you which chart to open (MRN) and the index admission dates.",
    "Fill in the white chart-based columns for each patient, one row per patient.",
    "",
    "Dates: MM/DD/YYYY. Leave a cell blank for not applicable. Enter UNK for unknown.",
    "Index-limb events are scored on the index limb only; contralateral procedures go in the contralateral columns.",
    "The 1-year window starts at index_proc_date. Record later events too.",
    "Categorical cells have dropdowns. When a chart is ambiguous, describe it in notes.",
    "",
    "This file contains MRNs and dates. Keep it off GitHub and store it securely.",
    "See the Codebook tab for every field's definition and allowed values."
  )
  openxlsx::writeData(wb, "Instructions", instr)
  openxlsx::setColWidths(wb, "Instructions", cols = 1, widths = 110)

  ## ---- Abstraction --------------------------------------------------------
  sheet <- "Abstraction"
  openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, sheet, dat, withFilter = FALSE)

  ref_hdr <- openxlsx::createStyle(fgFill = "#1F4E79", fontColour = "#FFFFFF",
                                   textDecoration = "bold", halign = "center",
                                   border = "TopBottomLeftRight", borderColour = "#BFBFBF")
  fill_hdr <- openxlsx::createStyle(fgFill = "#2E7D32", fontColour = "#FFFFFF",
                                    textDecoration = "bold", halign = "center",
                                    border = "TopBottomLeftRight", borderColour = "#BFBFBF")
  ref_body <- openxlsx::createStyle(fgFill = "#EDEDED")

  n_ref <- length(ref_cols)
  openxlsx::addStyle(wb, sheet, ref_hdr,  rows = 1, cols = seq_len(n_ref), gridExpand = TRUE)
  openxlsx::addStyle(wb, sheet, fill_hdr, rows = 1,
                     cols = (n_ref + 1):ncol(dat), gridExpand = TRUE)
  if (n > 0) {
    openxlsx::addStyle(wb, sheet, ref_body, rows = 2:(n + 1),
                       cols = seq_len(n_ref), gridExpand = TRUE, stack = TRUE)
  }
  openxlsx::freezePane(wb, sheet, firstActiveRow = 2, firstActiveCol = n_ref + 1)
  openxlsx::setColWidths(wb, sheet, cols = seq_len(ncol(dat)), widths = "auto")

  # Dropdowns for the categorical chart-based fields.
  rows_rng <- if (n > 0) 2:(n + 1) else 2:1000  # allow blank template too
  for (nm in names(.abstraction_choices)) {
    col_i <- match(nm, names(dat))
    if (!is.na(col_i)) {
      openxlsx::dataValidation(
        wb, sheet, col = col_i, rows = rows_rng, type = "list",
        value = sprintf('"%s"', paste(.abstraction_choices[[nm]], collapse = ",")),
        allowBlank = TRUE
      )
    }
  }

  ## ---- Codebook -----------------------------------------------------------
  openxlsx::addWorksheet(wb, "Codebook")
  codebook <- tibble::tibble(
    Column = c(ref_cols, .tofill_cols),
    Kind = c(rep("prefilled (do not edit)", length(ref_cols)),
             rep("chart-based (fill in)", length(.tofill_cols))),
    `Allowed values` = vapply(c(ref_cols, .tofill_cols), function(nm) {
      if (!is.null(.abstraction_choices[[nm]])) paste(.abstraction_choices[[nm]], collapse = ", ")
      else if (grepl("date", nm)) "MM/DD/YYYY"
      else ""
    }, character(1))
  )
  openxlsx::writeData(wb, "Codebook", codebook)
  openxlsx::setColWidths(wb, "Codebook", cols = 1:3, widths = c(24, 26, 40))

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# Convenience wrapper. Call this after sourcing the file:
#   source("dev/make_abstraction_workbook.R")
#   generate_abstraction_workbook()
# It reads the default project paths and writes the prefilled workbook.
# ---------------------------------------------------------------------------
generate_abstraction_workbook <- function(
    matched_path   = here::here("output",  "matched_cohort.rds"),
    crosswalk_path = here::here("private", "id_crosswalk.rds"),
    out_path       = here::here("private", "abstraction_workbook_prefilled.xlsx")) {

  if (!file.exists(matched_path)) {
    stop("Matched cohort not found at ", matched_path,
         ". Run run_all.R first.", call. = FALSE)
  }
  if (!file.exists(crosswalk_path)) {
    stop("Crosswalk not found at ", crosswalk_path,
         ". Run run_all.R first.", call. = FALSE)
  }
  path <- build_abstraction_workbook(
    readRDS(matched_path), readRDS(crosswalk_path), out_path
  )
  message("Wrote ", path, " (", "keep this file off GitHub).")
  invisible(path)
}

# Also runs when executed via `Rscript dev/make_abstraction_workbook.R`.
if (sys.nframe() == 0) generate_abstraction_workbook()
