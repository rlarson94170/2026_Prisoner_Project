# ---------------------------------------------------------------------------
# 01_import.R
# Read the raw registry export. Returns one row per procedure, with the
# original column names preserved (they are decoded in 02_recode.R).
# ---------------------------------------------------------------------------

import_raw <- function(path = RAW_DATA_PATH, sheet = RAW_DATA_SHEET) {
  if (!file.exists(path)) {
    stop("Raw data not found at RAW_DATA_PATH: ", path, call. = FALSE)
  }

  raw <- readxl::read_excel(
    path,
    sheet     = sheet,
    guess_max = 5000       # avoid mis-typing columns from early blanks
  )

  # Drop the many trailing all-empty spacer columns in the export.
  raw <- raw[, colSums(!is.na(raw)) > 0, drop = FALSE]

  message("01_import.R: read ", nrow(raw), " procedure rows, ",
          ncol(raw), " populated columns.")
  raw
}
