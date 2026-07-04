# Tests for the prefilled abstraction workbook builder. Needs openxlsx; skips
# cleanly if absent.

source(file.path("..", "..", "dev", "make_abstraction_workbook.R"))

make_matched_min <- function(n = 5) {
  tibble::tibble(
    study_id       = sprintf("PID%04d", seq_len(n)),
    inmate         = factor(rep(c("Inmate", "Non-inmate"), length.out = n),
                            levels = c("Non-inmate", "Inmate")),
    presentation   = factor(rep(c("CLTI", "Claudication"), length.out = n)),
    current_smoker = rep(c(1L, 0L), length.out = n)
  )
}

make_crosswalk_min <- function(n = 5) {
  tibble::tibble(
    study_id        = sprintf("PID%04d", seq_len(n)),
    mrn             = sprintf("%07d", 1000000 + seq_len(n)),
    admit_date      = as.Date("2020-01-01") + seq_len(n),
    procedure_date  = as.Date("2020-01-02") + seq_len(n),
    discharge_date  = as.Date("2020-01-05") + seq_len(n),
    death_date      = as.Date(NA),
    ssdi_death_date = as.Date(NA)
  )
}

test_that("prefill block pulls identifiers and index dates", {
  pf <- build_prefill(make_matched_min(), make_crosswalk_min())
  expect_equal(nrow(pf), 5)
  expect_true(all(c("study_id", "mrn", "index_proc_date", "presentation")
                  %in% names(pf)))
  expect_true(all(grepl("^[0-9]{2}/[0-9]{2}/[0-9]{4}$", pf$index_proc_date)))
  expect_equal(pf$inmate, as.integer(make_matched_min()$inmate == "Inmate"))
})

test_that("workbook builds with the three sheets and the chart-based columns", {
  skip_if_not_installed("openxlsx")

  path <- file.path(tempdir(), paste0("wb_", as.integer(runif(1, 1, 1e8)), ".xlsx"))
  build_abstraction_workbook(make_matched_min(), make_crosswalk_min(), path)
  expect_true(file.exists(path))

  sheets <- openxlsx::getSheetNames(path)
  expect_true(all(c("Instructions", "Abstraction", "Codebook") %in% sheets))

  d <- openxlsx::read.xlsx(path, sheet = "Abstraction")
  # Prefilled identifiers are present and populated.
  expect_true(all(c("study_id", "mrn", "index_proc_date") %in% names(d)))
  expect_false(any(is.na(d$study_id)))
  # Chart-based fields exist and start blank.
  expect_true(all(c("vital_status", "major_amp", "days_to_first_fu", "notes")
                  %in% names(d)))
  expect_true(all(is.na(d$vital_status)))
})
