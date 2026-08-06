# Tests for the cohort rules and de-identification in 03_cohort.R.

# A synthetic registry covering every cohort rule and edge case.
build_test_cohort <- function() {
  raw <- make_raw(
    # Inmate, male, two inmate admissions -> index is the earlier one.
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10"),
    make_proc("INM1", "2019-01-10", inmate = "1", sev_r = "10"),
    # Plain non-inmate male control.
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    # Crossover: a non-inmate admission and a later inmate admission.
    make_proc("CROSS", "2017-01-05", inmate = "0", sev_r = "10"),
    make_proc("CROSS", "2018-03-05", inmate = "1", sev_r = "10"),
    # Female -> excluded entirely.
    make_proc("FEM", "2019-01-01", inmate = "1", sex = "2", sev_r = "10"),
    # Non-inmate male whose only admission is hybrid -> excluded.
    make_proc("HYB", "2019-02-01", inmate = "0", hybrid = TRUE, sev_r = "10"),
    # Inmate, one admission, two staged procedures same admit date -> collapse.
    make_proc("STAGED", "2020-02-02", proc = "2020-02-02", inmate = "1", sev_r = "10"),
    make_proc("STAGED", "2020-02-02", proc = "2020-02-04", inmate = "1", sev_r = "10"),
    # Non-inmate male with no evaluable limb severity -> excluded.
    make_proc("NOSEV", "2019-03-01", inmate = "0", sev_r = "11", sev_l = "11"),
    # Non-inmate male: earliest admission hybrid (dropped), later clean one is index.
    make_proc("HYBMIX", "2016-05-01", inmate = "0", hybrid = TRUE, sev_r = "10"),
    make_proc("HYBMIX", "2017-08-01", inmate = "0", hybrid = FALSE, sev_r = "10"),
    # Implausible age (data-entry error) -> excluded by the adult floor.
    make_proc("BABY", "2017-01-16", inmate = "0", age = 0, sev_r = "10"),
    # Genuine minor -> also excluded.
    make_proc("TEEN", "2017-04-11", inmate = "0", age = 13, sev_r = "10")
  )
  proc <- recode_registry(raw)
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  cohort <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv)
  c(cohort, list(out_dir = tmp_out, private_dir = tmp_priv))
}

test_that("cohort size and group counts follow the rules", {
  co <- build_test_cohort()
  a <- co$analytic
  expect_equal(nrow(a), 5L)
  expect_equal(sum(a$inmate == "Inmate"), 3L)      # INM1, CROSS, STAGED
  expect_equal(sum(a$inmate == "Non-inmate"), 2L)  # CTL1, HYBMIX
})

test_that("females, hybrid-only, and under-age patients are excluded", {
  co <- build_test_cohort()
  # 5 retained; FEM, HYB, NOSEV, BABY, TEEN are gone. Confirm via flow counts.
  expect_equal(co$flow$n, c(14L, 13L, 11L, 9L, 8L, 7L, 5L, 5L))
  expect_match(co$flow$step[4], "age < 18")
  expect_match(co$flow$step[8], "common support")
  expect_false(any(co$analytic$age < 18))
})

test_that("excluded minors are listed for review", {
  co <- build_test_cohort()
  f <- file.path(co$private_dir, "excluded_under_age.csv")
  expect_true(file.exists(f))
  ex <- readr::read_csv(f, show_col_types = FALSE)
  expect_setequal(as.character(ex$mrn), c("BABY", "TEEN"))
})

test_that("crossover patients are classified as inmates with one index row", {
  co <- build_test_cohort()
  # Exactly 3 inmates, and every patient contributes a single row.
  expect_equal(sum(co$analytic$inmate == "Inmate"), 3L)
  expect_equal(nrow(co$analytic), length(unique(co$analytic$study_id)))
})

test_that("the analytic dataset carries no PHI columns", {
  co <- build_test_cohort()
  phi <- c("mrn", "MRN", "admit_date", "procedure_date", "discharge_date",
           "death_date", "ssdi_death_date")
  expect_false(any(phi %in% names(co$analytic)))
})

test_that("study IDs are well formed and unique", {
  co <- build_test_cohort()
  expect_true(all(grepl("^PID[0-9]{4}$", co$analytic$study_id)))
  expect_equal(anyDuplicated(co$analytic$study_id), 0L)
})

test_that("the crosswalk and flow file are written to the given folders", {
  co <- build_test_cohort()
  expect_true(file.exists(file.path(co$private_dir, "id_crosswalk.rds")))
  expect_true(file.exists(file.path(co$out_dir, "cohort_flow.csv")))
  cw <- readRDS(file.path(co$private_dir, "id_crosswalk.rds"))
  expect_true(all(c("study_id", "mrn") %in% names(cw)))
})

test_that("inmate is a factor with non-inmate as the reference level", {
  co <- build_test_cohort()
  expect_s3_class(co$analytic$inmate, "factor")
  expect_equal(levels(co$analytic$inmate), c("Non-inmate", "Inmate"))
})

# ---------------------------------------------------------------------------
# Patient updates (inmate flag and DOB-derived age)
# ---------------------------------------------------------------------------

# Build a cohort with a patient-updates file applied. `upd` is a data frame in
# the Patient Updates.xlsx column style.
build_with_updates <- function(upd) {
  raw <- make_raw(
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10"),
    make_proc("MISSED", "2016-01-12", inmate = "0", sev_r = "10"),
    make_proc("MISSED", "2019-05-01", inmate = "0", sev_r = "10"),
    make_proc("BADAGE", "2017-01-16", inmate = "0", age = 0, sev_r = "10"),
    make_proc("BADAGE", "2022-08-17", inmate = "0", age = 5, sev_r = "10"),
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    make_proc("CTL2", "2019-07-01", inmate = "0", sev_r = "10")
  )
  proc <- recode_registry(raw)
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  dir.create(tmp_priv, recursive = TRUE, showWarnings = FALSE)
  upath <- file.path(tmp_priv, "patient_updates.csv")
  readr::write_csv(upd, upath)
  cohort <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv,
                         updates_path = upath)
  c(cohort, list(out_dir = tmp_out, private_dir = tmp_priv))
}

test_that("an Inmate = yes update reclassifies the patient", {
  co <- build_with_updates(
    tibble::tibble(MRN = "MISSED", Inmate = "yes", Note = "missed by IT sweep")
  )
  expect_equal(sum(co$analytic$inmate == "Inmate"), 2L)   # INM1 + MISSED
  expect_true(all(co$updates$status == "matched 2 row(s)" |
                    co$updates$status == "matched 1 row(s)"))
})

test_that("yes/no, y/n, true/false and 1/0 are all accepted", {
  for (v in c("yes", "Y", "TRUE", "1")) {
    co <- build_with_updates(tibble::tibble(MRN = "MISSED", Inmate = v))
    expect_equal(sum(co$analytic$inmate == "Inmate"), 2L, info = v)
  }
  for (v in c("no", "N", "FALSE", "0")) {
    co <- build_with_updates(tibble::tibble(MRN = "CTL1", Inmate = v))
    expect_equal(sum(co$analytic$inmate == "Inmate"), 1L, info = v)
  }
})

test_that("a Surgery Date scopes an inmate update to one procedure", {
  co <- build_with_updates(
    tibble::tibble(MRN = "MISSED", `Surgery Date` = "2016-01-12", Inmate = "yes")
  )
  # The 2019 admission becomes a crossover non-inmate row and is dropped, so
  # MISSED contributes exactly one index admission, as an inmate.
  expect_equal(sum(co$analytic$inmate == "Inmate"), 2L)
  # BADAGE has no DOB here, so the adult floor removes it: CTL1 + CTL2 remain.
  expect_equal(sum(co$analytic$inmate == "Non-inmate"), 2L)
})

test_that("a DOB recomputes age for every procedure and rescues a bad age", {
  co <- build_with_updates(
    tibble::tibble(MRN = "BADAGE", DOB = "1944-11-12",
                   `Surgery Date` = "2017-01-16", Inmate = "no")
  )
  # Recorded ages were 0 and 5; true ages are 72 and 77. Without the DOB the
  # adult floor would discard this patient entirely.
  ages <- co$analytic$age[co$analytic$study_id %in% co$analytic$study_id]
  expect_true(72 %in% co$analytic$age)
  expect_false(any(co$analytic$age < 18))
  expect_match(co$updates$fields[1], "age \\(0/5 -> 72/77\\)")
  expect_false(file.exists(file.path(co$private_dir, "excluded_under_age.csv")))
})

test_that("without a DOB the bad-age patient is dropped by the adult floor", {
  co <- build_with_updates(tibble::tibble(MRN = "CTL1", Inmate = "no"))
  expect_false(any(co$analytic$age < 18))
  ex <- readr::read_csv(file.path(co$private_dir, "excluded_under_age.csv"),
                        show_col_types = FALSE)
  expect_equal(unique(as.character(ex$mrn)), "BADAGE")
})

test_that("an update for an MRN absent from the export warns and is logged", {
  expect_warning(
    co <- build_with_updates(
      tibble::tibble(MRN = c("MISSED", "GHOST"), Inmate = c("yes", "yes"))
    ),
    "absent from the"
  )
  expect_equal(co$updates$mrn[co$updates$status == "NOT FOUND in export"],
               "GHOST")
})

test_that("no updates file is a no-op", {
  raw <- make_raw(
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10"),
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10")
  )
  proc <- recode_registry(raw)
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  co <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv,
                     updates_path = file.path(tmp_priv, "nope.csv"))
  expect_equal(sum(co$analytic$inmate == "Inmate"), 1L)
  expect_equal(nrow(co$updates), 0L)
})

# ---------------------------------------------------------------------------
# Stable study IDs
# ---------------------------------------------------------------------------

test_that("an MRN keeps its study ID when the cohort grows", {
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  reg <- file.path(tmp_priv, "study_id_registry.csv")

  base_rows <- list(
    make_proc("AAA1", "2018-01-10", inmate = "0", sev_r = "10"),
    make_proc("CCC3", "2018-02-10", inmate = "1", sev_r = "10")
  )
  first <- build_cohort(recode_registry(do.call(make_raw, base_rows)),
                        out_dir = tmp_out, private_dir = tmp_priv,
                        id_registry_path = reg)
  cw1 <- readRDS(file.path(tmp_priv, "id_crosswalk.rds"))

  # Add a patient whose MRN sorts BEFORE an existing one. Under positional
  # numbering this would renumber everyone; with the registry it must not.
  second <- build_cohort(
    recode_registry(do.call(make_raw, c(
      base_rows, list(make_proc("BBB2", "2019-03-10", inmate = "0", sev_r = "10"))
    ))),
    out_dir = tmp_out, private_dir = tmp_priv, id_registry_path = reg
  )
  cw2 <- readRDS(file.path(tmp_priv, "id_crosswalk.rds"))

  keep <- cw1$mrn
  expect_equal(
    cw2$study_id[match(keep, cw2$mrn)],
    cw1$study_id[match(keep, cw1$mrn)]
  )
  expect_equal(nrow(cw2), nrow(cw1) + 1L)
  expect_equal(anyDuplicated(cw2$study_id), 0L)
})

test_that("study IDs do not encode exposure", {
  co <- build_test_cohort()
  cw <- readRDS(file.path(co$private_dir, "id_crosswalk.rds"))
  ids <- co$analytic$study_id[co$analytic$inmate == "Inmate"]
  # Inmates must not occupy a contiguous block at the end of the ID sequence.
  expect_false(all(ids == tail(sort(co$analytic$study_id), length(ids))))
})

test_that("leading zeros in MRNs do not create duplicate study IDs", {
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  reg <- file.path(tmp_priv, "study_id_registry.csv")
  ids1 <- assign_study_ids(c("04221952", "1001042"), reg)
  ids2 <- assign_study_ids(c("4221952", "01001042"), reg)
  expect_equal(ids1, ids2)
  expect_equal(anyDuplicated(ids1), 0L)
})

# ---------------------------------------------------------------------------
# Common support
# ---------------------------------------------------------------------------

test_that("controls in a covariate level with no inmates are trimmed", {
  raw <- make_raw(
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10"),
    make_proc("INM2", "2018-02-10", inmate = "1", sev_r = "10"),
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    make_proc("CTL2", "2019-07-01", inmate = "0", sev_r = "10"),
    # No inmate is on dialysis, so this control is off-support.
    make_proc("DIAL", "2019-08-01", inmate = "0", sev_r = "10", Dialysis = "2")
  )
  proc <- recode_registry(raw)
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  co <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv)

  expect_equal(nrow(co$analytic), 4L)
  expect_equal(sum(co$analytic$inmate == "Non-inmate"), 2L)
  expect_true("dialysis" %in% co$support$variable)
  expect_equal(sum(co$support$controls_dropped), 1L)
  expect_true(file.exists(file.path(tmp_out, "common_support.csv")))
  # dialysis is now constant, so it must be flagged for removal from the model.
  expect_true("dialysis" %in% co$degenerate_vars)
})

test_that("inmates are never dropped by the common-support step", {
  raw <- make_raw(
    # The only dialysis patient is an inmate: no control-only level exists, so
    # nothing is trimmed and the inmate stays.
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10", Dialysis = "2"),
    make_proc("INM2", "2018-02-10", inmate = "1", sev_r = "10"),
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    make_proc("CTL2", "2019-07-01", inmate = "0", sev_r = "10")
  )
  proc <- recode_registry(raw)
  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  co <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv)

  expect_equal(sum(co$analytic$inmate == "Inmate"), 2L)
  expect_equal(nrow(co$analytic), 4L)
  expect_equal(nrow(co$support), 0L)
})

test_that("a missing ambulation code is treated as missing, not a category", {
  raw <- make_raw(
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "10"),
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    make_proc("NOAMB", "2019-07-01", inmate = "0", sev_r = "10",
              Ambulation = NA_character_)
  )
  proc <- recode_registry(raw)
  expect_true(is.na(proc$ambulation[proc$mrn == "NOAMB"]))
  expect_false("Unknown" %in% proc$ambulation)

  tmp_out  <- file.path(tempdir(), paste0("out_",  as.integer(runif(1, 1, 1e8))))
  tmp_priv <- file.path(tempdir(), paste0("priv_", as.integer(runif(1, 1, 1e8))))
  co <- build_cohort(proc, out_dir = tmp_out, private_dir = tmp_priv)
  expect_equal(nrow(co$analytic), 2L)
})

test_that("the supported cohort leaves no control-only covariate level", {
  co <- build_test_cohort()
  a <- co$analytic
  for (v in intersect(SUPPORT_VARS, names(a))) {
    lv  <- as.character(a[[v]])
    inm <- unique(lv[a$inmate == "Inmate" & !is.na(lv)])
    ctl <- unique(lv[a$inmate == "Non-inmate" & !is.na(lv)])
    expect_length(setdiff(ctl, inm), 0)
  }
})
