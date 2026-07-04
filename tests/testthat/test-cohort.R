# Tests for the cohort rules and de-identification in 03_cohort.R.

# A synthetic registry covering every cohort rule and edge case.
build_test_cohort <- function() {
  raw <- make_raw(
    # Inmate, male, two inmate admissions -> index is the earlier one.
    make_proc("INM1", "2018-01-10", inmate = "1", sev_r = "5"),
    make_proc("INM1", "2019-01-10", inmate = "1", sev_r = "5"),
    # Plain non-inmate male control.
    make_proc("CTL1", "2019-06-01", inmate = "0", sev_r = "10"),
    # Crossover: a non-inmate admission and a later inmate admission.
    make_proc("CROSS", "2017-01-05", inmate = "0", sev_r = "10"),
    make_proc("CROSS", "2018-03-05", inmate = "1", sev_r = "4"),
    # Female -> excluded entirely.
    make_proc("FEM", "2019-01-01", inmate = "1", sex = "2", sev_r = "5"),
    # Non-inmate male whose only admission is hybrid -> excluded.
    make_proc("HYB", "2019-02-01", inmate = "0", hybrid = TRUE, sev_r = "10"),
    # Inmate, one admission, two staged procedures same admit date -> collapse.
    make_proc("STAGED", "2020-02-02", proc = "2020-02-02", inmate = "1", sev_r = "5"),
    make_proc("STAGED", "2020-02-02", proc = "2020-02-04", inmate = "1", sev_r = "5"),
    # Non-inmate male with no evaluable limb severity -> excluded.
    make_proc("NOSEV", "2019-03-01", inmate = "0", sev_r = "11", sev_l = "11"),
    # Non-inmate male: earliest admission hybrid (dropped), later clean one is index.
    make_proc("HYBMIX", "2016-05-01", inmate = "0", hybrid = TRUE, sev_r = "10"),
    make_proc("HYBMIX", "2017-08-01", inmate = "0", hybrid = FALSE, sev_r = "10")
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

test_that("females and hybrid-only patients are excluded", {
  co <- build_test_cohort()
  # 5 retained; FEM, HYB, NOSEV are gone. Confirm via the flow counts.
  expect_equal(co$flow$n, c(12L, 11L, 9L, 8L, 7L, 5L))
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
