# Tests for the VQI code decoding in 02_recode.R.

test_that("helpers map codes as expected", {
  expect_equal(as_code(c(" 1 ", "", "NA")), c("1", NA, NA))
  expect_equal(is_present(c("0", "1", NA)), c(FALSE, TRUE, FALSE))
  expect_equal(is_present(c("No", "0"), absent = c("0", "No")), c(FALSE, FALSE))
})

test_that("leg severity takes the worse of the two legs", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", sev_r = "9",  sev_l = "5"),  # claud vs tissue loss
    make_proc("B", "2019-01-01", sev_r = "0",  sev_l = "0"),  # asymptomatic
    make_proc("C", "2019-01-01", sev_r = "4",  sev_l = "10")  # rest pain vs severe claud
  )
  rc <- recode_registry(raw)
  expect_equal(as.character(rc$limb_severity), c("Tissue loss", "Asymptomatic", "Rest pain"))
  expect_equal(rc$presentation, c("CLTI", "Asymptomatic", "CLTI"))
})

test_that("comorbidity composites decode correctly", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", Diabetes = "3", CHF = "2", COPD = "3",
              Dialysis = "2", Hypertension = "3", Smoking = "2", CAD = "1"),
    make_proc("B", "2019-01-01", Diabetes = "0", CHF = "1", COPD = "1",
              Dialysis = "1", Hypertension = "0", Smoking = "1", CAD = "0")
  )
  rc <- recode_registry(raw)
  expect_equal(rc$diabetes_any,     c(1L, 0L))
  expect_equal(rc$diabetes_insulin, c(1L, 0L))
  expect_equal(rc$chf_symptomatic,  c(1L, 0L))   # code 1 = asymptomatic hx -> FALSE
  expect_equal(rc$copd_treated,     c(1L, 0L))   # code 1 = not treated -> FALSE
  expect_equal(rc$dialysis,         c(1L, 0L))   # only code 2 counts
  expect_equal(rc$hypertension,     c(1L, 0L))
  expect_equal(rc$current_smoker,   c(1L, 0L))   # only code 2 = current
})

test_that("coronary disease is a composite of CAD, prior CABG, prior PCI", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", CAD = "0", `Prior CABG` = "1", `Prior PCI` = "0"),
    make_proc("B", "2019-01-01", CAD = "0", `Prior CABG` = "0", `Prior PCI` = "2"),
    make_proc("C", "2019-01-01", CAD = "0", `Prior CABG` = "0", `Prior PCI` = "0")
  )
  rc <- recode_registry(raw)
  expect_equal(rc$coronary_disease, c(1L, 1L, 0L))
})

test_that("antiplatelet is aspirin OR a listed P2Y12/PAR1 agent, not medical-reason codes", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", `Pre-op ASA` = "0", `Pre-op Antiplatelet Drugs` = "1"),  # clopidogrel
    make_proc("B", "2019-01-01", `Pre-op ASA` = "1", `Pre-op Antiplatelet Drugs` = "0"),  # aspirin
    make_proc("C", "2019-01-01", `Pre-op ASA` = "0", `Pre-op Antiplatelet Drugs` = "6"),  # medical reason
    make_proc("D", "2019-01-01", `Pre-op ASA` = "0", `Pre-op Antiplatelet Drugs` = "0")   # none
  )
  rc <- recode_registry(raw)
  expect_equal(rc$antiplatelet, c(1L, 1L, 0L, 0L))
})

test_that("hybrid flag fires on any concomitant bypass field", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", hybrid = TRUE),
    make_proc("B", "2019-01-01", hybrid = FALSE)
  )
  rc <- recode_registry(raw)
  expect_equal(rc$hybrid, c(TRUE, FALSE))
})

test_that("BMI falls back to height and weight when BMI is missing", {
  raw <- make_raw(
    make_proc("A", "2019-01-01", BMI = NA, `Height Cm` = 200, `Weight Kg` = 80)
  )
  rc <- recode_registry(raw)
  expect_equal(rc$bmi, 20)  # 80 / (2.0^2)
})
