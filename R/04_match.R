# ---------------------------------------------------------------------------
# 04_match.R
# Propensity score matching and balance assessment.
#
# Matching variables (reduced set; sex is dropped because all patients are men):
#   age, current smoking, diabetes, dialysis, limb severity, coronary disease,
#   symptomatic CHF, urgency.
# Residual balance is then checked on the covariates NOT in the matching model
# (race, BMI, COPD, hypertension, statin, ACE/ARB, antiplatelet, anticoagulant,
# prior ipsilateral revascularization, prior amputation, ambulation, insulin).
# ---------------------------------------------------------------------------

match_variables <- c(
  "age", "current_smoker", "diabetes_any", "dialysis",
  "limb_severity", "coronary_disease", "chf_symptomatic", "urgency"
)

balance_only_variables <- c(
  "bmi", "race", "ambulation", "copd_treated", "hypertension",
  "statin", "ace_arb", "antiplatelet", "anticoagulant",
  "prior_ipsi_revasc", "prior_amputation", "diabetes_insulin"
)

run_matching <- function(analytic,
                         ratio   = 3,
                         caliper = 0.2,
                         out_dir = here::here("output")) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Treatment must be 0/1 for MatchIt.
  dat <- analytic %>%
    dplyr::mutate(treat = as.integer(inmate == "Inmate"))

  fml <- reformulate(match_variables, response = "treat")

  m.out <- MatchIt::matchit(
    fml,
    data     = dat,
    method   = "nearest",
    distance = "glm",          # logistic propensity score
    link     = "linear.logit", # match on the logit of the score
    caliper  = caliper,        # in SD units of the linear propensity score
    ratio    = ratio,
    replace  = FALSE
  )

  matched <- MatchIt::match.data(m.out)

  # ---- Balance on matching variables AND the residual-check variables ------
  bal <- cobalt::bal.tab(
    m.out,
    addl       = dat[, balance_only_variables, drop = FALSE],
    un         = TRUE,
    stats      = c("mean.diffs"),
    binary     = "std",
    continuous = "std"
  )
  capture.output(print(bal), file = file.path(out_dir, "balance_table.txt"))

  love <- cobalt::love.plot(
    m.out,
    addl        = dat[, balance_only_variables, drop = FALSE],
    stats       = "mean.diffs",
    thresholds  = c(m = 0.1),
    binary      = "std",
    continuous  = "std",
    var.order   = "unadjusted",
    abs         = TRUE,
    title       = "Covariate balance before and after matching"
  )
  ggplot2::ggsave(file.path(out_dir, "love_plot.png"),
                  love, width = 8, height = 6, dpi = 200)

  # ---- Table 1 (matched) with standardized mean differences ----------------
  all_vars <- c(match_variables, balance_only_variables)
  factor_vars <- all_vars[
    vapply(matched[all_vars], function(x) is.factor(x) || is.character(x), logical(1))
  ]
  t1 <- tableone::CreateTableOne(
    vars       = all_vars,
    strata     = "inmate",
    factorVars = factor_vars,
    data       = matched,
    test       = FALSE
  )
  t1_out <- print(t1, smd = TRUE, printToggle = FALSE, noSpaces = TRUE)
  utils::write.csv(t1_out, file.path(out_dir, "table1_matched.csv"))

  # De-identified matched cohort (git-ignored output folder).
  saveRDS(matched, file.path(out_dir, "matched_cohort.rds"))

  message("04_match.R: matched ", sum(matched$inmate == "Inmate"), " inmates to ",
          sum(matched$inmate == "Non-inmate"), " controls. ",
          "Max |SMD| after matching: ",
          round(max(abs(bal$Balance$Diff.Adj), na.rm = TRUE), 3), ".")

  list(model = m.out, matched = matched, balance = bal)
}
