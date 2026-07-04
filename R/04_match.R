# ---------------------------------------------------------------------------
# 04_match.R
# Propensity score matching and balance assessment.
#
# Matching variables (confounders of the outcomes; sex is dropped because all
# patients are men, and limb severity enters as the collapsed CLTI indicator):
#   age, current smoking, diabetes, dialysis, CLTI, coronary disease,
#   symptomatic CHF, urgency, treated COPD, prior ipsilateral revascularization,
#   ambulation, race, BMI.
#
# Baseline medications are NOT matched on. Higher preoperative use of statins,
# ACE/ARB, antiplatelets, and insulin in inmates is part of the incarceration
# effect we're studying (supervised administration), so those are reported as
# baseline differences rather than balanced away. They appear in the balance
# table for description, but they do not enter the matching model and do not
# count toward the balance target.
#
# Method: optimal 2:1 matching. Each inmate is matched to two distinct controls,
# chosen to minimize the total propensity-score distance. The 2:1 ratio keeps the
# control set bounded (up to 76 distinct controls) so the chart abstraction is
# feasible. Balance and group means are produced with cobalt.
#
# With 38 inmates and 13 correlated covariates, a bounded match cannot drive
# every covariate below 0.10 the way full matching can. We accept the standard
# threshold (|SMD| < 0.25, with < 0.10 ideal) and covariate-adjust any residual
# imbalance in the outcome models (a doubly-robust matched design). The residual
# offenders are reported by validate.R and passed to run_outcomes() as
# adjustment variables.
# ---------------------------------------------------------------------------

match_variables <- c(
  "age", "current_smoker", "diabetes_any", "dialysis", "clti",
  "coronary_disease", "chf_symptomatic", "urgency", "copd_treated",
  "prior_ipsi_revasc", "ambulation", "race", "bmi"
)

# Reported for description only; never part of the matching model.
report_variables <- c(
  "statin", "ace_arb", "antiplatelet", "anticoagulant", "diabetes_insulin",
  "hypertension", "prior_amputation", "limb_severity"
)

run_matching <- function(analytic, ratio = 2, out_dir = here::here("output")) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Treatment must be 0/1 for MatchIt.
  dat <- analytic %>%
    dplyr::mutate(treat = as.integer(inmate == "Inmate"))

  fml <- reformulate(match_variables, response = "treat")

  m.out <- MatchIt::matchit(
    fml,
    data     = dat,
    method   = "optimal",  # optimal fixed-ratio matching (needs optmatch)
    distance = "glm",      # logistic propensity score
    ratio    = ratio,      # controls per inmate (2 -> up to 76 controls)
    estimand = "ATT"
  )

  matched <- MatchIt::match.data(m.out)  # adds 'weights' and 'subclass'

  # ---- Balance on the matching covariates (drives pass/fail) ----------------
  bal_match <- cobalt::bal.tab(
    m.out, un = TRUE, binary = "std", continuous = "std"
  )
  max_smd_matched <- max(abs(bal_match$Balance$Diff.Adj), na.rm = TRUE)

  # ---- Full balance table + weighted group means (serves as Table 1) --------
  # cobalt applies the matching weights; disp = "means" adds the weighted
  # group means alongside the standardized differences.
  bal_full <- cobalt::bal.tab(
    m.out,
    addl       = dat[, report_variables, drop = FALSE],
    un         = TRUE,
    disp       = c("means"),
    binary     = "std",
    continuous = "std"
  )
  capture.output(print(bal_full), file = file.path(out_dir, "balance_table.txt"))

  # Weighted Table 1: variable, weighted mean per group, and SMD.
  t1 <- bal_full$Balance
  t1$Variable <- rownames(t1)
  utils::write.csv(t1, file.path(out_dir, "table1_matched.csv"), row.names = FALSE)

  love <- cobalt::love.plot(
    m.out,
    addl       = dat[, report_variables, drop = FALSE],
    stats      = "mean.diffs",
    thresholds = c(m = 0.1),
    binary     = "std",
    continuous = "std",
    var.order  = "unadjusted",
    abs        = TRUE,
    title      = "Covariate balance before and after matching"
  )
  ggplot2::ggsave(file.path(out_dir, "love_plot.png"),
                  love, width = 8, height = 7, dpi = 200)

  saveRDS(matched, file.path(out_dir, "matched_cohort.rds"))

  message("04_match.R: optimal ", ratio, ":1 matching retained ",
          sum(matched$inmate == "Inmate"), " inmates and ",
          sum(matched$inmate == "Non-inmate"), " controls. ",
          "Max |SMD| on matching covariates: ", round(max_smd_matched, 3), ".")

  list(model = m.out, matched = matched,
       balance = bal_full, balance_matched = bal_match,
       max_smd_matched = max_smd_matched)
}
