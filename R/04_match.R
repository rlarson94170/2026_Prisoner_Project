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
# table and Table 1 for description, but they do not enter the matching model
# and do not count toward the balance target.
#
# Method: optimal full matching (uses every control, weighted into subclasses),
# which balances this many covariates better than fixed-ratio nearest neighbor
# in a small treated group.
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

run_matching <- function(analytic, out_dir = here::here("output")) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Treatment must be 0/1 for MatchIt.
  dat <- analytic %>%
    dplyr::mutate(treat = as.integer(inmate == "Inmate"))

  fml <- reformulate(match_variables, response = "treat")

  m.out <- MatchIt::matchit(
    fml,
    data     = dat,
    method   = "full",   # optimal full matching (needs the optmatch package)
    distance = "glm",    # logistic propensity score
    estimand = "ATT"
  )

  matched <- MatchIt::match.data(m.out)  # adds 'weights' and 'subclass'

  # ---- Balance on the matching covariates (drives pass/fail) ----------------
  bal_match <- cobalt::bal.tab(
    m.out, un = TRUE, binary = "std", continuous = "std"
  )
  max_smd_matched <- max(abs(bal_match$Balance$Diff.Adj), na.rm = TRUE)

  # ---- Full balance table, including the reported-only variables ------------
  bal_full <- cobalt::bal.tab(
    m.out,
    addl       = dat[, report_variables, drop = FALSE],
    un         = TRUE,
    binary     = "std",
    continuous = "std"
  )
  capture.output(print(bal_full), file = file.path(out_dir, "balance_table.txt"))

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

  # ---- Weighted Table 1 (full matching implies weights) --------------------
  all_vars    <- c(match_variables, report_variables)
  factor_vars <- all_vars[
    vapply(matched[all_vars], function(x) is.factor(x) || is.character(x), logical(1))
  ]
  if (requireNamespace("survey", quietly = TRUE)) {
    dsn <- survey::svydesign(ids = ~1, weights = ~weights, data = matched)
    t1  <- tableone::svyCreateTableOne(
      vars = all_vars, strata = "inmate", factorVars = factor_vars,
      data = dsn, test = FALSE
    )
  } else {
    warning("survey not installed; Table 1 is unweighted.", call. = FALSE)
    t1 <- tableone::CreateTableOne(
      vars = all_vars, strata = "inmate", factorVars = factor_vars,
      data = matched, test = FALSE
    )
  }
  t1_out <- print(t1, smd = TRUE, printToggle = FALSE, noSpaces = TRUE)
  utils::write.csv(t1_out, file.path(out_dir, "table1_matched.csv"))

  saveRDS(matched, file.path(out_dir, "matched_cohort.rds"))

  message("04_match.R: full matching retained ",
          sum(matched$inmate == "Inmate"), " inmates and ",
          sum(matched$inmate == "Non-inmate"), " controls. ",
          "Max |SMD| on matching covariates: ", round(max_smd_matched, 3), ".")

  list(model = m.out, matched = matched,
       balance = bal_full, balance_matched = bal_match,
       max_smd_matched = max_smd_matched)
}
