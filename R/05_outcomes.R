# ---------------------------------------------------------------------------
# 05_outcomes.R  (PLACEHOLDER, runs on synthetic data until abstraction is done)
#
# Takes the matched cohort from 04_match.R plus a de-identified outcomes file
# and produces the comparisons in the SAP:
#   - overall survival: Kaplan-Meier + weighted Cox (hazard ratio, CI)
#   - MALE with death as a competing risk: cumulative incidence + Fine-Gray sHR
#   - readmission, current smoking, medication adherence: risk differences / RR
#   - follow-up access: days to first visit, number of visits
#
# Everything is reported as an effect size with a 95% CI, using cluster-robust
# standard errors on the matched sets (subclass), consistent with the SAP's
# "estimates over p-values" framing.
#
# Pass adjust_vars to covariate-adjust the models for any residual imbalance the
# match leaves behind (a doubly-robust design). Those covariates must be present
# in the matched cohort; run_outcomes carries them through automatically.
#
# The outcomes file is de-identified: one row per study_id with day-count
# intervals and event flags, never calendar dates. Use
# build_outcomes_from_abstraction() to derive it from the abstraction workbook
# and the private crosswalk (that step runs locally and its inputs stay in the
# git-ignored private/ folder).
# ---------------------------------------------------------------------------

## ---- Expected de-identified outcomes schema (one row per study_id) --------
# study_id           character  matches the matched cohort
# os_time            numeric    days from index procedure to death or last contact
# os_event           0/1        1 = died
# male_time          numeric    days to MALE, competing death, or last contact
# male_status        0/1/2      0 = censored, 1 = MALE, 2 = death without MALE
# readmit_1yr        0/1        unplanned readmission within 1 year
# current_smoker_fu  0/1        actively smoking at follow-up
# statin_adherent    0/1        statin documented active at follow-up
# days_to_first_fu   numeric    days from discharge to first vascular visit
# n_fu_visits_1yr    integer    vascular visits within 1 year

# ---------------------------------------------------------------------------
# Derivation from the abstraction workbook (kept out of the analytic path so no
# calendar date is ever needed downstream). `abstraction` and `crosswalk` are
# data frames keyed by study_id; both live only in private/.
# ---------------------------------------------------------------------------
build_outcomes_from_abstraction <- function(abstraction, crosswalk = NULL) {
  d <- abstraction
  # Pull in only the crosswalk columns the abstraction doesn't already have,
  # so shared columns (e.g. index_proc_date) don't collide during the join.
  if (!is.null(crosswalk)) {
    extra <- setdiff(names(crosswalk), names(abstraction))
    if (length(extra)) {
      d <- dplyr::left_join(d, crosswalk[, c("study_id", extra)], by = "study_id")
    }
  }

  as_day <- function(a, b) as.numeric(as.Date(a) - as.Date(b))

  d %>%
    dplyr::mutate(
      t0          = as.Date(index_proc_date),
      death_dt    = as.Date(death_date),
      last_dt     = as.Date(last_alive_date),
      amp_dt      = as.Date(major_amp_date),
      reint_dt    = as.Date(major_reint_date),
      male_dt     = pmin(amp_dt, reint_dt, na.rm = TRUE),

      os_event    = as.integer(!is.na(death_dt)),
      os_time     = as_day(dplyr::coalesce(death_dt, last_dt), t0),

      male_status = dplyr::case_when(
        !is.na(male_dt) & (is.na(death_dt) | male_dt <= death_dt) ~ 1L,
        !is.na(death_dt)                                          ~ 2L,
        TRUE                                                     ~ 0L
      ),
      male_time = dplyr::case_when(
        male_status == 1L ~ as_day(male_dt, t0),
        male_status == 2L ~ as_day(death_dt, t0),
        TRUE              ~ as_day(last_dt, t0)
      ),

      current_smoker_fu = as.integer(smoke_fu == "current"),
      statin_adherent   = as.integer(statin_active_fu == "yes"),
      days_to_first_fu  = as_day(first_fu_date, as.Date(index_dc_date))
    ) %>%
    dplyr::transmute(
      study_id, os_time, os_event, male_time, male_status,
      readmit_1yr = as.integer(readmit_1yr),
      current_smoker_fu, statin_adherent,
      days_to_first_fu, n_fu_visits_1yr = as.integer(n_fu_visits_1yr)
    )
}

# ---------------------------------------------------------------------------
# Small helper: a cluster-robust effect of inmate status on a binary,
# continuous, or count outcome, returned as a one-row tidy data frame.
#   family = "gaussian"     -> risk / mean difference (identity link)
#   family = "quasipoisson" -> risk ratio or rate ratio (log link, exponentiated)
# ---------------------------------------------------------------------------
# Only keep adjustment variables that are present and not constant.
usable_adjust <- function(d, adjust) {
  adj <- intersect(adjust, names(d))
  adj[vapply(adj, function(v) {
    length(unique(d[[v]][!is.na(d[[v]])])) > 1
  }, logical(1))]
}

robust_effect <- function(d, outcome, label, effect,
                          family = "gaussian", exponentiate = FALSE,
                          adjust = character(0)) {
  d$.y   <- d[[outcome]]
  d$.grp <- as.integer(d$inmate == "Inmate")
  adj <- usable_adjust(d, adjust)
  fam <- switch(family,
                gaussian     = stats::gaussian(),
                quasipoisson = stats::quasipoisson(link = "log"))
  rhs <- paste(c(".grp", adj), collapse = " + ")
  fit <- stats::glm(stats::as.formula(paste(".y ~", rhs)),
                    data = d, weights = d$weights, family = fam)
  V   <- sandwich::vcovCL(fit, cluster = d$subclass)
  ct  <- lmtest::coeftest(fit, vcov. = V)
  est <- ct[".grp", "Estimate"]; se <- ct[".grp", "Std. Error"]
  lo  <- est - 1.96 * se;        hi <- est + 1.96 * se
  if (exponentiate) { est <- exp(est); lo <- exp(lo); hi <- exp(hi) }
  tibble::tibble(outcome = label, effect = effect,
                 estimate = est, conf.low = lo, conf.high = hi)
}

# ---------------------------------------------------------------------------
# Main entry point.
# ---------------------------------------------------------------------------
run_outcomes <- function(matched, outcomes, out_dir = here::here("output"),
                         adjust_vars = character(0)) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Carry the adjustment covariates through from the matched cohort so the
  # outcome models can covariate-adjust for residual imbalance (doubly robust).
  keep <- c("study_id", "inmate", "weights", "subclass", adjust_vars)
  d <- matched[, intersect(keep, names(matched))] %>%
    dplyr::inner_join(outcomes, by = "study_id")
  adj <- usable_adjust(d, adjust_vars)

  results <- list()

  ## ---- 1. Overall survival ------------------------------------------------
  km <- survival::survfit(
    survival::Surv(os_time, os_event) ~ inmate, data = d, weights = weights
  )
  s1 <- summary(km, times = 365.25, extend = TRUE)
  surv_1yr <- tibble::tibble(
    group          = sub("inmate=", "", s1$strata),
    surv_1yr       = s1$surv,
    surv_1yr_lower = s1$lower,
    surv_1yr_upper = s1$upper
  )
  readr::write_csv(surv_1yr, file.path(out_dir, "survival_1yr.csv"))

  cox_rhs <- paste(c("inmate", adj), collapse = " + ")
  cox <- survival::coxph(
    stats::as.formula(paste0("survival::Surv(os_time, os_event) ~ ", cox_rhs)),
    data = d, weights = weights, cluster = subclass, robust = TRUE
  )
  ci  <- summary(cox)$conf.int
  irow <- grep("^inmate", rownames(ci))[1]
  results[["survival"]] <- tibble::tibble(
    outcome = "Overall survival (1 yr and full follow-up)", effect = "Hazard ratio",
    estimate = ci[irow, "exp(coef)"],
    conf.low = ci[irow, "lower .95"], conf.high = ci[irow, "upper .95"]
  )
  save_km_plot(km, file.path(out_dir, "km_survival.png"))

  ## ---- 2. MALE with death as a competing risk -----------------------------
  d$male_fac <- factor(d$male_status, levels = c(0, 1, 2),
                       labels = c("censor", "MALE", "death"))

  # 1-year cumulative incidence of MALE per group (computed group by group so
  # the state columns and strata never have to be lined up by position).
  cif_1yr <- do.call(rbind, lapply(levels(d$inmate), function(g) {
    dg  <- d[d$inmate == g, , drop = FALSE]
    fit <- survival::survfit(survival::Surv(male_time, male_fac) ~ 1,
                             data = dg, weights = dg$weights)
    s   <- summary(fit, times = 365.25, extend = TRUE)
    mc  <- which(colnames(s$pstate) == "MALE")
    tibble::tibble(group = g, male_cif_1yr = as.numeric(s$pstate[, mc]))
  }))
  readr::write_csv(cif_1yr, file.path(out_dir, "male_cif_1yr.csv"))

  d_fg <- d[, c("male_time", "male_fac", "inmate", "subclass", "weights", adj)]
  fg <- survival::finegray(
    survival::Surv(male_time, male_fac) ~ ., data = d_fg, etype = "MALE"
  )
  fg_rhs <- paste(c("inmate", adj), collapse = " + ")
  fgcox <- survival::coxph(
    stats::as.formula(paste0("survival::Surv(fgstart, fgstop, fgstatus) ~ ", fg_rhs)),
    data = fg, weights = fgwt * weights, cluster = subclass, robust = TRUE
  )
  fci  <- summary(fgcox)$conf.int
  frow <- grep("^inmate", rownames(fci))[1]
  results[["male"]] <- tibble::tibble(
    outcome = "MALE at 1 year (competing risk)", effect = "Subdistribution HR",
    estimate = fci[frow, "exp(coef)"],
    conf.low = fci[frow, "lower .95"], conf.high = fci[frow, "upper .95"]
  )

  ## ---- 3. Binary outcomes: readmission, smoking, adherence ----------------
  results[["readmit"]] <- robust_effect(
    d, "readmit_1yr", "Readmission within 1 year", "Risk difference", adjust = adj)
  results[["smoke"]] <- robust_effect(
    d, "current_smoker_fu", "Current smoking at follow-up", "Risk difference", adjust = adj)
  results[["statin"]] <- robust_effect(
    d, "statin_adherent", "Statin adherence at follow-up", "Risk difference", adjust = adj)

  ## ---- 4. Follow-up access ------------------------------------------------
  results[["days_fu"]] <- robust_effect(
    d, "days_to_first_fu", "Days to first follow-up", "Mean difference (days)", adjust = adj)
  results[["n_fu"]] <- robust_effect(
    d, "n_fu_visits_1yr", "Follow-up visits within 1 year", "Rate ratio",
    family = "quasipoisson", exponentiate = TRUE, adjust = adj)

  ## ---- Assemble and write -------------------------------------------------
  results_tbl <- dplyr::bind_rows(results) %>%
    dplyr::mutate(dplyr::across(c(estimate, conf.low, conf.high), ~ round(., 3)))
  readr::write_csv(results_tbl, file.path(out_dir, "outcome_results.csv"))

  message("05_outcomes.R: analyzed ", nrow(d), " matched patients across ",
          nrow(results_tbl), " outcomes. Results in outcome_results.csv.")

  list(results = results_tbl, survival_1yr = surv_1yr, cif_1yr = cif_1yr,
       km = km, cox = cox)
}

# ---------------------------------------------------------------------------
# Minimal Kaplan-Meier plot (avoids a survminer dependency).
# ---------------------------------------------------------------------------
save_km_plot <- function(km, path) {
  strata <- rep(names(km$strata), km$strata)
  df <- tibble::tibble(
    time  = km$time,
    surv  = km$surv,
    group = sub("inmate=", "", strata)
  )
  p <- ggplot2::ggplot(df, ggplot2::aes(time, surv, colour = group)) +
    ggplot2::geom_step() +
    ggplot2::coord_cartesian(ylim = c(0, 1)) +
    ggplot2::labs(x = "Days from index procedure", y = "Survival probability",
                  colour = NULL, title = "Overall survival by group") +
    ggplot2::theme_minimal()
  ggplot2::ggsave(path, p, width = 7, height = 5, dpi = 200)
}
