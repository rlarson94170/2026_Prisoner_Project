# ---------------------------------------------------------------------------
# 02_recode.R
# Decode the VQI numeric codes into clinically meaningful analysis variables.
# Code meanings come from the registry data dictionary
# (Pathways_Registry_Abstraction_Data_Dictionary_Procedure_06302026.xlsx).
# Returns one row per procedure with tidy, analysis-ready columns.
# ---------------------------------------------------------------------------

# Map a leg-symptom code to an ordinal severity (0-3). Dictionary codes:
# 0 asymptomatic; 1/2/9/10 claudication; 4 rest pain;
# 3/5/6/7 tissue loss; 8 acute ischemia (treated as most severe);
# 11 not treated (retired) -> missing.
leg_severity_rank <- function(code) {
  code <- as_code(code)
  dplyr::case_when(
    code == "0"                       ~ 0L,   # asymptomatic
    code %in% c("1", "2", "9", "10")  ~ 1L,   # claudication
    code == "4"                       ~ 2L,   # ischemic rest pain
    code %in% c("3", "5", "6", "7", "8") ~ 3L, # tissue loss / acute
    TRUE                              ~ NA_integer_
  )
}

recode_registry <- function(raw) {

  # --- Hybrid flag: any concomitant open bypass field coded "1" ------------
  byp_cols <- names(raw)[
    grepl("concomitant", names(raw), ignore.case = TRUE) &
    grepl("bypass",      names(raw), ignore.case = TRUE)
  ]
  hybrid <- if (length(byp_cols) == 0) {
    rep(FALSE, nrow(raw))
  } else {
    raw %>%
      dplyr::select(dplyr::all_of(byp_cols)) %>%
      dplyr::mutate(dplyr::across(everything(), ~ as_code(.) == "1" & !is.na(as_code(.)))) %>%
      {rowSums(., na.rm = TRUE) > 0}
  }

  # Pull the columns we need by their exact export names.
  df <- raw %>%
    dplyr::transmute(
      # identifiers and dates (dropped before anything is written out)
      mrn            = as.character(.data[["MRN"]]),
      admit_date     = as.Date(.data[["Admit Date"]]),
      procedure_date = as.Date(.data[["Procedure Date"]]),
      discharge_date = as.Date(.data[["Discharge Date"]]),
      death_date     = suppressWarnings(as.Date(.data[["Date of Death"]])),
      ssdi_death_date= suppressWarnings(as.Date(.data[["SSDI Date of Death"]])),

      # exposure and eligibility
      inmate    = as.integer(as_code(.data[["Inmate"]]) == "1"),
      male      = as_code(.data[["Birth Sex"]]) == "1",
      hybrid    = hybrid,

      # continuous covariates
      age = suppressWarnings(as.numeric(.data[["Age at Procedure (years)"]])),
      bmi = {
        bmi_raw <- suppressWarnings(as.numeric(.data[["BMI"]]))
        h <- suppressWarnings(as.numeric(.data[["Height Cm"]])) / 100
        w <- suppressWarnings(as.numeric(.data[["Weight Kg"]]))
        dplyr::coalesce(bmi_raw, round(w / (h^2), 1))
      },

      # categorical / binary covariates
      race = dplyr::case_when(
        as_code(.data[["Race"]]) == "5" ~ "White",
        as_code(.data[["Race"]]) == "3" ~ "Black",
        TRUE                            ~ "Other/Unknown"
      ),
      urgency = dplyr::if_else(as_code(.data[["Urgency"]]) == "1",
                               "Elective", "Urgent/Emergent"),
      ambulation = dplyr::case_when(
        as_code(.data[["Ambulation"]]) == "1"            ~ "Ambulatory",
        as_code(.data[["Ambulation"]]) %in% c("2","5","6") ~ "Assisted",
        as_code(.data[["Ambulation"]]) %in% c("3","4")   ~ "Non-ambulatory",
        TRUE                                             ~ NA_character_
      ),
      coronary_disease = is_present(.data[["CAD"]]) |
                         is_present(.data[["Prior CABG"]]) |
                         is_present(.data[["Prior PCI"]]),
      chf_symptomatic  = as_code(.data[["CHF"]])  %in% c("2","3","4"),
      copd_treated     = as_code(.data[["COPD"]]) %in% c("2","3"),
      diabetes_any     = is_present(.data[["Diabetes"]]),
      diabetes_insulin = as_code(.data[["Diabetes"]]) == "3",
      dialysis         = as_code(.data[["Dialysis"]]) == "2",
      hypertension     = is_present(.data[["Hypertension"]]),
      current_smoker   = as_code(.data[["Smoking"]]) == "2",

      # pre-operative medications
      statin        = as_code(.data[["Pre-op Statin"]]) == "1",
      ace_arb       = as_code(.data[["Pre-op ACE-Inhibitor/ARB"]]) == "1",
      antiplatelet  = as_code(.data[["Pre-op ASA"]]) == "1" |
                      as_code(.data[["Pre-op Antiplatelet Drugs"]]) %in%
                        c("1","2","3","4","10","11","99"),
      anticoagulant = is_present(.data[["Pre Chronic Anticoagulant"]],
                                 absent = c("0","No","None")),

      # history
      prior_ipsi_revasc = as_code(.data[["Lg Art Byp,Endarterectomy,PVI"]]) == "1",
      prior_amputation  = as_code(.data[["Prior Amp (Leg, Foot, Toe)"]]) == "1",

      # limb severity from the worse of the two legs
      sev_right = leg_severity_rank(.data[["Leg Symptoms Right"]]),
      sev_left  = leg_severity_rank(.data[["Leg Symptoms Left"]])
    ) %>%
    dplyr::mutate(
      severity_rank = pmax(sev_right, sev_left, na.rm = TRUE),
      limb_severity = factor(
        dplyr::case_when(
          severity_rank == 0 ~ "Asymptomatic",
          severity_rank == 1 ~ "Claudication",
          severity_rank == 2 ~ "Rest pain",
          severity_rank == 3 ~ "Tissue loss",
          TRUE               ~ NA_character_
        ),
        levels = c("Asymptomatic", "Claudication", "Rest pain", "Tissue loss")
      ),
      presentation = dplyr::case_when(
        severity_rank >= 2 ~ "CLTI",
        severity_rank == 1 ~ "Claudication",
        severity_rank == 0 ~ "Asymptomatic",
        TRUE               ~ NA_character_
      )
    ) %>%
    dplyr::select(-sev_right, -sev_left)

  message("02_recode.R: recoded ", nrow(df), " procedures. ",
          "Hybrid procedures flagged: ", sum(df$hybrid, na.rm = TRUE), ".")
  df
}
