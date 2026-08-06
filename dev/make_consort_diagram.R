# ---------------------------------------------------------------------------
# dev/make_consort_diagram.R
# Regenerates the CONSORT-style cohort-flow figure from the numbers the pipeline
# produced: output/cohort_flow.csv (written by 03_cohort.R) and the matched
# cohort (output/matched_cohort.rds). Writes a self-contained SVG, no plotting
# packages required.
#
# The layout is derived from the flow table rather than hard-coded, so adding or
# removing a cohort rule changes the figure automatically. An earlier version
# assumed exactly six steps and silently mislabelled the diagram when the age
# floor and common-support steps were added.
#
# After sourcing:
#   source("dev/make_consort_diagram.R")
#   generate_consort_diagram()
# ---------------------------------------------------------------------------

# Presentation labels for the boxes, keyed on the step name in cohort_flow.csv.
# Anything not listed falls back to the raw step text, so a new rule still
# renders (just less prettily) instead of breaking the figure.
.consort_box_labels <- c(
  "Raw procedures"                                             = "PVI procedures, 2016-2024",
  "Male procedures"                                            = "Male procedures",
  "After excluding hybrids"                                    = "Purely endovascular",
  "After excluding age < 18"                                   = "Adults (age 18+)",
  "After dropping crossover non-inmate admissions"             = "After crossover handling",
  "Distinct admissions (first procedure each)"                 = "Distinct index admissions",
  "Index admissions (one per patient, covariates present)"     = "Index patients, complete covariates",
  "After restricting controls to the region of common support" = "Region of common support"
)

# Reason for the drop INTO each step, keyed the same way.
.consort_exclusion_labels <- c(
  "Male procedures"                                            = "Female",
  "After excluding hybrids"                                    = "Hybrid, endovascular + open bypass",
  "After excluding age < 18"                                   = "Under 18 at procedure",
  "After dropping crossover non-inmate admissions"             = "Non-inmate admissions of crossover patients",
  "Distinct admissions (first procedure each)"                 = "Staged same-admission procedures",
  "Index admissions (one per patient, covariates present)"     = "Non-index admissions & incomplete matching covariates",
  "After restricting controls to the region of common support" = "Controls in covariate strata containing no inmates"
)

# Units for each box, so the figure doesn't imply patients where it means rows.
.consort_units <- c(
  "Raw procedures"                                             = "procedures",
  "Male procedures"                                            = "procedures",
  "After excluding hybrids"                                    = "procedures",
  "After excluding age < 18"                                   = "procedures",
  "After dropping crossover non-inmate admissions"             = "procedures",
  "Distinct admissions (first procedure each)"                 = "admissions",
  "Index admissions (one per patient, covariates present)"     = "patients",
  "After restricting controls to the region of common support" = "patients"
)

.xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x
}

# Wrap a label to at most `width` characters per line, returning <tspan> rows.
.wrap_lines <- function(txt, width = 42) {
  words <- strsplit(txt, " ", fixed = TRUE)[[1]]
  lines <- character(0); cur <- ""
  for (w in words) {
    cand <- if (nzchar(cur)) paste(cur, w) else w
    if (nchar(cand) > width && nzchar(cur)) { lines <- c(lines, cur); cur <- w }
    else cur <- cand
  }
  c(lines, cur)
}

build_consort_svg <- function(flow, n_inmate, n_control, path,
                              n_inmate_pool = NULL) {

  stopifnot(all(c("step", "n") %in% names(flow)), nrow(flow) >= 2)
  cm <- function(x) formatC(as.integer(x), big.mark = ",", format = "d")

  steps <- as.character(flow$step)
  ns    <- as.integer(flow$n)
  k     <- length(steps)

  lab  <- ifelse(steps %in% names(.consort_box_labels),
                 .consort_box_labels[steps], steps)
  unit <- ifelse(steps %in% names(.consort_units), .consort_units[steps], "")

  # Geometry
  x_main <- 300; w_main <- 360; x_box <- x_main - w_main / 2
  x_exc  <- 700; w_exc  <- 340; x_exc_box <- x_exc - w_exc / 2
  y0     <- 46;  h_box  <- 62;  pitch <- 116
  y_top  <- y0 + (seq_len(k) - 1) * pitch          # top of each main box
  h_last <- 78                                     # last flow box is taller
  y_final_top <- y_top[k] + h_last + 74            # matched-cohort box
  h_final <- 96
  height  <- y_final_top + h_final + 40

  pool      <- ns[k] - n_inmate
  unmatched <- pool - n_control
  final     <- n_inmate + n_control

  parts <- c(sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 900 %d" width="900" height="%d" font-family="Arial, Helvetica, sans-serif">
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#1F4E79"/></marker>
    <marker id="arrowGrey" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#8A8A8A"/></marker>
  </defs>
  <rect width="900" height="%d" fill="#FFFFFF"/>
  <text x="%d" y="26" text-anchor="middle" font-size="17" font-weight="bold" fill="#1F4E79">Cohort selection (CONSORT-style flow)</text>',
    height, height, height, x_main))

  for (i in seq_len(k)) {
    hb <- if (i == k) h_last else h_box
    parts <- c(parts, sprintf(
      '<rect x="%d" y="%d" width="%d" height="%d" rx="6" fill="#FFFFFF" stroke="#1F4E79" stroke-width="2"/>
       <text x="%d" y="%d" text-anchor="middle" font-size="14" font-weight="bold" fill="#1a1a1a">%s</text>
       <text x="%d" y="%d" text-anchor="middle" font-size="13" fill="#1a1a1a">n = %s%s</text>',
      x_box, y_top[i], w_main, hb,
      x_main, y_top[i] + 25, .xml_escape(lab[i]),
      x_main, y_top[i] + 45, cm(ns[i]),
      if (nzchar(unit[i])) paste0(" ", unit[i]) else ""))

    # extra line on the last flow box: how the supported cohort splits
    if (i == k) {
      parts <- c(parts, sprintf(
        '<text x="%d" y="%d" text-anchor="middle" font-size="12" fill="#555">%s inmates &#160;&#8226;&#160; %s non-inmate pool</text>',
        x_main, y_top[i] + 66, cm(n_inmate_pool %||% n_inmate), cm(pool)))
    }

    # vertical arrow to the next box
    if (i < k) {
      parts <- c(parts, sprintf(
        '<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#1F4E79" stroke-width="2" marker-end="url(#arrow)"/>',
        x_main, y_top[i] + hb, x_main, y_top[i + 1] - 6))

      excluded <- ns[i] - ns[i + 1]
      nxt <- steps[i + 1]
      reason <- if (nxt %in% names(.consort_exclusion_labels)) {
        .consort_exclusion_labels[[nxt]]
      } else paste("Excluded at:", nxt)

      if (excluded > 0) {
        y_mid  <- y_top[i] + hb + (y_top[i + 1] - y_top[i] - hb) / 2
        lines  <- .wrap_lines(sprintf("%s  (n = %s)", reason, cm(excluded)), 44)
        h_e    <- 20 + 15 * length(lines)
        parts <- c(parts, sprintf(
          '<line x1="%d" y1="%.0f" x2="%d" y2="%.0f" stroke="#8A8A8A" stroke-width="1.6" stroke-dasharray="4 3" marker-end="url(#arrowGrey)"/>
           <rect x="%d" y="%.0f" width="%d" height="%.0f" rx="5" fill="#F5F5F5" stroke="#9A9A9A" stroke-width="1.4"/>',
          x_main, y_mid, x_exc_box - 6, y_mid,
          x_exc_box, y_mid - h_e / 2, w_exc, h_e))
        for (j in seq_along(lines)) {
          parts <- c(parts, sprintf(
            '<text x="%d" y="%.0f" text-anchor="middle" font-size="12.5" fill="#333">%s</text>',
            x_exc, y_mid - h_e / 2 + 16 + 15 * (j - 1), .xml_escape(lines[j])))
        }
      }
    }
  }

  # Arrow into the matched-cohort box, plus the unmatched-controls exclusion.
  y_mid <- y_top[k] + h_last + (y_final_top - y_top[k] - h_last) / 2
  parts <- c(parts, sprintf(
    '<line x1="%d" y1="%d" x2="%d" y2="%d" stroke="#1F4E79" stroke-width="2" marker-end="url(#arrow)"/>',
    x_main, y_top[k] + h_last, x_main, y_final_top - 6))
  if (unmatched > 0) {
    parts <- c(parts, sprintf(
      '<line x1="%d" y1="%.0f" x2="%d" y2="%.0f" stroke="#8A8A8A" stroke-width="1.6" stroke-dasharray="4 3" marker-end="url(#arrowGrey)"/>
       <rect x="%d" y="%.0f" width="%d" height="35" rx="5" fill="#F5F5F5" stroke="#9A9A9A" stroke-width="1.4"/>
       <text x="%d" y="%.0f" text-anchor="middle" font-size="12.5" fill="#333">Unmatched non-inmate controls  (n = %s)</text>',
      x_main, y_mid, x_exc_box - 6, y_mid,
      x_exc_box, y_mid - 17.5, w_exc,
      x_exc, y_mid + 4.5, cm(unmatched)))
  }

  parts <- c(parts, sprintf(
    '<rect x="%d" y="%d" width="%d" height="%d" rx="6" fill="#EAF1FA" stroke="#1F4E79" stroke-width="2"/>
     <text x="%d" y="%d" text-anchor="middle" font-size="14" font-weight="bold" fill="#1a1a1a">Propensity-matched cohort</text>
     <text x="%d" y="%d" text-anchor="middle" font-size="13" fill="#1a1a1a">optimal 2:1  &#8226;  n = %s</text>
     <text x="%d" y="%d" text-anchor="middle" font-size="13" font-weight="bold" fill="#1F4E79">%s inmates &#160;|&#160; %s matched controls</text>',
    x_box, y_final_top, w_main, h_final,
    x_main, y_final_top + 26,
    x_main, y_final_top + 48, cm(final),
    x_main, y_final_top + 74, cm(n_inmate), cm(n_control)))

  parts <- c(parts, "</svg>")

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(paste(parts, collapse = "\n"), path)
  invisible(path)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

generate_consort_diagram <- function(
    flow_path     = here::here("output", "cohort_flow.csv"),
    matched_path  = here::here("output", "matched_cohort.rds"),
    analytic_path = NULL,
    out_path      = here::here("output", "consort_diagram.svg")) {

  if (!file.exists(flow_path) || !file.exists(matched_path)) {
    stop("Need output/cohort_flow.csv and output/matched_cohort.rds. ",
         "Run run_all.R first.", call. = FALSE)
  }
  flow    <- utils::read.csv(flow_path, stringsAsFactors = FALSE)
  matched <- readRDS(matched_path)
  n_inm   <- sum(matched$inmate == "Inmate")
  n_ctl   <- sum(matched$inmate == "Non-inmate")

  path <- build_consort_svg(flow, n_inm, n_ctl, out_path, n_inmate_pool = n_inm)
  message("Wrote ", path, " (", nrow(flow), " flow steps).")
  invisible(path)
}

if (sys.nframe() == 0) generate_consort_diagram()
