# ---------------------------------------------------------------------------
# dev/make_consort_diagram.R
# Regenerates the CONSORT-style cohort-flow figure from the numbers the pipeline
# produced: output/cohort_flow.csv (written by 03_cohort.R) and the matched
# cohort (output/matched_cohort.rds). Writes a self-contained SVG, no plotting
# packages required.
#
# After sourcing:
#   source("dev/make_consort_diagram.R")
#   generate_consort_diagram()
# ---------------------------------------------------------------------------

build_consort_svg <- function(flow_n, n_inmate, n_control, path) {
  cm <- function(x) formatC(x, big.mark = ",", format = "d")

  # flow_n is the 6-step count vector from cohort_flow.csv, in order.
  b <- flow_n
  e <- c(b[1] - b[2], b[2] - b[3], b[3] - b[4], b[4] - b[5], b[5] - b[6])
  pool      <- b[6] - n_inmate
  unmatched <- pool - n_control
  final     <- n_inmate + n_control

  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 900 980" font-family="Arial, Helvetica, sans-serif">
  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#1F4E79"/></marker>
    <marker id="arrowGrey" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="strokeWidth"><path d="M0,0 L8,3 L0,6 Z" fill="#8A8A8A"/></marker>
  </defs>
  <text x="295" y="22" text-anchor="middle" font-size="17" font-weight="bold" fill="#1F4E79">Cohort selection (CONSORT-style flow)</text>
  <g stroke="#1F4E79" stroke-width="2" marker-end="url(#arrow)">
    <line x1="295" y1="102" x2="295" y2="148"/><line x1="295" y1="222" x2="295" y2="268"/>
    <line x1="295" y1="342" x2="295" y2="388"/><line x1="295" y1="462" x2="295" y2="508"/>
    <line x1="295" y1="582" x2="295" y2="628"/><line x1="295" y1="720" x2="295" y2="800"/>
  </g>
  <g stroke="#8A8A8A" stroke-width="1.6" stroke-dasharray="4 3" marker-end="url(#arrowGrey)">
    <line x1="295" y1="125" x2="536" y2="125"/><line x1="295" y1="245" x2="536" y2="245"/>
    <line x1="295" y1="365" x2="536" y2="365"/><line x1="295" y1="485" x2="536" y2="485"/>
    <line x1="295" y1="605" x2="536" y2="605"/><line x1="295" y1="760" x2="536" y2="760"/>
  </g>
  <g fill="#FFFFFF" stroke="#1F4E79" stroke-width="2">
    <rect x="120" y="36" width="350" height="66" rx="6"/><rect x="120" y="156" width="350" height="66" rx="6"/>
    <rect x="120" y="276" width="350" height="66" rx="6"/><rect x="120" y="396" width="350" height="66" rx="6"/>
    <rect x="120" y="516" width="350" height="66" rx="6"/><rect x="120" y="628" width="350" height="92" rx="6"/>
    <rect x="120" y="800" width="350" height="104" rx="6" fill="#EAF1FA"/>
  </g>
  <g font-size="14" fill="#1a1a1a" text-anchor="middle">
    <text x="295" y="63"><tspan font-weight="bold">PVI procedures, 2016–2024</tspan></text><text x="295" y="84" font-size="13">n = %s</text>
    <text x="295" y="183"><tspan font-weight="bold">Male procedures</tspan></text><text x="295" y="204" font-size="13">n = %s</text>
    <text x="295" y="303"><tspan font-weight="bold">Purely endovascular</tspan></text><text x="295" y="324" font-size="13">n = %s</text>
    <text x="295" y="423"><tspan font-weight="bold">After crossover handling</tspan></text><text x="295" y="444" font-size="13">n = %s procedures</text>
    <text x="295" y="543"><tspan font-weight="bold">Distinct index admissions</tspan></text><text x="295" y="564" font-size="13">n = %s</text>
    <text x="295" y="656"><tspan font-weight="bold">Index patients, complete covariates</tspan></text><text x="295" y="677" font-size="13">n = %s patients</text><text x="295" y="700" font-size="12" fill="#555">%s inmates  •  %s non-inmate pool</text>
    <text x="295" y="828"><tspan font-weight="bold">Propensity-matched cohort</tspan></text><text x="295" y="849" font-size="13">optimal 2:1  •  n = %s</text><text x="295" y="878" font-size="13" font-weight="bold" fill="#1F4E79">%s inmates &#160;|&#160; %s matched controls</text>
  </g>
  <g fill="#F5F5F5" stroke="#9A9A9A" stroke-width="1.4">
    <rect x="536" y="106" width="330" height="40" rx="5"/><rect x="536" y="226" width="330" height="40" rx="5"/>
    <rect x="536" y="344" width="330" height="44" rx="5"/><rect x="536" y="464" width="330" height="44" rx="5"/>
    <rect x="536" y="583" width="330" height="46" rx="5"/><rect x="536" y="740" width="330" height="40" rx="5"/>
  </g>
  <g font-size="12.5" fill="#333" text-anchor="middle">
    <text x="701" y="131">Female  (n = %s)</text>
    <text x="701" y="251">Hybrid, endovascular + open bypass  (n = %s)</text>
    <text x="701" y="362">Non-inmate admissions of</text><text x="701" y="378">crossover patients  (n = %s)</text>
    <text x="701" y="482">Staged same-admission</text><text x="701" y="498">procedures  (n = %s)</text>
    <text x="701" y="600">Non-index admissions &amp; incomplete</text><text x="701" y="616">matching covariates  (n = %s)</text>
    <text x="701" y="765">Unmatched non-inmate controls  (n = %s)</text>
  </g>
</svg>',
    cm(b[1]), cm(b[2]), cm(b[3]), cm(b[4]), cm(b[5]), cm(b[6]),
    cm(n_inmate), cm(pool), cm(final), cm(n_inmate), cm(n_control),
    cm(e[1]), cm(e[2]), cm(e[3]), cm(e[4]), cm(e[5]), cm(unmatched))

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(svg, path)
  invisible(path)
}

generate_consort_diagram <- function(
    flow_path    = here::here("output", "cohort_flow.csv"),
    matched_path = here::here("output", "matched_cohort.rds"),
    out_path     = here::here("output", "consort_diagram.svg")) {

  if (!file.exists(flow_path) || !file.exists(matched_path)) {
    stop("Need output/cohort_flow.csv and output/matched_cohort.rds. ",
         "Run run_all.R first.", call. = FALSE)
  }
  flow    <- utils::read.csv(flow_path)
  matched <- readRDS(matched_path)
  n_inm   <- sum(matched$inmate == "Inmate")
  n_ctl   <- sum(matched$inmate == "Non-inmate")
  path <- build_consort_svg(flow$n, n_inm, n_ctl, out_path)
  message("Wrote ", path)
  invisible(path)
}

if (sys.nframe() == 0) generate_consort_diagram()
