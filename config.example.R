# ---------------------------------------------------------------------------
# Local configuration TEMPLATE.
#
# Copy this file to `config.R` (which is git-ignored) and edit the paths so
# they point to YOUR local copies. Never commit config.R and never place the
# raw data inside the repository if you can avoid it.
# ---------------------------------------------------------------------------

# Absolute path to the raw VQI PVI registry export on your machine.
RAW_DATA_PATH <- "/absolute/path/to/2016_2024_PVI_DATA.xlsx"

# Worksheet name inside that workbook.
RAW_DATA_SHEET <- "pvi_combined"

# Optional: manually maintained corrections to unreliable registry fields.
#
# Two fields need it. The `Inmate` flag came from an IT sweep whose method
# changed when the EMR moved from Cerner to Epic, so it under-ascertains the
# earlier era. And "Age at Procedure" is corrupt for at least one patient
# (recorded as 0 and 5 for a man who was 72 and 77), which without a correction
# would push a valid patient out through the adult age floor.
#
# Corrections live with the data rather than in the code, so the list stays
# auditable and no MRN is hard-coded in the pipeline.
#
# Accepts .xlsx or .csv. Columns (case-insensitive, all optional but MRN):
#   MRN            required
#   Inmate         yes/no, y/n, true/false, or 1/0
#   DOB            date of birth; recomputes age for ALL of that patient's
#                  procedures, which is the point - age errors are per-patient
#   Surgery Date   scopes an Inmate correction to a single procedure
#   Note           free text, carried into the audit log
#
# Updates naming an MRN that is absent from the export cannot be applied: that
# patient was never in the registry pull. The pipeline warns and validate.R
# records them in private/known_missing_patients.csv as a documented exclusion.
# Set to NULL if you have no updates file.
PATIENT_UPDATES_PATH <- "/absolute/path/to/Patient Updates.xlsx"

# Random seed so the matching is reproducible across team members.
SEED <- 20260704
