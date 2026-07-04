# ---------------------------------------------------------------------------
# Local configuration TEMPLATE.
#
# Copy this file to `config.R` (which is git-ignored) and edit the path so it
# points to YOUR local copy of the registry export. Never commit config.R and
# never place the raw data inside the repository if you can avoid it.
# ---------------------------------------------------------------------------

# Absolute path to the raw VQI PVI registry export on your machine.
RAW_DATA_PATH <- "/absolute/path/to/2016_2024_PVI_DATA.xlsx"

# Worksheet name inside that workbook.
RAW_DATA_SHEET <- "pvi_combined"

# Random seed so the matching is reproducible across team members.
SEED <- 20260704
