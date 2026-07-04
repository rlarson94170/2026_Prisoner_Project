# ---------------------------------------------------------------------------
# 00_setup.R
# Packages, configuration, and small helpers shared across the pipeline.
# ---------------------------------------------------------------------------

## ---- Packages -------------------------------------------------------------
# Install once if needed:
# install.packages(c("tidyverse","readxl","janitor","MatchIt","optmatch",
#                    "cobalt","tableone","survey","here"))
suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, tidyr, stringr, ggplot2, readr, purrr
  library(readxl)      # read the .xlsx export
  library(janitor)     # clean_names(), tabyl()
  library(MatchIt)     # propensity score matching
  library(optmatch)    # optimal full matching engine
  library(cobalt)      # balance tables and Love plots
  library(tableone)    # Table 1 with standardized mean differences
  library(survey)      # weighted Table 1 for full matching
  library(here)        # project-root-relative paths
})

## ---- Configuration --------------------------------------------------------
# config.R is git-ignored and holds the local data path (see config.example.R).
config_file <- here::here("config.R")
if (!file.exists(config_file)) {
  stop(
    "config.R not found. Copy config.example.R to config.R and set RAW_DATA_PATH.",
    call. = FALSE
  )
}
source(config_file)

if (!exists("SEED")) SEED <- 20260704
set.seed(SEED)

## ---- Output folders (all git-ignored) -------------------------------------
dir.create(here::here("output"),  showWarnings = FALSE)
dir.create(here::here("private"), showWarnings = FALSE)

## ---- Helpers --------------------------------------------------------------
source(here::here("R", "utils.R"))

message("00_setup.R loaded. Seed = ", SEED)
