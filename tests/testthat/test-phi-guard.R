# Guardrails so PHI or a real data path never lands in a committed file.

repo_source_files <- function() {
  root <- file.path("..", "..")
  list.files(
    c(file.path(root, "R"), root),
    pattern    = "\\.(R|md)$",
    full.names = TRUE,
    recursive  = FALSE
  )
}

test_that("no source file hard-codes a medical record number", {
  files <- repo_source_files()
  offenders <- character(0)
  for (f in files) {
    txt <- readLines(f, warn = FALSE)
    # An MRN assignment would look like mrn <- "123456" or MRN = '000123'.
    hits <- grep("(?i)mrn\\s*(<-|=)\\s*['\"][0-9]{4,}", txt, perl = TRUE, value = TRUE)
    if (length(hits)) offenders <- c(offenders, paste(basename(f), hits))
  }
  expect_equal(offenders, character(0))
})

test_that(".gitignore blocks data, config, outputs, and the crosswalk", {
  gi <- readLines(file.path("..", "..", ".gitignore"), warn = FALSE)
  for (pat in c("config.R", "data/", "output/", "private/", "*.xlsx", "*.rds")) {
    expect_true(any(trimws(gi) == pat), info = pat)
  }
})

test_that("the example config ships a placeholder path, not a real one", {
  cfg <- readLines(file.path("..", "..", "config.example.R"), warn = FALSE)
  path_line <- grep("RAW_DATA_PATH", cfg, value = TRUE)
  expect_true(any(grepl("/absolute/path/to/", path_line)))
})

test_that("a local config.R, if present, is git-ignored", {
  # config.R holds the real data path and is expected to exist locally once set
  # up. What matters is that it can never be committed, so it must be listed in
  # .gitignore rather than being absent.
  gi <- readLines(file.path("..", "..", ".gitignore"), warn = FALSE)
  expect_true(any(trimws(gi) == "config.R"))
})
