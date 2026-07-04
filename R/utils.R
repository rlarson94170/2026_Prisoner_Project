# ---------------------------------------------------------------------------
# utils.R
# Small helpers shared across the pipeline and the tests. Kept dependency-free
# so the test suite can source this without loading configuration or data.
# ---------------------------------------------------------------------------

# Coerce a coded column to a trimmed character vector so numeric/text exports
# both behave. Blank / placeholder strings become NA.
as_code <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

# TRUE when a coded value represents a present/positive finding, i.e. it is not
# missing and not one of the given "absent" codes.
is_present <- function(x, absent = c("0")) {
  x <- as_code(x)
  !is.na(x) & !(x %in% absent)
}
