write_tmp <- function(df, ...) {
  path <- tempfile(fileext = ".dta")
  write_dta(df, path, ...)
  path
}

types_of <- function(df, ...) {
  d <- describe_dta(write_tmp(df, ...))
  stats::setNames(d$type, d$variable)
}

# Read back with haven (ReadStat), an independent parser.
read_back <- function(path) {
  skip_if_not_installed("haven")
  df <- as.data.frame(haven::read_dta(path))
  for (v in names(df)) attr(df[[v]], "format.stata") <- NULL
  df
}
