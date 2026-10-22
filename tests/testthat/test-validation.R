test_that("invalid Stata variable names are rejected", {
  bad <- function(nm) {
    df <- data.frame(1)
    names(df) <- nm
    expect_error(write_dta(df, tempfile()), "Invalid Stata variable name", info = nm)
  }
  bad("my.var")
  bad("1abc")
  bad("with space")
  bad("_all")
  bad("byte")
  bad("str12")
  bad(strrep("a", 33))
  df <- data.frame(1)
  names(df) <- ""
  expect_error(write_dta(df, tempfile()), "must be named")
})

test_that("unicode names are allowed in 118 but not 117", {
  df <- data.frame(1)
  names(df) <- "variável"
  expect_silent(write_dta(df, tempfile()))
  expect_error(write_dta(df, tempfile(), version = 13), "Invalid Stata variable name")
  names(df) <- "ok_name_32chars_long_exactly_xxxx"
  expect_equal(nchar(names(df)), 33)
  expect_error(write_dta(df, tempfile()), "Invalid Stata variable name")
})

test_that("duplicate names and unsupported columns are rejected", {
  df <- data.frame(a = 1, b = 2)
  names(df) <- c("a", "a")
  expect_error(write_dta(df, tempfile()), "Duplicate column names")
  df <- data.frame(a = I(list(1, 2)))
  expect_error(write_dta(df, tempfile()), "cannot be written to Stata")
  df <- data.frame(a = complex(2))
  expect_error(write_dta(df, tempfile()), "cannot be written to Stata")
  expect_error(write_dta(list(a = 1), tempfile()), "must be a data frame")
  expect_error(write_dta(data.frame(a = 1)[, FALSE], tempfile()), "at least one column")
})

test_that("bad labels and formats are rejected", {
  df <- data.frame(a = 1)
  attr(df$a, "format.stata") <- "9.0g"
  expect_error(write_dta(df, tempfile()), "display format")
  df <- data.frame(a = 1)
  attr(df$a, "label") <- c("two", "labels")
  expect_error(write_dta(df, tempfile()), "single string")
  expect_error(write_dta(data.frame(a = 1), tempfile(), label = 1), "single string")
  expect_error(write_dta(data.frame(a = 1), tempfile(), version = 20), "between 13 and 19")
})

test_that("labelled character columns lose their labels with a warning", {
  skip_if_not_installed("haven")
  df <- data.frame(s = haven::labelled(c("a", "b"), c(A = "a")))
  expect_warning(path <- write_tmp(df), "string variable")
  expect_equal(describe_dta(path)$value_label, "")
})

test_that("unwritable paths give a clear error", {
  expect_error(write_dta(data.frame(a = 1), file.path(tempdir(), "no", "such", "dir", "x.dta")), "Cannot open")
})

test_that("describe_dta rejects non-dta files", {
  f <- tempfile()
  writeLines("hello", f)
  expect_error(describe_dta(f), "not a Stata 13\\+ .dta file")
})

test_that("tagged_na validates its input", {
  expect_error(tagged_na("A"), "single lowercase")
  expect_error(tagged_na("ab"), "single lowercase")
  expect_true(all(is.na(tagged_na("a", "b"))))
})
