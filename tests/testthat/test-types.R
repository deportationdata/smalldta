test_that("doubles holding whole numbers are demoted like Stata's compress", {
  df <- data.frame(
    byte_lo = c(-127, 0, 100),
    int_lo = c(-32767, 0, 32740),
    long_lo = c(-2147483647, 0, 2147483620),
    byte_over = c(-128, 0, 0), # -128 is not a valid byte in Stata
    byte_over2 = c(0, 101, 0), # 101 is byte's sysmiss
    int_over = c(0, 32741, 0),
    long_over = c(0, 2147483621, 0),
    real = c(0.5, 1, 2),
    big = c(1e15, 0, 0)
  )
  expect_equal(unname(types_of(df)), c(
    "byte", "int", "long", "int", "int", "long", "double", "double", "double"
  ))
})

test_that("integers, logicals and all-missing columns take the smallest type", {
  df <- data.frame(
    i_byte = c(1L, 100L, NA),
    i_int = c(1L, 30000L, NA),
    i_long = c(1L, 40000L, NA),
    i_max = c(1L, .Machine$integer.max, NA), # exceeds Stata's long range
    lgl = c(TRUE, FALSE, NA),
    all_na_dbl = c(NA_real_, NA_real_, NA_real_),
    all_na_int = c(NA_integer_, NA_integer_, NA_integer_),
    all_na_chr = c(NA_character_, NA_character_, NA_character_)
  )
  expect_equal(unname(types_of(df)), c(
    "byte", "int", "long", "double", "byte", "byte", "byte", "str1"
  ))
})

test_that("compress = FALSE reproduces haven's wide types", {
  df <- data.frame(x = c(1, 2), i = c(1L, 2L), l = c(TRUE, FALSE), f = factor(c("a", "b")))
  expect_equal(unname(types_of(df, compress = FALSE)), c("double", "long", "byte", "long"))
  expect_equal(unname(types_of(df)), c("byte", "byte", "byte", "byte"))
})

test_that("strings are sized in UTF-8 bytes and switch to strL past 2045 bytes", {
  df <- data.frame(
    ascii = c("a", "bbbb", ""),
    utf8 = c("é", "日本語", "x"), # 2 and 9 bytes
    empty = c("", "", ""),
    exact = c(strrep("x", 2045), "", "a"),
    longer = c(strrep("x", 2046), "", "a"),
    stringsAsFactors = FALSE
  )
  expect_equal(unname(types_of(df)), c("str4", "str9", "str1", "str2045", "strL"))
})

test_that("dta_types() previews without writing", {
  df <- data.frame(x = c(1, 2), s = c("ab", "c"), f = factor(c("u", "v")))
  attr(df$x, "label") <- "Ex"
  t <- dta_types(df)
  expect_equal(t$type, c("byte", "str2", "byte"))
  expect_equal(t$bytes, c(1L, 2L, 1L))
  expect_equal(t$format, c("%8.0g", "%9s", "%8.0g"))
  expect_equal(t$label, c("Ex", "", ""))
  expect_equal(t$value_labels, c(0L, 0L, 2L))
})

test_that("the file is much smaller than haven's for the same data", {
  skip_if_not_installed("haven")
  n <- 1000
  df <- data.frame(
    a = as.numeric(sample(0:9, n, TRUE)),
    b = as.numeric(sample(1000:2000, n, TRUE)),
    c = sample(c(TRUE, FALSE), n, TRUE),
    d = sample(c("x", "yy"), n, TRUE),
    stringsAsFactors = FALSE
  )
  ours <- tempfile(fileext = ".dta")
  theirs <- tempfile(fileext = ".dta")
  write_dta(df, ours)
  haven::write_dta(df, theirs)
  expect_equal(unname(describe_dta(theirs)$type)[1:2], c("double", "double"))
  expect_equal(unname(describe_dta(ours)$type), c("byte", "int", "byte", "str2"))
  expect_lt(file.size(ours), file.size(theirs) / 2)
  expect_equal(read_back(ours), read_back(theirs))
})
