test_that("values survive a round trip through haven for every storage type", {
  skip_if_not_installed("haven")
  df <- data.frame(
    v_byte = c(-127, 0, 100, NA),
    v_int = c(-32767, 0, 32740, NA),
    v_long = c(-2147483647, 0, 2147483620, NA),
    dbl = c(-1.5, 0.1, 1e300, NA),
    int_col = c(1L, NA, 3L, 4L),
    lgl = c(TRUE, FALSE, NA, TRUE),
    chr = c("a", "", NA, "日本語"),
    stringsAsFactors = FALSE
  )
  path <- write_tmp(df)
  expect_equal(
    unname(describe_dta(path)$type),
    c("byte", "int", "long", "double", "byte", "byte", "str9")
  )
  back <- read_back(path)
  expect_equal(back$v_byte, df$v_byte)
  expect_equal(back$v_int, df$v_int)
  expect_equal(back$v_long, df$v_long)
  expect_equal(back$dbl, df$dbl)
  expect_equal(back$int_col, as.numeric(df$int_col))
  expect_equal(back$lgl, as.numeric(df$lgl))
  # Stata has no missing string: NA becomes ""
  expect_equal(back$chr, c("a", "", "", "日本語"))
})

test_that("NaN and Inf are written as system missing", {
  skip_if_not_installed("haven")
  back <- read_back(write_tmp(data.frame(x = c(NaN, Inf, -Inf, 1.5))))
  expect_equal(back$x, c(NA, NA, NA, 1.5))
})

test_that("tagged NAs become extended missing values in every numeric type", {
  skip_if_not_installed("haven")
  df <- data.frame(
    b = c(1, tagged_na("a"), tagged_na("z"), NA),
    i = c(1000, tagged_na("b"), NA, 2),
    l = c(1e6, tagged_na("c"), NA, 2),
    d = c(0.5, tagged_na("d"), NA, 2)
  )
  path <- write_tmp(df)
  expect_equal(unname(describe_dta(path)$type), c("byte", "int", "long", "double"))
  back <- read_back(path)
  for (v in names(df)) {
    expect_equal(haven::na_tag(back[[v]]), haven::na_tag(df[[v]]), info = v)
    expect_equal(is.na(back[[v]]), is.na(df[[v]]), info = v)
  }
  expect_equal(back$b[1], 1)
  # our tagged_na() is bit-identical to haven's
  expect_identical(tagged_na("q"), haven::tagged_na("q"))
})

test_that("factors and labelled vectors get value labels; codes are compressed", {
  skip_if_not_installed("haven")
  df <- data.frame(
    f = factor(c("lo", "hi", NA, "lo"), levels = c("lo", "hi")),
    lab = haven::labelled(c(1, 2, NA, 200), c(one = 1, two = 2, big = 200)),
    lab_int = haven::labelled(c(1L, 5L, NA, 5L), c(uno = 1L, cinco = 5L)),
    lab_na = haven::labelled(c(1, tagged_na("a"), NA, 1), c(yes = 1, refused = tagged_na("a")))
  )
  path <- write_tmp(df)
  d <- describe_dta(path)
  expect_equal(d$type, c("byte", "int", "byte", "byte"))
  expect_equal(d$value_label, c("f", "lab", "lab_int", "lab_na"))
  back <- haven::read_dta(path)
  expect_equal(attr(back$f, "labels"), c(lo = 1, hi = 2))
  expect_equal(as.character(haven::as_factor(back$f)), c("lo", "hi", NA, "lo"))
  expect_equal(attr(back$lab, "labels"), c(one = 1, two = 2, big = 200))
  expect_equal(attr(back$lab_int, "labels"), c(uno = 1, cinco = 5))
  expect_equal(names(attr(back$lab_na, "labels")), c("yes", "refused"))
  expect_equal(haven::na_tag(attr(back$lab_na, "labels")), c(NA, "a"))
})

test_that("unlabellable value labels are dropped with a warning", {
  df <- data.frame(x = haven::labelled(c(1, 2.5), c(a = 1, b = 2.5, c = NA)))
  expect_warning(path <- write_tmp(df), "Dropping 2 value label")
  expect_equal(describe_dta(path)$type, "double")
  skip_if_not_installed("haven")
  expect_equal(attr(haven::read_dta(path)$x, "labels"), c(a = 1))
})

test_that("dates and times use Stata's epoch and formats", {
  skip_if_not_installed("haven")
  df <- data.frame(
    d = as.Date(c("1960-01-01", "1970-01-01", "2024-02-29", NA)),
    t = as.POSIXct(c("1960-01-01 00:00:00", "2024-02-29 12:34:56.789", NA, "1999-12-31 23:59:59"), tz = "UTC")
  )
  path <- write_tmp(df)
  d <- describe_dta(path)
  expect_equal(d$type, c("int", "double"))
  expect_equal(d$format, c("%td", "%tc"))
  back <- read_back(path)
  expect_equal(back$d, df$d)
  expect_equal(as.numeric(back$t), as.numeric(df$t), tolerance = 1e-9)
  expect_s3_class(back$t, "POSIXct")
})

test_that("labels, formats and the dataset label are written", {
  skip_if_not_installed("haven")
  df <- data.frame(x = c(1.5, 2), y = "s", stringsAsFactors = FALSE)
  attr(df$x, "label") <- "Variable label with ünïcode"
  attr(df$x, "format.stata") <- "%5.2f"
  attr(df$y, "label") <- "Second"
  path <- write_tmp(df, label = "My dataset")
  d <- describe_dta(path)
  expect_equal(d$label, c("Variable label with ünïcode", "Second"))
  expect_equal(d$format, c("%5.2f", "%9s"))
  expect_equal(attr(d, "label"), "My dataset")
  expect_equal(attr(d, "nobs"), 2)
  expect_equal(attr(d, "nvar"), 2)
  expect_match(attr(d, "timestamp"), "^[0-9]{2} [A-Z][a-z]{2} [0-9]{4} [0-9]{2}:[0-9]{2}$")
  back <- haven::read_dta(path)
  expect_equal(attr(back$x, "label"), "Variable label with ünïcode")
  expect_equal(attr(back$x, "format.stata"), "%5.2f")
  expect_equal(attr(back, "label"), "My dataset")
})

test_that("over-long labels are truncated with a warning", {
  df <- data.frame(x = 1)
  attr(df$x, "label") <- strrep("é", 81)
  expect_warning(path <- write_tmp(df), "truncated to 80 characters")
  expect_equal(describe_dta(path)$label, strrep("é", 80))
})

test_that("strL columns share repeated values and round-trip", {
  skip_if_not_installed("haven")
  long <- strrep("L", 3000)
  df <- data.frame(
    s = c(long, "", long, NA, "short", "short"),
    n = 1:6,
    s2 = c("", strrep("M", 2100), strrep("M", 2100), "x", "", "y"),
    stringsAsFactors = FALSE
  )
  path <- write_tmp(df)
  expect_equal(describe_dta(path)$type, c("strL", "byte", "strL"))
  # duplicates are stored once: the same data with distinct long values is
  # bigger by (at least) the 3000 + 2100 bytes the repeats would need
  distinct <- df
  distinct$s[3] <- strrep("K", 3000)
  distinct$s2[3] <- strrep("N", 2100)
  expect_lte(file.size(path), file.size(write_tmp(distinct)) - 5100)
  back <- read_back(path)
  expect_equal(back$s, c(long, "", long, "", "short", "short"))
  expect_equal(back$s2, df$s2)
  expect_equal(back$n, as.numeric(df$n))
})

test_that("zero-row and single-column data frames are valid files", {
  skip_if_not_installed("haven")
  df <- data.frame(x = numeric(), s = character(), stringsAsFactors = FALSE)
  path <- write_tmp(df)
  expect_equal(describe_dta(path)$type, c("byte", "str1"))
  back <- read_back(path)
  expect_equal(nrow(back), 0L)
  expect_equal(names(back), c("x", "s"))

  one <- read_back(write_tmp(data.frame(only = 7)))
  expect_equal(one$only, 7)
})

test_that("a larger table with every type round-trips", {
  skip_if_not_installed("haven")
  set.seed(1)
  n <- 20000
  df <- data.frame(
    a = sample(c(-127:100, NA), n, TRUE) + 0,
    b = sample(c(-32767:32740, NA), n, TRUE) + 0,
    c = sample(c(-2147483647, 2147483620, 0, NA), n, TRUE),
    d = rnorm(n),
    e = sample(c("", "a", "bcd", NA, "日本"), n, TRUE),
    f = factor(sample(letters, n, TRUE)),
    g = sample(c(TRUE, FALSE, NA), n, TRUE),
    stringsAsFactors = FALSE
  )
  path <- write_tmp(df)
  expect_equal(describe_dta(path)$type, c("byte", "int", "long", "double", "str6", "byte", "byte"))
  back <- read_back(path)
  expect_equal(back$a, df$a)
  expect_equal(back$b, df$b)
  expect_equal(back$c, df$c)
  expect_equal(back$d, df$d)
  expect_equal(back$e, ifelse(is.na(df$e), "", df$e))
  expect_equal(as.character(haven::as_factor(back$f)), as.character(df$f))
  expect_equal(back$g, as.numeric(df$g))
})
