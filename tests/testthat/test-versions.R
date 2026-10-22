test_that("version maps to the format Stata itself writes", {
  expect_equal(dta_format_for(13, 10), 117L)
  expect_equal(dta_format_for(14, 10), 118L)
  expect_equal(dta_format_for(19, 10), 118L)
  expect_equal(dta_format_for(15, 40000), 119L)
  expect_equal(dta_format_for(19, 40000), 119L)
  expect_error(dta_format_for(14, 40000), "32,767")
  expect_error(dta_format_for(13, 40000), "32,767")
  expect_error(dta_format_for(12, 10), "between 13 and 19")
  expect_error(dta_format_for("14", 10), "between 13 and 19")
})

test_that("format 117 (Stata 13) files round-trip", {
  skip_if_not_installed("haven")
  df <- data.frame(
    x = c(1, 300, NA), s = c("a", "bb", ""), f = factor(c("p", "q", "p")),
    l = c(strrep("z", 2500), "", "k"),
    stringsAsFactors = FALSE
  )
  attr(df$x, "label") <- "Thirteen"
  path <- tempfile(fileext = ".dta")
  write_dta(df, path, version = 13, label = "v13")
  d <- describe_dta(path)
  expect_equal(attr(d, "format"), 117L)
  expect_equal(d$type, c("int", "str2", "byte", "strL"))
  expect_equal(d$label[1], "Thirteen")
  expect_equal(attr(d, "label"), "v13")
  back <- read_back(path)
  expect_equal(back$x, df$x)
  expect_equal(back$s, df$s)
  expect_equal(back$l, df$l)
  expect_equal(attr(back$f, "labels"), c(p = 1, q = 2))
})

test_that("format 119 files round-trip (small K, forced)", {
  df <- data.frame(
    x = c(5, NA), s = c("hello", "w"), l = c(strrep("q", 2046), strrep("q", 2046)),
    stringsAsFactors = FALSE
  )
  path <- tempfile(fileext = ".dta")
  write_dta_impl(df, path, fmt = 119L)
  d <- describe_dta(path)
  expect_equal(attr(d, "format"), 119L)
  expect_equal(d$type, c("byte", "str5", "strL"))

  # Stata's dta_119 spec packs a strL reference as a 3-byte v and a 5-byte o.
  # (haven's ReadStat uses the 118 layout for 119 files, so it cannot be used
  # to check this; pandas follows the spec and reads these files correctly.)
  bytes <- readBin(path, "raw", file.size(path))
  data_start <- grepRaw("<data>", bytes, fixed = TRUE) + 6L
  rowsize <- 1L + 5L + 8L
  cell <- function(row) bytes[data_start + (row - 1L) * rowsize + 6L + 0:7]
  expect_equal(as.integer(cell(1)), c(3, 0, 0, 1, 0, 0, 0, 0)) # (v, o) = (3, 1)
  expect_equal(as.integer(cell(2)), c(3, 0, 0, 1, 0, 0, 0, 0)) # shared GSO
  # exactly one GSO: "GSO" + v (4) + o (8) + t (1) + len (4) + 2046 + "\0"
  expect_equal(length(grepRaw("GSO", bytes, fixed = TRUE, all = TRUE)), 1L)
  gso <- grepRaw("GSO", bytes, fixed = TRUE)
  expect_equal(as.integer(bytes[gso + 3:19]), c(3, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 130, 255, 7, 0, 0))

  skip_if_not_installed("haven")
  back <- read_back(path)
  expect_equal(back$x, df$x)
  expect_equal(back$s, df$s)
})
