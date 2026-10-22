#' Describe the variables stored in a `.dta` file
#'
#' Reads only the metadata of a Stata file (formats 117, 118 and 119; Stata
#' 13 and later) and reports each variable's storage type, like Stata's
#' `describe`. Useful to check how compactly a file was written.
#'
#' @param path Path to a `.dta` file.
#' @return A data frame with columns `variable`, `type`, `format`,
#'   `value_label` and `label`, with attributes `format` (the `.dta` format
#'   number), `nobs`, `nvar`, `label` (dataset label) and `timestamp`.
#' @examples
#' path <- tempfile(fileext = ".dta")
#' write_dta(data.frame(x = c(1, 2, 3), s = c("a", "bb", "ccc")), path)
#' describe_dta(path)
#' @export
describe_dta <- function(path) {
  con <- file(path.expand(path), "rb")
  on.exit(close(con))
  read_str <- function(n) readChar(con, n, useBytes = TRUE)
  expect <- function(what) {
    got <- read_str(nchar(what))
    if (!identical(got, what)) {
      stop("`", path, "` is not a Stata 13+ .dta file (expected '", what, "')", call. = FALSE)
    }
  }
  expect("<stata_dta><header><release>")
  fmt <- as.integer(read_str(3L))
  if (!(fmt %in% c(117L, 118L, 119L))) {
    stop("Unsupported .dta format ", fmt, call. = FALSE)
  }
  spec <- dta_spec(fmt)
  expect("</release><byteorder>")
  endian <- switch(read_str(3L), LSF = "little", MSF = "big", stop("Bad byteorder", call. = FALSE))
  read_uint <- function(n) {
    b <- as.integer(readBin(con, "raw", n))
    if (endian == "big") b <- rev(b)
    sum(b * 256^(seq_len(n) - 1))
  }
  expect("</byteorder><K>")
  K <- read_uint(spec$k_bytes)
  expect("</K><N>")
  N <- read_uint(spec$n_bytes)
  expect("</N><label>")
  label <- read_str(read_uint(spec$label_len_bytes))
  expect("</label><timestamp>")
  timestamp <- read_str(read_uint(1L))
  expect("</timestamp></header><map>")
  map <- vapply(1:14, function(i) read_uint(8L), 1)

  read_fixed <- function(offset, tagname, width) {
    seek(con, map[offset] + nchar(tagname) + 2L)
    b <- readBin(con, "raw", width * K)
    vapply(seq_len(K), function(i) {
      chunk <- b[(i - 1L) * width + seq_len(width)]
      end <- match(as.raw(0L), chunk)
      s <- rawToChar(chunk[seq_len(if (is.na(end)) width else end - 1L)])
      Encoding(s) <- "UTF-8"
      s
    }, "")
  }
  seek(con, map[3L] + nchar("<variable_types>"))
  codes <- vapply(seq_len(K), function(i) read_uint(2L), 1)
  out <- data.frame(
    variable = read_fixed(4L, "varnames", spec$name_bytes),
    type = type_name(codes),
    format = read_fixed(6L, "formats", spec$format_bytes),
    value_label = read_fixed(7L, "value_label_names", spec$lblname_bytes),
    label = read_fixed(8L, "variable_labels", spec$varlabel_bytes),
    stringsAsFactors = FALSE
  )
  attr(out, "format") <- fmt
  attr(out, "nobs") <- N
  attr(out, "nvar") <- K
  attr(out, "label") <- label
  attr(out, "timestamp") <- timestamp
  out
}
