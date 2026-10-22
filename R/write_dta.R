#' Write a compact Stata `.dta` file
#'
#' Writes a data frame to Stata's `.dta` format using, for every variable, the
#' smallest storage type that holds its values exactly: the same demotions
#' Stata's own `compress` command performs (`double` to `long`/`int`/`byte`,
#' `str#` sized to the longest value, `strL` only beyond 2,045 bytes). The
#' result is a file that does not need to be `compress`ed after loading.
#'
#' The signature matches [haven::write_dta()], so it can be used as a drop-in
#' replacement. Column handling follows haven's conventions:
#'
#' * `logical`, `integer` and `double` columns become `byte`, `int`, `long` or
#'   `double`, whichever is the smallest type whose range holds every
#'   non-missing value (doubles must be whole numbers to be demoted; they are
#'   never converted to `float`).
#' * `character` columns become `str#` with `#` equal to the longest value in
#'   bytes (UTF-8), or `strL` when any value exceeds 2,045 bytes. Repeated
#'   `strL` values are stored once.
#' * `factor`s and `haven::labelled()` vectors are written with value labels;
#'   factor codes are then compressed like any other integer.
#' * `Date` becomes a `%td` date, `POSIXct` a `%tc` date-time (milliseconds,
#'   UTC), and `hms` a `%tcHH:MM:SS` time.
#' * `NA` becomes Stata's system missing value `.`; [tagged_na()] /
#'   `haven::tagged_na()` values become `.a`-`.z`. `NaN`, `Inf` and `-Inf`
#'   are written as `.`.
#' * The `label` attribute of a column is written as its variable label and a
#'   `format.stata` attribute as its display format.
#'
#' @param data A data frame. Every column must have a valid Stata variable
#'   name: 1-32 characters, starting with a letter or underscore, containing
#'   only letters, digits and underscores, and not a reserved word.
#' @param path Path to the file to write.
#' @param version Stata version to target, 13 to 19. Stata 14 through 19 all
#'   use format 118, so the default writes a file readable by Stata 14 or
#'   later; datasets with more than 32,767 variables are written in format
#'   119 (Stata 15+). `version = 13` writes format 117.
#' @param label Dataset label (at most 80 characters). Defaults to the
#'   `label` attribute of `data`, if any.
#' @param compress If `FALSE`, use haven's storage types instead (integers as
#'   `long`, doubles as `double`). Mainly useful for comparison.
#' @return `data`, invisibly.
#' @seealso [dta_types()] to preview the storage types without writing, and
#'   [describe_dta()] to inspect the types recorded in an existing file.
#' @examples
#' df <- data.frame(
#'   id = 1:5,
#'   score = c(1, 2, 3, 4, 5), # a double in R, but a byte in Stata
#'   name = c("a", "bb", "ccc", "", NA),
#'   when = as.Date("2024-01-01") + 0:4
#' )
#' path <- tempfile(fileext = ".dta")
#' write_dta(df, path)
#' describe_dta(path)
#' @export
write_dta <- function(data, path, version = 19, label = attr(data, "label", exact = TRUE),
                      compress = TRUE) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  fmt <- dta_format_for(version, ncol(data))
  write_dta_impl(data, path, fmt = fmt, label = label, compress = compress)
}

write_dta_impl <- function(data, path, fmt, label = NULL, compress = TRUE) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("`path` must be a single string", call. = FALSE)
  }
  if (!is.logical(compress) || length(compress) != 1L || is.na(compress)) {
    stop("`compress` must be TRUE or FALSE", call. = FALSE)
  }
  spec <- dta_spec(fmt)
  K <- ncol(data)
  N <- nrow(data)
  if (K < 1L) {
    stop("`data` must have at least one column", call. = FALSE)
  }
  if (K > spec$max_vars) {
    stop("Stata format ", fmt, " cannot hold more than ", spec$max_vars, " variables", call. = FALSE)
  }
  nms <- check_names(names(data), spec)
  dataset_label <- if (is.null(label) || (length(label) == 1L && is.na(label))) {
    ""
  } else {
    if (!is.character(label) || length(label) != 1L) {
      stop("`label` must be a single string", call. = FALSE)
    }
    truncate_label(enc2utf8(label), spec, "The dataset label")
  }

  cols <- lapply(seq_len(K), function(i) prepare_column(data[[i]], nms[i], compress, spec))

  codes <- vapply(cols, function(c) as.integer(c$code), 1L)
  widths <- vapply(cols, function(c) as.integer(c$width), 1L)
  types <- vapply(cols, function(c) TYPE_ID[[c$type]], 1L)
  formats <- vapply(cols, function(c) c$format, "")
  varlabels <- vapply(cols, function(c) c$label, "")
  has_labels <- !vapply(cols, function(c) is.null(c$value_labels), NA)
  lblnames <- ifelse(has_labels, nms, "")
  strl_bytes <- sum(vapply(cols, function(c) c$strl_bytes, 1))
  rowsize <- sum(as.double(widths))

  value_labels_raw <- build_value_labels(
    nms[has_labels], lapply(cols[has_labels], function(c) c$value_labels), spec
  )
  hdr <- build_header(
    spec, K, N, dataset_label, nms, codes, formats, lblnames, varlabels,
    rowsize, strl_bytes, value_labels_raw
  )
  tail <- c(value_labels_raw, charToRaw("</stata_dta>"))

  .Call(
    C_write_dta, path.expand(path), hdr$head, tail,
    lapply(cols, function(c) c$values), types, widths,
    lapply(cols, function(c) c$first), as.double(N), as.integer(fmt), hdr$eof
  )
  invisible(data)
}

#' Preview the Stata storage types `write_dta()` will use
#'
#' Runs the same type selection as [write_dta()] without writing anything.
#'
#' @inheritParams write_dta
#' @return A data frame with one row per column of `data`: `variable`,
#'   `class` (in R), `type` (Stata storage type such as `byte` or `str12`),
#'   `bytes` (per observation), `format`, `label`, and `value_labels`
#'   (the number of value labels, or 0).
#' @examples
#' dta_types(data.frame(x = c(1, 2, 3), y = c("a", "bb", "ccc"), z = c(TRUE, FALSE, NA)))
#' @export
dta_types <- function(data, compress = TRUE, version = 19) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame", call. = FALSE)
  }
  spec <- dta_spec(dta_format_for(version, ncol(data)))
  nms <- check_names(names(data), spec)
  cols <- lapply(seq_len(ncol(data)), function(i) prepare_column(data[[i]], nms[i], compress, spec))
  data.frame(
    variable = nms,
    class = vapply(data, function(x) class(x)[1L], ""),
    type = type_name(vapply(cols, function(c) as.integer(c$code), 1L)),
    bytes = vapply(cols, function(c) as.integer(c$width), 1L),
    format = vapply(cols, function(c) c$format, ""),
    label = vapply(cols, function(c) c$label, ""),
    value_labels = vapply(cols, function(c) length(c$value_labels$values), 1L),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}
