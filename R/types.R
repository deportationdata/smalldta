# Stata storage-type codes as recorded in <variable_types> (help dta, 5.3)
DTA_STRL <- 32768L
DTA_DOUBLE <- 65526L
DTA_FLOAT <- 65527L
DTA_LONG <- 65528L
DTA_INT <- 65529L
DTA_BYTE <- 65530L

STATA_MAX_STR <- 2045L

# Internal ids shared with src/write_dta.c
TYPE_ID <- c(byte = 1L, int = 2L, long = 3L, double = 5L, str = 6L, strL = 7L)
TYPE_CODE <- c(byte = DTA_BYTE, int = DTA_INT, long = DTA_LONG, double = DTA_DOUBLE, strL = DTA_STRL)
TYPE_WIDTH <- c(byte = 1L, int = 2L, long = 4L, double = 8L, strL = 8L)

# Days / milliseconds between 1960-01-01 (Stata epoch) and 1970-01-01 (R epoch)
STATA_EPOCH_DAYS <- 3653
STATA_EPOCH_MS <- 315619200000

`%||%` <- function(x, y) if (is.null(x)) y else x

# Per-format field widths (help dta, dta_117, dta_119).
dta_spec <- function(fmt) {
  switch(as.character(fmt),
    "117" = list(
      fmt = 117L, k_bytes = 2L, n_bytes = 4L, label_len_bytes = 1L,
      name_bytes = 33L, sort_bytes = 2L, format_bytes = 49L,
      lblname_bytes = 33L, varlabel_bytes = 81L, gso_o_bytes = 4L,
      max_vars = 32767L, unicode = FALSE
    ),
    "118" = list(
      fmt = 118L, k_bytes = 2L, n_bytes = 8L, label_len_bytes = 2L,
      name_bytes = 129L, sort_bytes = 2L, format_bytes = 57L,
      lblname_bytes = 129L, varlabel_bytes = 321L, gso_o_bytes = 8L,
      max_vars = 32767L, unicode = TRUE
    ),
    "119" = list(
      fmt = 119L, k_bytes = 4L, n_bytes = 8L, label_len_bytes = 2L,
      name_bytes = 129L, sort_bytes = 4L, format_bytes = 57L,
      lblname_bytes = 129L, varlabel_bytes = 321L, gso_o_bytes = 8L,
      max_vars = 120000L, unicode = TRUE
    ),
    stop("Unsupported .dta format ", fmt, call. = FALSE)
  )
}

# Map a Stata version (13-19) to the .dta format Stata itself would write.
dta_format_for <- function(version, nvar) {
  if (!is.numeric(version) || length(version) != 1L || is.na(version) ||
    !(version %in% 13:19)) {
    stop("`version` must be a Stata version between 13 and 19", call. = FALSE)
  }
  if (version == 13) {
    if (nvar > 32767L) {
      stop("Stata 13 cannot read datasets with more than 32,767 variables", call. = FALSE)
    }
    return(117L)
  }
  if (nvar > 32767L) {
    if (version == 14) {
      stop("Stata 14 cannot read datasets with more than 32,767 variables; ",
        "use `version = 15` or later (format 119)",
        call. = FALSE
      )
    }
    return(119L)
  }
  118L
}

# The smallest integer storage type whose non-missing range covers [lo, hi]
# (help dta, section 4: byte -127..100, int -32767..32740,
# long -2147483647..2147483620). Values outside long stay double.
smallest_integer_type <- function(lo, hi) {
  if (lo >= -127 && hi <= 100) {
    "byte"
  } else if (lo >= -32767 && hi <= 32740) {
    "int"
  } else if (lo >= -2147483647 && hi <= 2147483620) {
    "long"
  } else {
    "double"
  }
}

# Same demotions as Stata's `compress`: doubles to long/int/byte when every
# non-missing value is a whole number in range; never to float.
choose_numeric_type <- function(x, compress) {
  if (is.logical(x)) {
    return("byte")
  }
  if (is.integer(x)) {
    finite <- x[!is.na(x)]
    if (!length(finite)) {
      return(if (compress) "byte" else "long")
    }
    r <- range(finite)
    if (!compress) {
      return(if (r[2L] > 2147483620) "double" else "long")
    }
    return(smallest_integer_type(r[1L], r[2L]))
  }
  # double
  if (!compress) {
    return("double")
  }
  finite <- x[is.finite(x)]
  if (!length(finite)) {
    return("byte")
  }
  if (any(finite != trunc(finite))) {
    return("double")
  }
  r <- range(finite)
  smallest_integer_type(r[1L], r[2L])
}

# str# sized to the longest UTF-8 byte length; strL beyond 2045 bytes.
choose_string_type <- function(nbytes) {
  w <- if (length(nbytes)) max(nbytes) else 0L
  if (w > STATA_MAX_STR) {
    list(type = "strL", code = DTA_STRL, width = 8L)
  } else {
    w <- max(w, 1L)
    list(type = "str", code = w, width = w)
  }
}

default_format <- function(type, width) {
  switch(type,
    byte = "%8.0g",
    int = "%8.0g",
    long = "%12.0g",
    double = "%10.0g",
    str = sprintf("%%%ds", max(9L, width)),
    strL = "%9s"
  )
}

type_name <- function(code) {
  ifelse(code >= 1L & code <= STATA_MAX_STR, paste0("str", code),
    ifelse(code == DTA_STRL, "strL",
      ifelse(code == DTA_DOUBLE, "double",
        ifelse(code == DTA_FLOAT, "float",
          ifelse(code == DTA_LONG, "long",
            ifelse(code == DTA_INT, "int",
              ifelse(code == DTA_BYTE, "byte", NA_character_)
            )
          )
        )
      )
    )
  )
}

na_tag <- function(x) .Call(C_na_tag, as.double(x))

#' Extended missing values (`.a` to `.z`)
#'
#' Creates "tagged" missing values that are written to Stata as the extended
#' missing values `.a`, `.b`, ..., `.z`. These are bit-compatible with
#' `haven::tagged_na()`, so data read with haven round-trips unchanged.
#'
#' @param ... Single lowercase letters, `"a"` to `"z"`.
#' @return A double vector of `NA`s carrying the given tags.
#' @examples
#' x <- c(1, tagged_na("a"), NA)
#' is.na(x)
#' @export
tagged_na <- function(...) {
  tags <- as.character(c(...))
  bad <- !grepl("^[a-z]$", tags)
  if (any(bad)) {
    stop("Tags must be single lowercase letters a-z", call. = FALSE)
  }
  .Call(C_tagged_na, tags)
}
