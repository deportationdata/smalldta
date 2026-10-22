# Convert one R column into what the writer needs: a base vector, a Stata
# storage type, display format, variable label and (optional) value labels.
prepare_column <- function(x, name, compress, spec) {
  label <- variable_label(x, name, spec)
  format <- attr(x, "format.stata", exact = TRUE)
  value_labels <- NULL

  if (inherits(x, "haven_labelled")) {
    value_labels <- attr(x, "labels", exact = TRUE)
  }

  if (is.factor(x)) {
    lv <- enc2utf8(levels(x))
    value_labels <- stats::setNames(seq_along(lv), lv)
    x <- as.integer(x)
  } else if (inherits(x, "Date")) {
    x <- as.double(x) + STATA_EPOCH_DAYS
    format <- format %||% "%td"
  } else if (inherits(x, "POSIXct")) {
    x <- round(as.double(x) * 1000) + STATA_EPOCH_MS
    format <- format %||% "%tc"
  } else if (inherits(x, "hms")) {
    x <- round(as.double(x) * 1000)
    format <- format %||% "%tcHH:MM:SS"
  } else if (inherits(x, "difftime") || inherits(x, "integer64")) {
    x <- as.double(x)
  }
  x <- strip_attributes(x)

  if (is.character(x)) {
    if (!is.null(value_labels)) {
      warning("Stata cannot attach value labels to a string variable; ",
        "dropping the labels of `", name, "`",
        call. = FALSE
      )
      value_labels <- NULL
    }
    x <- enc2utf8(x)
    nbytes <- nchar(x, type = "bytes")
    nbytes[is.na(x)] <- 0L
    st <- choose_string_type(nbytes)
    first <- NULL
    strl_bytes <- 0
    if (st$type == "strL") {
      first <- match(x, x)
      first[is.na(x) | x == ""] <- 0L
      emit <- first == seq_along(first)
      # GSO = "GSO" + v(4) + o(4|8) + t(1) + len(4) + contents + "\0"
      strl_bytes <- sum(emit) * (3 + 4 + spec$gso_o_bytes + 1 + 4 + 1) + sum(as.double(nbytes[emit]))
    }
    return(list(
      values = x, type = st$type, code = st$code, width = st$width,
      format = check_format(format %||% default_format(st$type, st$width), name, spec),
      label = label, value_labels = NULL, first = first, strl_bytes = strl_bytes
    ))
  }

  if (!(is.logical(x) || is.integer(x) || is.double(x))) {
    stop("Column `", name, "` has type ", typeof(x),
      " (class ", paste(class(x), collapse = "/"), "), which cannot be written to Stata",
      call. = FALSE
    )
  }
  type <- choose_numeric_type(x, compress)
  list(
    values = x, type = type, code = TYPE_CODE[[type]], width = TYPE_WIDTH[[type]],
    format = check_format(format %||% default_format(type, NULL), name, spec),
    label = label, value_labels = prepare_value_labels(value_labels, name),
    first = NULL, strl_bytes = 0
  )
}

strip_attributes <- function(x) {
  attributes(x) <- NULL
  x
}

variable_label <- function(x, name, spec) {
  l <- attr(x, "label", exact = TRUE)
  if (is.null(l) || (length(l) == 1L && is.na(l))) {
    return("")
  }
  if (!is.character(l) || length(l) != 1L) {
    stop("The `label` attribute of `", name, "` must be a single string", call. = FALSE)
  }
  truncate_label(enc2utf8(l), spec, sprintf("The label of `%s`", name))
}

# Stata labels: at most 80 characters (UTF-8, so up to 320 bytes) in 118+,
# 80 bytes in 117.
truncate_label <- function(l, spec, what) {
  max_bytes <- if (spec$unicode) 320L else 80L
  if (nchar(l, type = "chars") > 80L || nchar(l, type = "bytes") > max_bytes) {
    l <- substr(l, 1L, 80L)
    while (nchar(l, type = "bytes") > max_bytes) {
      l <- substr(l, 1L, nchar(l, type = "chars") - 1L)
    }
    warning(what, " was truncated to 80 characters", call. = FALSE)
  }
  l
}

check_format <- function(format, name, spec) {
  if (!is.character(format) || length(format) != 1L || is.na(format) ||
    !startsWith(format, "%")) {
    stop("The `format.stata` attribute of `", name, "` must be a single Stata display format such as \"%9.0g\"",
      call. = FALSE
    )
  }
  format <- enc2utf8(format)
  if (nchar(format, type = "bytes") >= spec$format_bytes) {
    stop("The display format of `", name, "` is too long for Stata", call. = FALSE)
  }
  format
}

# Value labels as a haven-style named vector -> list(values = int, text = chr),
# sorted by value. Stata stores label values as 4-byte integers, with .a-.z
# encoded as 2147483622-2147483647; sysmiss cannot be labelled.
prepare_value_labels <- function(labels, name) {
  if (is.null(labels) || !length(labels)) {
    return(NULL)
  }
  text <- names(labels)
  if (is.null(text) || anyNA(text)) {
    stop("The value labels of `", name, "` must be a named vector", call. = FALSE)
  }
  if (!is.numeric(labels)) {
    warning("Value labels of `", name, "` are not numeric and were dropped", call. = FALSE)
    return(NULL)
  }
  vals <- as.double(unname(labels))
  code <- rep(NA_real_, length(vals))
  tags <- na_tag(vals)
  tagged <- is.na(vals) & tags != ""
  code[tagged] <- 2147483621 + match(tags[tagged], letters)
  ok <- !is.na(vals) & is.finite(vals) & vals == trunc(vals) &
    vals >= -2147483647 & vals <= 2147483620
  code[ok] <- vals[ok]
  bad <- is.na(code)
  if (any(bad)) {
    warning("Dropping ", sum(bad), " value label(s) of `", name,
      "` whose values Stata cannot label (non-integer, out of range, or plain NA): ",
      paste(utils::head(text[bad], 5L), collapse = ", "),
      call. = FALSE
    )
    code <- code[!bad]
    text <- text[!bad]
  }
  if (!length(code)) {
    return(NULL)
  }
  if (anyDuplicated(code)) {
    stop("The value labels of `", name, "` label the same value more than once", call. = FALSE)
  }
  ord <- order(code)
  list(values = code[ord], text = enc2utf8(text[ord]))
}

STATA_RESERVED <- c(
  "_all", "_b", "byte", "_coef", "_cons", "double", "float", "if", "in",
  "int", "long", "_n", "_N", "_pi", "_pred", "_rc", "_skip", "strL",
  "using", "with"
)

check_names <- function(nms, spec) {
  if (is.null(nms) || anyNA(nms) || any(nms == "")) {
    stop("Every column must be named", call. = FALSE)
  }
  dup <- unique(nms[duplicated(nms)])
  if (length(dup)) {
    stop("Duplicate column names: ", paste(dup, collapse = ", "), call. = FALSE)
  }
  nms <- enc2utf8(nms)
  pattern <- if (spec$unicode) {
    "^[\\p{L}_][\\p{L}\\p{M}0-9_]*$"
  } else {
    "^[A-Za-z_][A-Za-z0-9_]*$"
  }
  bad <- !grepl(pattern, nms, perl = TRUE) |
    nchar(nms, type = "chars") > 32L |
    nchar(nms, type = "bytes") >= spec$name_bytes |
    nms %in% STATA_RESERVED |
    grepl("^str[0-9]+$", nms)
  if (any(bad)) {
    stop("Invalid Stata variable name(s): ", paste(nms[bad], collapse = ", "),
      "\nNames must be 1-32 characters, start with a letter or underscore, ",
      "contain only letters, digits and underscores, and not be Stata reserved words.",
      call. = FALSE
    )
  }
  nms
}
