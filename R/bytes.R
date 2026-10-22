# Little-endian encoding of whole numbers held in doubles (exact below 2^53).
# Negative values are written in two's complement for the given width.
raw_le <- function(x, n) {
  x <- as.double(x)
  neg <- x < 0
  x[neg] <- x[neg] + 2^(8 * n)
  out <- raw(n * length(x))
  if (!length(x)) {
    return(out)
  }
  for (k in seq_len(n)) {
    out[seq(k, by = n, length.out = length(x))] <- as.raw(x %% 256)
    x <- x %/% 256
  }
  out
}

# Fixed-width, \0-terminated UTF-8 string array (varnames, formats, labels...).
raw_fixed <- function(s, width) {
  bytes <- lapply(s, charToRaw)
  n <- lengths(bytes)
  if (any(n >= width)) {
    stop("internal error: string does not fit in a ", width, "-byte field", call. = FALSE)
  }
  out <- raw(width * length(s))
  if (sum(n) > 0) {
    out[rep((seq_along(s) - 1) * width, n) + sequence(n)] <- unlist(bytes, use.names = FALSE)
  }
  out
}

tag <- function(name, body = raw(0)) {
  c(charToRaw(paste0("<", name, ">")), body, charToRaw(paste0("</", name, ">")))
}

# "dd Mon yyyy hh:mm", always English month names (help dta, 5.1.6)
dta_timestamp <- function(time = Sys.time()) {
  lt <- as.POSIXlt(time)
  sprintf(
    "%02d %s %04d %02d:%02d", lt$mday, month.abb[lt$mon + 1L],
    lt$year + 1900L, lt$hour, lt$min
  )
}

# Bytes from "<stata_dta>" through "<characteristics></characteristics>",
# with the <map> filled in from the sizes of the data/strls/value-label parts.
build_header <- function(spec, K, N, dataset_label, names, codes, formats,
                         lblnames, varlabels, rowsize, strl_bytes, value_labels_raw) {
  label_bytes <- charToRaw(dataset_label)
  ts <- dta_timestamp()
  header <- c(
    charToRaw("<stata_dta>"),
    tag("header", c(
      tag("release", charToRaw(as.character(spec$fmt))),
      tag("byteorder", charToRaw("LSF")),
      tag("K", raw_le(K, spec$k_bytes)),
      tag("N", raw_le(N, spec$n_bytes)),
      tag("label", c(raw_le(length(label_bytes), spec$label_len_bytes), label_bytes)),
      tag("timestamp", c(as.raw(17L), charToRaw(ts)))
    ))
  )
  sections <- list(
    header,
    tag("map", raw(14 * 8)),
    tag("variable_types", raw_le(codes, 2L)),
    tag("varnames", raw_fixed(names, spec$name_bytes)),
    tag("sortlist", raw((K + 1) * spec$sort_bytes)),
    tag("formats", raw_fixed(formats, spec$format_bytes)),
    tag("value_label_names", raw_fixed(lblnames, spec$lblname_bytes)),
    tag("variable_labels", raw_fixed(varlabels, spec$varlabel_bytes)),
    tag("characteristics")
  )
  starts <- cumsum(c(0, vapply(sections, length, 1L)))
  data_start <- starts[10L]
  strls_start <- data_start + 6 + N * rowsize + 7 # "<data>" rows "</data>"
  vl_start <- strls_start + 7 + strl_bytes + 8 # "<strls>" GSOs "</strls>"
  close_start <- vl_start + length(value_labels_raw)
  eof <- close_start + 12 # "</stata_dta>"
  map <- c(starts[1:10], strls_start, vl_start, close_start, eof)
  sections[[2L]] <- tag("map", raw_le(map, 8L))
  list(
    head = c(unlist(sections, use.names = FALSE), charToRaw("<data>")),
    eof = eof
  )
}

# <value_labels><lbl>...</lbl>...</value_labels> (help dta, 5.12)
build_value_labels <- function(lbl_names, lbl_defs, spec) {
  blocks <- Map(function(name, def) {
    text_bytes <- lapply(def$text, charToRaw)
    lens <- lengths(text_bytes) + 1L
    off <- cumsum(c(0L, lens[-length(lens)]))
    txt <- raw(sum(lens))
    txt[rep(off, lens - 1L) + sequence(lens - 1L)] <- unlist(text_bytes, use.names = FALSE)
    n <- length(def$values)
    table <- c(raw_le(n, 4L), raw_le(length(txt), 4L), raw_le(off, 4L), raw_le(def$values, 4L), txt)
    tag("lbl", c(raw_le(length(table), 4L), raw_fixed(name, spec$lblname_bytes), raw(3), table))
  }, lbl_names, lbl_defs)
  tag("value_labels", unlist(blocks, use.names = FALSE))
}
