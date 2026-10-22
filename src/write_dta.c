/*
 * Writes the <data> and <strls> sections of a Stata .dta file (formats 117,
 * 118 and 119), sandwiched between header/tail bytes prepared in R.
 *
 * Layout reference: Stata's `help dta` (format 118), `help dta_117` and
 * `help dta_119`. Everything is written little-endian ("LSF").
 */
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

/* Storage-type ids shared with R/types.R (TYPE_ID). */
#define T_BYTE   1
#define T_INT    2
#define T_LONG   3
#define T_DOUBLE 5
#define T_STR    6
#define T_STRL   7

/*
 * haven's tagged NAs (Stata's extended missing values .a-.z) store the tag
 * letter in one byte of the NaN payload of R's NA_REAL. Mirror haven exactly
 * so `haven::tagged_na("a")` round-trips as `.a`.
 */
#ifdef WORDS_BIGENDIAN
#define TAG_BYTE 3
#else
#define TAG_BYTE 4
#endif

typedef union {
  double value;
  unsigned char byte[8];
} ieee_double;

/* 0 = ordinary value, 1 = sysmiss ".", 2..27 = ".a" .. ".z" */
static int missing_code(double x) {
  if (ISNAN(x)) {
    if (R_IsNA(x)) {
      ieee_double u;
      u.value = x;
      unsigned char tag = u.byte[TAG_BYTE];
      if (tag >= 'a' && tag <= 'z') return 2 + (tag - 'a');
    }
    return 1;
  }
  if (!R_FINITE(x)) return 1; /* Stata has no +/-Inf */
  return 0;
}

static void put_le(unsigned char *p, uint64_t v, int n) {
  for (int k = 0; k < n; k++) {
    p[k] = (unsigned char)(v & 0xFFu);
    v >>= 8;
  }
}

static void put_numeric(unsigned char *p, double x, int type) {
  int m = missing_code(x);
  switch (type) {
  case T_BYTE: {
    int v = m ? 100 + m : (int)(int64_t)x;
    p[0] = (unsigned char)(int8_t)v;
    break;
  }
  case T_INT: {
    int v = m ? 32740 + m : (int)(int64_t)x;
    put_le(p, (uint16_t)(int16_t)v, 2);
    break;
  }
  case T_LONG: {
    int64_t v = m ? 2147483620LL + m : (int64_t)x;
    put_le(p, (uint32_t)(int32_t)v, 4);
    break;
  }
  default: { /* T_DOUBLE */
    uint64_t bits;
    if (m) {
      /* "." is 0x7fe0000000000000; ".a".."z" add 1..26 in bits 40-47 */
      bits = 0x7fe0000000000000ULL + ((uint64_t)(m - 1) << 40);
    } else {
      memcpy(&bits, &x, 8);
    }
    put_le(p, bits, 8);
  }
  }
}

typedef struct {
  FILE *f;
  uint64_t written;
  const char *path;
} out_t;

static void out_write(out_t *o, const void *p, size_t n) {
  if (n == 0) return;
  if (fwrite(p, 1, n, o->f) != n) {
    int e = errno;
    fclose(o->f);
    o->f = NULL;
    Rf_error("Failed to write to '%s': %s", o->path, strerror(e));
  }
  o->written += n;
}

static double numeric_at(SEXP col, const void *ptr, R_xlen_t j) {
  switch (TYPEOF(col)) {
  case REALSXP:
    return ((const double *)ptr)[j];
  case INTSXP: {
    int v = ((const int *)ptr)[j];
    return v == NA_INTEGER ? NA_REAL : (double)v;
  }
  case LGLSXP: {
    int v = ((const int *)ptr)[j];
    return v == NA_LOGICAL ? NA_REAL : (double)v;
  }
  default:
    return NA_REAL;
  }
}

/*
 * path          character(1): output file
 * head          raw: everything from "<stata_dta>" through "<data>"
 * tail          raw: "<value_labels>...</value_labels></stata_dta>"
 * cols          list of K vectors (double/integer/logical/character)
 * types         integer K: T_* ids above
 * widths        integer K: bytes per cell (1, 2, 4, 8, str width, 8 for strL)
 * strl_first    list of K: for strL columns, 1-based index of the first
 *               occurrence of each value (0 for "" / NA); NULL otherwise
 * nobs          double(1): number of rows
 * fmt           integer(1): 117, 118 or 119
 * expected_size double(1): total bytes the R side computed for the map
 */
SEXP C_write_dta(SEXP path, SEXP head, SEXP tail, SEXP cols, SEXP types,
                 SEXP widths, SEXP strl_first, SEXP nobs, SEXP fmt,
                 SEXP expected_size) {
  const char *cpath = R_ExpandFileName(Rf_translateChar(STRING_ELT(path, 0)));
  const int K = Rf_length(cols);
  const R_xlen_t N = (R_xlen_t)REAL(nobs)[0];
  const int format = INTEGER(fmt)[0];
  /* (v,o) packing inside the 8-byte data cell: 4+4 (117), 2+6 (118), 3+5 (119) */
  const int vlen = format == 117 ? 4 : (format == 118 ? 2 : 3);
  /* o field width in a GSO header: 4 (117) or 8 (118/119) */
  const int gso_olen = format == 117 ? 4 : 8;
  const int *type = INTEGER(types);
  const int *width = INTEGER(widths);

  size_t rowsize = 0;
  int n_strl = 0;
  const void **ptr = (const void **)R_alloc(K > 0 ? K : 1, sizeof(void *));
  const int **first = (const int **)R_alloc(K > 0 ? K : 1, sizeof(int *));
  for (int i = 0; i < K; i++) {
    SEXP col = VECTOR_ELT(cols, i);
    rowsize += (size_t)width[i];
    ptr[i] = NULL;
    first[i] = NULL;
    switch (type[i]) {
    case T_STR:
      if (TYPEOF(col) != STRSXP) Rf_error("internal error: column %d is not character", i + 1);
      break;
    case T_STRL:
      if (TYPEOF(col) != STRSXP) Rf_error("internal error: column %d is not character", i + 1);
      first[i] = INTEGER(VECTOR_ELT(strl_first, i));
      n_strl++;
      break;
    default:
      if (TYPEOF(col) == REALSXP) ptr[i] = REAL(col);
      else if (TYPEOF(col) == INTSXP) ptr[i] = INTEGER(col);
      else if (TYPEOF(col) == LGLSXP) ptr[i] = LOGICAL(col);
      else Rf_error("internal error: column %d is not numeric", i + 1);
    }
  }

  out_t o;
  o.f = fopen(cpath, "wb");
  if (o.f == NULL) Rf_error("Cannot open '%s' for writing: %s", cpath, strerror(errno));
  o.written = 0;
  o.path = cpath;
  setvbuf(o.f, NULL, _IOFBF, 1 << 20);

  out_write(&o, RAW(head), (size_t)XLENGTH(head));

  /* ---- <data>: observations, row-major ---- */
  R_xlen_t chunk_rows = rowsize ? (R_xlen_t)((1 << 20) / rowsize) : 1;
  if (chunk_rows < 1) chunk_rows = 1;
  unsigned char *buf = (unsigned char *)R_alloc((size_t)chunk_rows * rowsize + 1, 1);

  R_xlen_t j = 0;
  while (j < N) {
    R_xlen_t nrows = (N - j < chunk_rows) ? N - j : chunk_rows;
    unsigned char *p = buf;
    for (R_xlen_t r = 0; r < nrows; r++, j++) {
      for (int i = 0; i < K; i++) {
        SEXP col = VECTOR_ELT(cols, i);
        const int w = width[i];
        switch (type[i]) {
        case T_STR: {
          SEXP s = STRING_ELT(col, j);
          if (s == NA_STRING) {
            memset(p, 0, (size_t)w);
          } else {
            size_t n = (size_t)LENGTH(s);
            if (n > (size_t)w) n = (size_t)w;
            memcpy(p, CHAR(s), n);
            if (n < (size_t)w) memset(p + n, 0, (size_t)w - n);
          }
          break;
        }
        case T_STRL: {
          int f = first[i][j];
          if (f > 0) {
            put_le(p, (uint64_t)(i + 1), vlen);
            put_le(p + vlen, (uint64_t)f, 8 - vlen);
          } else {
            memset(p, 0, 8); /* (0,0): the predefined empty string */
          }
          break;
        }
        default:
          put_numeric(p, numeric_at(col, ptr[i], j), type[i]);
        }
        p += w;
      }
    }
    out_write(&o, buf, (size_t)(p - buf));
  }

  /* ---- <strls>: one GSO per first occurrence, in observation-major order ---- */
  out_write(&o, "</data><strls>", 14);
  if (n_strl > 0) {
    unsigned char hdr[3 + 4 + 8 + 1 + 4];
    for (j = 0; j < N; j++) {
      for (int i = 0; i < K; i++) {
        if (type[i] != T_STRL || first[i][j] != j + 1) continue;
        SEXP s = STRING_ELT(VECTOR_ELT(cols, i), j);
        uint64_t len = (uint64_t)LENGTH(s) + 1; /* t == 130: includes the trailing \0 */
        unsigned char *h = hdr;
        memcpy(h, "GSO", 3);
        h += 3;
        put_le(h, (uint64_t)(i + 1), 4);
        h += 4;
        put_le(h, (uint64_t)(j + 1), gso_olen);
        h += gso_olen;
        *h++ = 130;
        put_le(h, len, 4);
        h += 4;
        out_write(&o, hdr, (size_t)(h - hdr));
        out_write(&o, CHAR(s), (size_t)(len - 1));
        out_write(&o, "", 1);
      }
    }
  }
  out_write(&o, "</strls>", 8);

  out_write(&o, RAW(tail), (size_t)XLENGTH(tail));

  if (fclose(o.f) != 0) {
    o.f = NULL;
    Rf_error("Failed to close '%s': %s", cpath, strerror(errno));
  }
  double expected = REAL(expected_size)[0];
  if ((double)o.written != expected) {
    Rf_error("internal error: wrote %.0f bytes but the file map expected %.0f",
             (double)o.written, expected);
  }
  return R_NilValue;
}

/* Tag letter of haven-style tagged NAs ("" for anything else). */
SEXP C_na_tag(SEXP x) {
  R_xlen_t n = XLENGTH(x);
  const double *px = REAL(x);
  SEXP out = PROTECT(Rf_allocVector(STRSXP, n));
  char s[2] = {0, 0};
  for (R_xlen_t i = 0; i < n; i++) {
    int m = missing_code(px[i]);
    s[0] = m >= 2 ? (char)('a' + (m - 2)) : '\0';
    SET_STRING_ELT(out, i, Rf_mkChar(s));
  }
  UNPROTECT(1);
  return out;
}

/* Build a haven-compatible tagged NA from a single letter. */
SEXP C_tagged_na(SEXP tags) {
  R_xlen_t n = XLENGTH(tags);
  SEXP out = PROTECT(Rf_allocVector(REALSXP, n));
  for (R_xlen_t i = 0; i < n; i++) {
    SEXP s = STRING_ELT(tags, i);
    ieee_double u;
    u.value = NA_REAL;
    if (s != NA_STRING && LENGTH(s) == 1 && CHAR(s)[0] >= 'a' && CHAR(s)[0] <= 'z') {
      u.byte[TAG_BYTE] = (unsigned char)CHAR(s)[0];
    }
    REAL(out)[i] = u.value;
  }
  UNPROTECT(1);
  return out;
}
