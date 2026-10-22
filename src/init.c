#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP C_write_dta(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_na_tag(SEXP);
extern SEXP C_tagged_na(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_write_dta", (DL_FUNC)&C_write_dta, 10},
    {"C_na_tag", (DL_FUNC)&C_na_tag, 1},
    {"C_tagged_na", (DL_FUNC)&C_tagged_na, 1},
    {NULL, NULL, 0}};

void R_init_smalldta(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
