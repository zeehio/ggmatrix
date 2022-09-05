/*
 * Copyright 2022 mikefc <mikefc@coolbutuseless.com>
 *
 * The contents of this file are extracted from the nara package
 * https://github.com/coolbutuseless/nara
 * It is re-distributed under the MIT license
 */
#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>

SEXP colour_to_integer_(SEXP);

static const R_CallMethodDef CEntries[] = {

  {"colour_to_integer_"  , (DL_FUNC) &colour_to_integer_  , 1},
  {NULL , NULL, 0}
};


void R_init_ggmatrix(DllInfo *info) {
  R_registerRoutines(
    info,      // DllInfo
    NULL,      // .C
    CEntries,  // .Call
    NULL,      // Fortran
    NULL       // External
  );
  R_useDynamicSymbols(info, FALSE);
}
