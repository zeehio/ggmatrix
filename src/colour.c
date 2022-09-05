/*
 * Copyright 2022 mikefc <mikefc@coolbutuseless.com>
 *
 * The contents of this file are extracted from the nara package
 * https://github.com/coolbutuseless/nara
 *
 * This file is distributed under the MIT license terms.
 */
#include <R.h>
#include <Rinternals.h>

int rcolour_to_int(const char *rcol);


// Reverse byte ordering in an int
#define reverse_bytes_32(num) ( ((num & 0xFF000000) >> 24) | ((num & 0x00FF0000) >> 8) | ((num & 0x0000FF00) << 8) | ((num & 0x000000FF) << 24) )


int colour_to_integer(SEXP colour_) {

  const char *colour;

  if ((isLogical(colour_) && asInteger(colour_) == NA_LOGICAL)) {
    return(0);  //  '#00000000'
  } else if (isString(colour_) && asChar(colour_) == NA_STRING) {
    return(0);
  } else if (colour_ == NA_STRING) {
    return(0);
  } else if (isInteger(colour_)) {
    return(asInteger(colour_));
  } else if (isReal(colour_)) {
    return(asInteger(colour_));
  } else if (TYPEOF(colour_) == STRSXP ) {
    colour = CHAR(asChar(colour_));
  } else if (TYPEOF(colour_) == CHARSXP) {
    colour = CHAR(colour_);
  } else {
    error(">> Colour must be string or integer not %s", type2char(TYPEOF(colour_)));
  }

  if (colour[0] == '#') {
    // Rprintf("Hex colour\n");
    int n = strlen(colour);

    if (n != 7 && n != 9) {
      error("This doesn't seem like a colour: %s", colour);
    }

    unsigned long res = strtoul(colour+1,  NULL, 16);

    if (n == 7) {
      res <<= 8;
      res += 255;
    }

    return (int)reverse_bytes_32(res);
  } else {
    // Rprintf("Unknown colour: %s\n", colour);
    return rcolour_to_int(colour) ;
  }
}



//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SEXP colour_to_integer_(SEXP colour_) {

  if (length(colour_) == 1) {
    if ((isLogical(colour_) && asInteger(colour_) == NA_LOGICAL)) {
      return(ScalarInteger(0));  //  '#00000000'
    } else if (isString(colour_) && asChar(colour_) == NA_STRING) {
      return(ScalarInteger(0));  //  '#00000000'
    }
  }

  if (isReal(colour_)) {
    SEXP res_ = PROTECT(allocVector(INTSXP, length(colour_)));

    for (int i = 0; i < length(colour_); i++) {
      INTEGER(res_)[i] = (int)REAL(colour_)[i];
    }

    UNPROTECT(1);
    return res_;
  } else if (isInteger(colour_)) {
    return(colour_);
  } else if (TYPEOF(colour_) != STRSXP) {
    error("Colours must be type '%s' or '%s' not '%s'", type2char(STRSXP), type2char(INTSXP), type2char(TYPEOF(colour_)));
  }

  SEXP res_ = PROTECT(allocVector(INTSXP, length(colour_)));

  for (int i = 0; i < length(colour_); i++) {
    INTEGER(res_)[i] = colour_to_integer(STRING_ELT(colour_, i));
  }

  UNPROTECT(1);
  return res_;
}
