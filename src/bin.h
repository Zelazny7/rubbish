#ifndef BIN_H
#define BIN_H

#include "R.h"
#include "variable.h"
#include "queue.h"
#include "xtab.h"

#define RETURN_R

struct iv {
  double asc_woe;
  double dsc_woe;
  double asc_iv;
  double dsc_iv;
  double iv;
};

struct opts {
  double min_iv;
  int min_cnt;
  int min_res;
  int max_bin;
  int mono;
  SEXP except;
};

// prototype for main working function
size_t find_best_split(int start, int stop, struct xtab* xtab, double* grand_tot, struct opts* opts);

SEXP bin(SEXP x, SEXP y, SEXP wt, SEXP miniv, SEXP mincnt, SEXP minres, SEXP maxbin, SEXP monotonicity, SEXP except);

struct iv calc_iv(double* asc_cnts, double* dsc_cnts, double* tots);

double calc_sv_woe(double* dx, double* dy, int size, double value, double* tots);

#endif
