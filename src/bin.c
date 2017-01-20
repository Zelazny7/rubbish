#include "R.h"
#include "stdio.h"
#include "variable.h"
#include "queue.h"
#include "xtab.h"
#include "bin.h"

#define RETURN_R

// called from R and handles passing of data to and from
SEXP bin(SEXP x, SEXP y, SEXP wt, SEXP miniv, SEXP mincnt, SEXP minres, SEXP maxbin, SEXP mono, SEXP except) {

  struct variable* v = variable_factory(REAL(x), LENGTH(x));
  //print_variable(v);

  struct xtab* xtab = xtab_factory(v, REAL(y), REAL(wt)); // create the xtab
  double* grand_tots = get_xtab_totals(xtab, 0, xtab->size);

  struct queue* q = queue_factory(); // create the queue
  struct work w = {0, xtab->size - 1}; // last index is one less than the size
  enqueue(q, w);

  // create a vector to store the split rows and init to zero
  size_t* breaks = calloc(xtab->size, sizeof(size_t));
  int num_bins = 1;

  // fille options structure
  struct opts o = {*REAL(miniv), *INTEGER(mincnt), *INTEGER(minres), *INTEGER(maxbin), *INTEGER(mono), except};

  // bin the variable until it's done
  while(!is_empty(q)) {
    struct work w = dequeue(q); // take work from queue
    size_t split = find_best_split(w.start, w.stop, xtab, grand_tots, &o);

    if ((split != -1) & (num_bins < o.max_bin)) { // split found!
      num_bins++;
      breaks[split] = 1; // update breaks array
      struct work w1 = {w.start, split};
      struct work w2 = {split + 1, w.stop};
      enqueue(q, w1); // add work to queue
      enqueue(q, w2);
    }
  }

  // return breaks in an R object
#ifdef RETURN_R
  SEXP r_brk = PROTECT(allocVector(REALSXP, num_bins + 1));
  size_t j = 0;
  REAL(r_brk)[0] = R_NegInf;
  for(size_t i = 0; i < xtab->size; i++) {
    if (breaks[i] == 1) {
      j++;
      REAL(r_brk)[j] = (xtab->values[i] + xtab->values[i+1]) / 2;
    }
  }
  REAL(r_brk)[j + 1] = R_PosInf;
#endif

  // Release resources
  release_variable(v);
  release_xtab(xtab);
  release_queue(q);
  free(breaks);
  free(grand_tots);

#ifdef RETURN_R
  UNPROTECT(1);
  return r_brk;
#endif

  return R_NilValue;
}

size_t find_best_split(int start, int stop, struct xtab* xtab, double* grand_tot, struct opts* o) {

  //double* tot = get_xtab_totals(xtab, start, stop + 1);
  double tot[2] = {0};
  double asc[2] = {0}, dsc[2] = {0};
  double best_iv = -1;
  int valid = 0;
  size_t best_split_idx = -1;

  // need totals without exceptions
  for (size_t i = start; i <= stop; i++) {
    int skip = 0;
    if (LENGTH(o->except) > 0) {
      for (size_t j = 0; j < LENGTH(o->except); j++){
        if (xtab->values[i] == REAL(o->except)[j]) skip = 1;
      }
    }

    if (!skip) {
      tot[0] += xtab->zero_ct[i];
      tot[1] += xtab->ones_ct[i];
    }
  }


  // now get cumulative counts
  for (size_t i = start; i <= stop; i++) {
    valid = 0;

    int skip = 0;
    if (LENGTH(o->except) > 0) {
      for (size_t j = 0; j < LENGTH(o->except); j++){
        // Rprintf("exception: %f\n", REAL(o.except)[j]);
        if (xtab->values[i] == REAL(o->except)[j]) skip = 1;
      }
    }

    if (!skip) {
      asc[0] += xtab->zero_ct[i];
      asc[1] += xtab->ones_ct[i];
      dsc[0] = tot[0] - asc[0];
      dsc[1] = tot[1] - asc[1];
    }

    struct iv iv = calc_iv(asc, dsc, grand_tot);
    int woe_sign = (iv.asc_woe > iv.dsc_woe) ? 1 : -1;

    if ((asc[0] + asc[1]) < o->min_cnt) { // minsplit
      valid = -1;
    } else if ((dsc[0] + dsc[1]) < o->min_cnt) { // minsplit
      valid = -1;
    } else if (isinf(iv.iv) | isnan(iv.iv)) { // infinite or nan iv
      valid = -1;
    } else if (iv.iv < o->min_iv) { // min iv
      valid = -1;
    } else if ((asc[1] < o->min_res) | (dsc[1] < o->min_res))  {
      valid = -1;
    } else if ((o->mono == 1) | (o->mono == -1)) {
      if (woe_sign != o->mono) {
        valid = -1;
      }
    }

    if ((valid != -1) & (iv.iv > best_iv)) {
      best_iv = iv.iv;
      best_split_idx = i;
      if (o->mono == 2) o->mono = woe_sign;
    }
  }

  //free(tot);
  return best_split_idx;
}

struct iv calc_iv(double* asc_cnts, double* dsc_cnts, double* tots) {
  struct iv iv = {0};
  iv.asc_woe = log((asc_cnts[0]/tots[0])/(asc_cnts[1]/tots[1]));
  iv.dsc_woe = log((dsc_cnts[0]/tots[0])/(dsc_cnts[1]/tots[1]));

  iv.asc_iv  = iv.asc_woe * (asc_cnts[0]/tots[0] - asc_cnts[1]/tots[1]);
  iv.dsc_iv  = iv.dsc_woe * (dsc_cnts[0]/tots[0] - dsc_cnts[1]/tots[1]);
  iv.iv = iv.asc_iv + iv.dsc_iv;

  return iv;
}
