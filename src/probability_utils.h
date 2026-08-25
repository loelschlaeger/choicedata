#ifndef CHOICEDATA_PROBABILITY_UTILS_H
#define CHOICEDATA_PROBABILITY_UTILS_H

#include <Rcpp.h>

#include <vector>

namespace choicedata {

extern const R_xlen_t interrupt_size;

void check_interrupt(R_xlen_t i, R_xlen_t step = 1024);

double log_diff_exp(double x, double y);

double log_add_exp(double x, double y);

double log_sum_exp(const Rcpp::NumericVector& x,
                   const std::vector<bool>& use, bool interrupt = false);

double log_sum_exp(const std::vector<double>& x,
                   const std::vector<bool>& use, bool interrupt = false);

std::vector<bool> make_availability_mask(
  int size, const Rcpp::IntegerVector& available, bool use_all
);

}

#endif
