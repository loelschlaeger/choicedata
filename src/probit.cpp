#include <RcppArmadillo.h>

#include "probability_utils.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;
using namespace choicedata;

// Compute cross-sectional ordered probit probabilities
// [[Rcpp::export]]
NumericVector cpp_ordered_probit(
    List x, Nullable<List> y, NumericVector beta, double sigma,
    NumericVector gamma, Nullable<NumericMatrix> omega = R_NilValue,
    Nullable<IntegerVector> position = R_NilValue, bool log = false) {
  R_xlen_t n = x.size();
  int alternatives = gamma.size() + 1;
  bool observed = y.isNotNull();
  bool mixed = omega.isNotNull();
  List response = observed ? as<List>(y) : List::create();
  if (observed && response.size() != n) {
    stop("Ordered choices must have one entry per observation.");
  }

  NumericMatrix omega_mat(0, 0);
  IntegerVector position_vec;
  if (mixed) {
    omega_mat = as<NumericMatrix>(omega);
    if (position.isNull()) {
      stop("Random effect positions are required with Omega.");
    }
    position_vec = as<IntegerVector>(position);
    if (omega_mat.nrow() != position_vec.size() ||
        omega_mat.ncol() != position_vec.size()) {
      stop("Omega must match random effect positions.");
    }
    for (int pos : position_vec) {
      if (pos < 1 || pos > beta.size()) {
        stop("Random effect positions must index beta.");
      }
    }
  }

  NumericVector out(observed ? n : n * alternatives);
  bool long_loop = out.size() >= interrupt_size;
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix design = x[obs];
    if (design.nrow() != 1 || design.ncol() != beta.size()) {
      stop("Ordered design matrices must have one row and match beta.");
    }
    double utility = 0;
    for (int p = 0; p < design.ncol(); ++p) {
      utility += design(0, p) * beta[p];
    }
    double variance = sigma;
    if (mixed) {
      for (R_xlen_t row = 0; row < position_vec.size(); ++row) {
        double x_row = design(0, position_vec[row] - 1);
        for (R_xlen_t col = 0; col < position_vec.size(); ++col) {
          variance += x_row * omega_mat(row, col) *
            design(0, position_vec[col] - 1);
        }
      }
    }
    double scale = std::sqrt(variance);
    IntegerVector choice;
    if (observed) {
      choice = response[obs];
      if (choice.size() == 0) {
        stop("Observed choices must not be empty.");
      }
    }
    int first = observed ? choice[0] - 1 : 0;
    int final = observed ? first + 1 : alternatives;
    if (first < 0 || final > alternatives) {
      stop("Ordered choices are outside the available categories.");
    }
    double row_total = 0;
    for (int j = first; j < final; ++j) {
      double lower = j == 0 ? R_NegInf : gamma[j - 1];
      double upper = j == alternatives - 1 ? R_PosInf : gamma[j];
      lower = (lower - utility) / scale;
      upper = (upper - utility) / scale;
      double probability;
      if (!log) {
        probability = lower > 0 ?
          R::pnorm(lower, 0, 1, false, false) -
            R::pnorm(upper, 0, 1, false, false) :
          R::pnorm(upper, 0, 1, true, false) -
            R::pnorm(lower, 0, 1, true, false);
        probability = std::max(probability, 0.0);
      } else {
        double log_large;
        double log_small;
        if (lower > 0) {
          log_large = R::pnorm(lower, 0, 1, false, true);
          log_small = R::pnorm(upper, 0, 1, false, true);
        } else {
          log_large = R::pnorm(upper, 0, 1, true, true);
          log_small = R::pnorm(lower, 0, 1, true, true);
        }
        probability = log_diff_exp(log_large, log_small);
        if (!std::isfinite(probability)) {
          probability = std::log(
            std::numeric_limits<double>::min()
          );
        }
      }
      R_xlen_t index = observed ? obs : obs + n * j;
      out[index] = probability;
      if (!observed && !log) {
        row_total += probability;
      }
    }
    if (!observed && !log && row_total > 0) {
      for (int j = 0; j < alternatives; ++j) {
        out[obs + n * j] /= row_total;
      }
    }
  }
  if (!observed) {
    out.attr("dim") = IntegerVector::create(n, alternatives);
  }
  return out;
}

// Compute binary probit probabilities from utility difference
// [[Rcpp::export]]
NumericVector cpp_binary_probit(
    List x, Nullable<List> y, NumericVector beta, NumericMatrix sigma,
    Nullable<NumericMatrix> omega = R_NilValue,
    Nullable<IntegerVector> position = R_NilValue, bool log = false,
    Nullable<List> availability = R_NilValue) {
  R_xlen_t n = x.size();
  bool observed = y.isNotNull();
  bool mixed = omega.isNotNull();
  List response = observed ? as<List>(y) : List::create();
  if (observed && response.size() != n) {
    stop("Choices must have one entry per observation.");
  }
  if (sigma.nrow() != 2 || sigma.ncol() != 2) {
    stop("Binary probit requires a two-dimensional Sigma.");
  }

  NumericMatrix omega_mat(0, 0);
  IntegerVector position_vec;
  if (mixed) {
    omega_mat = as<NumericMatrix>(omega);
    if (position.isNull()) {
      stop("Random effect positions are required with Omega.");
    }
    position_vec = as<IntegerVector>(position);
    if (omega_mat.nrow() != position_vec.size() ||
        omega_mat.ncol() != position_vec.size()) {
      stop("Omega must match random effect positions.");
    }
    for (int pos : position_vec) {
      if (pos < 1 || pos > beta.size()) {
        stop("Random effect positions must index beta.");
      }
    }
  }

  bool has_availability = availability.isNotNull();
  List availability_list = has_availability ?
    as<List>(availability) : List::create();
  if (has_availability && availability_list.size() != n) {
    stop("Availability must have one entry per observation.");
  }

  NumericVector out(observed ? n : n * 2);
  bool long_loop = out.size() >= interrupt_size;
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix design = x[obs];
    if (design.nrow() != 2 || design.ncol() != beta.size()) {
      stop("Binary design matrices must have two rows and match beta.");
    }
    IntegerVector available;
    if (has_availability) {
      available = availability_list[obs];
    }
    std::vector<bool> use = make_availability_mask(
      2, available, !has_availability
    );
    int available_count = use[0] + use[1];
    IntegerVector choice;
    if (observed) {
      choice = response[obs];
      if (choice.size() == 0 || choice.size() > available_count) {
        stop("Choices must contain available alternatives.");
      }
      std::vector<bool> chosen(2, false);
      for (int value : choice) {
        int pos = value - 1;
        if (pos < 0 || pos > 1 || !use[pos] || chosen[pos]) {
          stop("Choices must contain available alternatives once.");
        }
        chosen[pos] = true;
      }
    }
    if (available_count == 1) {
      int only = use[0] ? 0 : 1;
      if (observed) {
        if (choice[0] - 1 != only) {
          stop("The chosen alternative must be available.");
        }
        out[obs] = log ? 0 : 1;
      } else {
        out[obs + n * only] = 1;
      }
      continue;
    }

    double utility[2] = {0, 0};
    for (int alternative = 0; alternative < 2; ++alternative) {
      for (int p = 0; p < design.ncol(); ++p) {
        utility[alternative] += design(alternative, p) * beta[p];
      }
    }
    int first = 0;
    if (observed) {
      first = choice[0] - 1;
    }
    int second = 1 - first;
    double variance = sigma(first, first) + sigma(second, second) -
      2 * sigma(first, second);
    if (mixed) {
      for (R_xlen_t row = 0; row < position_vec.size(); ++row) {
        int pos_row = position_vec[row] - 1;
        double x_row = design(first, pos_row) -
          design(second, pos_row);
        for (R_xlen_t col = 0; col < position_vec.size(); ++col) {
          int pos_col = position_vec[col] - 1;
          double x_col = design(first, pos_col) -
            design(second, pos_col);
          variance += x_row * omega_mat(row, col) * x_col;
        }
      }
    }
    double z = (utility[first] - utility[second]) /
      std::sqrt(variance);
    if (observed) {
      out[obs] = R::pnorm(z, 0, 1, true, log);
    } else {
      out[obs] = R::pnorm(z, 0, 1, true, false);
      out[obs + n] = R::pnorm(z, 0, 1, false, false);
    }
  }
  if (!observed) {
    out.attr("dim") = IntegerVector::create(n, 2);
  }
  return out;
}

// Build probit comparison constraints for a choice or partial ranking
// [[Rcpp::export]]
List cpp_probit_d(
    NumericVector v, IntegerVector y, bool ranked,
    Nullable<IntegerVector> availability = R_NilValue) {
  int j = v.size();
  bool has_availability = availability.isNotNull();
  IntegerVector available = has_availability ?
    as<IntegerVector>(availability) : seq_len(j);
  std::vector<bool> use = make_availability_mask(
    j, available, !has_availability
  );
  int rows = available.size() - 1;
  NumericMatrix d(rows, j);
  NumericVector upper(rows);
  bool long_loop = rows >= interrupt_size;
  if (ranked) {
    if (y.size() == 0 || y.size() > available.size()) {
      stop("Partial rankings must contain available alternatives.");
    }
    std::vector<bool> ranked_mask(j, false);
    int row = 0;
    for (R_xlen_t r = 0; r < y.size(); ++r) {
      int pos = y[r] - 1;
      if (pos < 0 || pos >= j || !use[pos] || ranked_mask[pos]) {
        stop("Rankings must contain available alternatives once.");
      }
      ranked_mask[pos] = true;
    }
    for (R_xlen_t r = 0; r + 1 < y.size(); ++r) {
      if (long_loop) {
        check_interrupt(r);
      }
      int first = y[r] - 1;
      int second = y[r + 1] - 1;
      d(row, first) = -1;
      d(row, second) = 1;
      upper[row++] = v[first] - v[second];
    }
    int last = y[y.size() - 1] - 1;
    for (int value : available) {
      int alt = value - 1;
      if (!ranked_mask[alt]) {
        d(row, last) = -1;
        d(row, alt) = 1;
        upper[row++] = v[last] - v[alt];
      }
    }
  } else {
    int ref = y[0] - 1;
    if (ref < 0 || ref >= j || !use[ref]) {
      stop("The chosen alternative must be available.");
    }
    int row = 0;
    for (int value : available) {
      int alt = value - 1;
      if (long_loop) {
        check_interrupt(alt);
      }
      if (alt != ref) {
        d(row, ref) = -1;
        d(row, alt) = 1;
        upper[row++] = v[ref] - v[alt];
      }
    }
  }
  return List::create(_["D"] = d, _["upper"] = upper);
}

// Construct and standardize covariance matrices
// [[Rcpp::export]]
List cpp_probit_cov(NumericMatrix x, NumericMatrix omega,
                    NumericMatrix sigma, NumericMatrix d,
                    int occasions) {
  int n = x.nrow();
  int j = sigma.nrow();
  arma::mat xa(x.begin(), n, x.ncol(), false);
  arma::mat oa(omega.begin(), omega.nrow(), omega.ncol(), false);
  arma::mat sa(sigma.begin(), j, j, false);
  arma::mat da(d.begin(), d.nrow(), n, false);
  bool large = n >= 512;
  if (large) {
    Rcpp::checkUserInterrupt();
  }
  arma::mat u = xa * oa * xa.t();
  if (large) {
    Rcpp::checkUserInterrupt();
  }
  for (int t = 0; t < occasions; ++t) {
    if (occasions >= interrupt_size) {
      check_interrupt(t);
    }
    int first = t * j;
    int last = first + j - 1;
    u.submat(first, first, last, last) += sa;
  }
  if (large) {
    Rcpp::checkUserInterrupt();
  }
  arma::mat cov = da * u * da.t();
  if (large) {
    Rcpp::checkUserInterrupt();
  }
  arma::vec scale = arma::sqrt(cov.diag());
  arma::mat corr = cov;
  corr.each_col() /= scale;
  corr.each_row() /= scale.t();
  return List::create(
    _["cov"] = wrap(cov),
    _["corr"] = wrap(corr),
    _["scale"] = wrap(scale)
  );
}
