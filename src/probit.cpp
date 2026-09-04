#include <RcppArmadillo.h>

#include <oeli.h>

#include "probability_utils.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace choicedata;

namespace {

const double negative_infinity = -std::numeric_limits<double>::infinity();

// Difference constraints of one choice occasion: rows of `d` select utility
// differences that must be positive... expressed as `d %*% U < upper` where
// `upper` holds the corresponding differences of the systematic utilities.
// This mirrors `cpp_probit_d()` for the chosen alternative `ref` (zero-based)
// or for a partial ranking.
void probit_constraints(
    const arma::vec& v, const std::vector<int>& ranking, int ref, bool ranked,
    const std::vector<bool>& use, const std::vector<int>& available,
    arma::mat& d, arma::vec& upper
) {
  const int j = static_cast<int>(v.n_elem);
  const int rows = static_cast<int>(available.size()) - 1;
  d.zeros(std::max(rows, 0), j);
  upper.set_size(std::max(rows, 0));
  int row = 0;
  if (ranked) {
    std::vector<bool> ranked_mask(j, false);
    for (int value : ranking) {
      const int pos = value - 1;
      if (pos < 0 || pos >= j || !use[pos] || ranked_mask[pos]) {
        stop("Rankings must contain available alternatives once.");
      }
      ranked_mask[pos] = true;
    }
    for (size_t r = 0; r + 1 < ranking.size(); ++r) {
      const int first = ranking[r] - 1;
      const int second = ranking[r + 1] - 1;
      d(row, first) = -1;
      d(row, second) = 1;
      upper[row++] = v[first] - v[second];
    }
    const int last = ranking.back() - 1;
    for (int value : available) {
      const int alt = value - 1;
      if (!ranked_mask[alt]) {
        d(row, last) = -1;
        d(row, alt) = 1;
        upper[row++] = v[last] - v[alt];
      }
    }
  } else {
    if (ref < 0 || ref >= j || !use[ref]) {
      stop("The chosen alternative must be available.");
    }
    for (int value : available) {
      const int alt = value - 1;
      if (alt != ref) {
        d(row, ref) = -1;
        d(row, alt) = 1;
        upper[row++] = v[ref] - v[alt];
      }
    }
  }
}

// Multivariate normal rectangle probability of `oeli`: exact for up to three
// dimensions and GHK simulation on Halton points otherwise.
double mvn_cdf(
    const arma::vec& lower, const arma::vec& upper, const arma::mat& corr,
    int ghk_draws
) {
  if (upper.n_elem == 0) return 1.0;
  return oeli::pmvnorm(
    upper, arma::zeros<arma::vec>(upper.n_elem), corr, 1e-3,
    Rcpp::wrap(lower), "ghk", ghk_draws
  );
}

// Probability that all constrained utility differences are below `upper`
// given the covariance `cov` of the differences.
double constraint_probability(
    const arma::mat& cov, const arma::vec& upper, int ghk_draws
) {
  const arma::uword rows = upper.n_elem;
  if (rows == 0) return 1.0;
  arma::vec scale = arma::sqrt(cov.diag());
  arma::mat corr = cov;
  corr.each_col() /= scale;
  corr.each_row() /= scale.t();
  arma::vec lower(rows);
  lower.fill(negative_infinity);
  return mvn_cdf(lower, upper / scale, corr, ghk_draws);
}

double log_probability(double p) {
  return std::log(std::max(p, 0.0) + std::numeric_limits<double>::min());
}

std::vector<int> availability_indices(
    const List& availability_list, bool has_availability, R_xlen_t obs, int j
) {
  std::vector<int> available;
  if (has_availability) {
    IntegerVector values = availability_list[obs];
    available.assign(values.begin(), values.end());
  } else {
    available.resize(j);
    for (int alt = 0; alt < j; ++alt) available[alt] = alt + 1;
  }
  return available;
}

arma::mat completed_omega(
    const Nullable<NumericMatrix>& omega,
    const Nullable<IntegerVector>& position, int p
) {
  arma::mat out(p, p, arma::fill::zeros);
  if (omega.isNull()) return out;
  NumericMatrix omega_mat = as<NumericMatrix>(omega);
  if (position.isNull()) {
    stop("Random effect positions are required with Omega.");
  }
  IntegerVector position_vec = as<IntegerVector>(position);
  if (omega_mat.nrow() != position_vec.size() ||
      omega_mat.ncol() != position_vec.size()) {
    stop("Omega must match random effect positions.");
  }
  for (R_xlen_t row = 0; row < position_vec.size(); ++row) {
    if (position_vec[row] < 1 || position_vec[row] > p) {
      stop("Random effect positions must index beta.");
    }
    for (R_xlen_t col = 0; col < position_vec.size(); ++col) {
      out(position_vec[row] - 1, position_vec[col] - 1) = omega_mat(row, col);
    }
  }
  return out;
}

}

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
  if (y.size() == 0 || (ranked && y.size() > available.size())) {
    stop(ranked ? "Partial rankings must contain available alternatives." :
      "The chosen alternative must be available.");
  }
  arma::vec utilities(v.begin(), j, false);
  std::vector<int> available_vector(available.begin(), available.end());
  std::vector<int> ranking(y.begin(), y.end());
  arma::mat d;
  arma::vec upper;
  probit_constraints(
    utilities, ranking, y[0] - 1, ranked, use, available_vector, d, upper
  );
  return List::create(
    _["D"] = wrap(d), _["upper"] = NumericVector(upper.begin(), upper.end())
  );
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

// Cross-sectional probit probabilities for any number of alternatives, with
// optional normal random effects integrated analytically.
// [[Rcpp::export]]
NumericVector cpp_probit_probabilities(
    List x, Nullable<List> y, NumericVector beta, NumericMatrix sigma,
    Nullable<NumericMatrix> omega = R_NilValue,
    Nullable<IntegerVector> position = R_NilValue, bool ranked = false,
    bool log = false, Nullable<List> availability = R_NilValue,
    int ghk_draws = 500
) {
  const R_xlen_t n = x.size();
  const int j = sigma.nrow();
  const int p = beta.size();
  const bool observed = y.isNotNull();
  List response = observed ? as<List>(y) : List::create();
  if (observed && response.size() != n) {
    stop("Choices must have one entry per observation.");
  }
  const bool has_availability = availability.isNotNull();
  List availability_list = has_availability ?
    as<List>(availability) : List::create();
  if (has_availability && availability_list.size() != n) {
    stop("Availability must have one entry per observation.");
  }
  arma::mat omega_full = completed_omega(omega, position, p);
  arma::mat sigma_mat(sigma.begin(), j, j, false);
  arma::vec beta_vec(beta.begin(), p, false);

  NumericVector out(observed ? n : n * j);
  arma::mat d;
  arma::vec upper;
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    if (n >= interrupt_size) check_interrupt(obs);
    NumericMatrix design = x[obs];
    if (design.nrow() != j || design.ncol() != p) {
      stop("Design matrices must have one row per alternative.");
    }
    arma::mat X(design.begin(), j, p, false);
    arma::vec v = X * beta_vec;
    arma::mat U = X * omega_full * X.t() + sigma_mat;
    std::vector<int> available = availability_indices(
      availability_list, has_availability, obs, j
    );
    IntegerVector available_vector(available.begin(), available.end());
    std::vector<bool> use = make_availability_mask(
      j, available_vector, !has_availability
    );
    if (observed) {
      IntegerVector choice = response[obs];
      if (choice.size() == 0) stop("Observed choices must not be empty.");
      std::vector<int> ranking(choice.begin(), choice.end());
      probit_constraints(
        v, ranking, choice[0] - 1, ranked, use, available, d, upper
      );
      const double probability = constraint_probability(
        d * U * d.t(), upper, ghk_draws
      );
      out[obs] = log ? log_probability(probability) : probability;
    } else {
      double total = 0.0;
      for (int value : available) {
        const int alt = value - 1;
        std::vector<int> ranking(1, value);
        probit_constraints(v, ranking, alt, false, use, available, d, upper);
        const double probability = constraint_probability(
          d * U * d.t(), upper, ghk_draws
        );
        out[obs + n * alt] = probability;
        total += probability;
      }
      if (total > 0.0) {
        for (int value : available) out[obs + n * (value - 1)] /= total;
      }
    }
  }
  if (!observed) out.attr("dim") = IntegerVector::create(n, j);
  return out;
}

// Joint panel probabilities of mixed probit models with normal random
// effects, evaluated per decider with optional composite marginal
// likelihoods over pairs of choice occasions.
// [[Rcpp::export]]
SEXP cpp_panel_probabilities(
    List x, List y, IntegerVector tp, NumericVector beta,
    NumericMatrix omega, NumericMatrix sigma,
    Nullable<List> availability = R_NilValue, bool ranked = false,
    int cml_type = 0, bool log = false, int ghk_draws = 500,
    bool return_chunks = false
) {
  const R_xlen_t n_deciders = tp.size();
  const int j = sigma.nrow();
  const int p = beta.size();
  if (omega.nrow() != p || omega.ncol() != p) {
    stop("Omega must be completed to the dimension of beta.");
  }
  const bool has_availability = availability.isNotNull();
  List availability_list = has_availability ?
    as<List>(availability) : List::create();
  if (has_availability && availability_list.size() != x.size()) {
    stop("Availability must have one entry per observation.");
  }
  if (y.size() != x.size()) {
    stop("Choices must have one entry per observation.");
  }
  arma::mat omega_mat(omega.begin(), p, p, false);
  arma::mat sigma_mat(sigma.begin(), j, j, false);
  arma::vec beta_vec(beta.begin(), p, false);

  NumericVector probabilities(return_chunks ? 0 : n_deciders);
  List chunk_output(return_chunks ? n_deciders : 0);
  R_xlen_t offset = 0;
  arma::mat d_t;
  arma::vec upper_t;
  for (R_xlen_t dec = 0; dec < n_deciders; ++dec) {
    if (n_deciders >= interrupt_size / 8) check_interrupt(dec, 128);
    const int occasions = tp[dec];
    if (occasions < 1 || offset + occasions > x.size()) {
      stop("Panel lengths must match the number of observations.");
    }
    // Stack the design and the constraints of all occasions.
    arma::mat X_n(occasions * j, p);
    std::vector<arma::mat> d_blocks(occasions);
    std::vector<arma::vec> upper_blocks(occasions);
    std::vector<int> row_start(occasions + 1, 0);
    for (int t = 0; t < occasions; ++t) {
      const R_xlen_t obs = offset + t;
      NumericMatrix design = x[obs];
      if (design.nrow() != j || design.ncol() != p) {
        stop("Design matrices must have one row per alternative.");
      }
      arma::mat X(design.begin(), j, p, false);
      X_n.rows(t * j, t * j + j - 1) = X;
      arma::vec v = X * beta_vec;
      std::vector<int> available = availability_indices(
        availability_list, has_availability, obs, j
      );
      IntegerVector available_vector(available.begin(), available.end());
      std::vector<bool> use = make_availability_mask(
        j, available_vector, !has_availability
      );
      IntegerVector choice = y[obs];
      if (choice.size() == 0) stop("Observed choices must not be empty.");
      std::vector<int> ranking(choice.begin(), choice.end());
      probit_constraints(
        v, ranking, choice[0] - 1, ranked, use, available, d_t, upper_t
      );
      d_blocks[t] = d_t;
      upper_blocks[t] = upper_t;
      row_start[t + 1] = row_start[t] + static_cast<int>(upper_t.n_elem);
    }
    const int rows = row_start[occasions];
    arma::mat D(rows, occasions * j, arma::fill::zeros);
    arma::vec upper(rows);
    for (int t = 0; t < occasions; ++t) {
      const int size = row_start[t + 1] - row_start[t];
      if (size > 0) {
        D.submat(
          row_start[t], t * j, row_start[t + 1] - 1, t * j + j - 1
        ) = d_blocks[t];
        upper.subvec(row_start[t], row_start[t + 1] - 1) = upper_blocks[t];
      }
    }
    arma::mat U = X_n * omega_mat * X_n.t();
    for (int t = 0; t < occasions; ++t) {
      U.submat(t * j, t * j, t * j + j - 1, t * j + j - 1) += sigma_mat;
    }
    arma::mat cov = D * U * D.t();
    arma::vec scale = arma::sqrt(cov.diag());
    arma::mat corr = cov;
    corr.each_col() /= scale;
    corr.each_row() /= scale.t();
    arma::vec standardized = upper / scale;

    // Occasion chunks of the full or composite likelihood.
    std::vector<std::vector<int>> chunks;
    if (cml_type == 0) {
      std::vector<int> all(occasions);
      for (int t = 0; t < occasions; ++t) all[t] = t;
      chunks.push_back(all);
    } else if (occasions > 1) {
      for (int first = 0; first + 1 < occasions; ++first) {
        const int last = cml_type == 1 ? occasions - 1 : first + 1;
        for (int second = first + 1; second <= last; ++second) {
          chunks.push_back(std::vector<int>{first, second});
        }
      }
    }
    std::vector<double> chunk_probabilities(chunks.size());
    for (size_t k = 0; k < chunks.size(); ++k) {
      std::vector<arma::uword> index;
      for (int t : chunks[k]) {
        for (int r = row_start[t]; r < row_start[t + 1]; ++r) {
          index.push_back(static_cast<arma::uword>(r));
        }
      }
      arma::uvec rows_k(index);
      arma::vec lower_k(rows_k.n_elem);
      lower_k.fill(negative_infinity);
      chunk_probabilities[k] = std::max(
        mvn_cdf(
          lower_k, standardized.elem(rows_k), corr.submat(rows_k, rows_k),
          ghk_draws
        ),
        0.0
      );
    }
    if (return_chunks) {
      NumericVector values(chunk_probabilities.size());
      for (size_t k = 0; k < chunk_probabilities.size(); ++k) {
        values[k] = log ? log_probability(chunk_probabilities[k]) :
          chunk_probabilities[k];
      }
      chunk_output[dec] = values;
    } else {
      double log_value = 0.0;
      for (double value : chunk_probabilities) {
        log_value += log_probability(value);
      }
      probabilities[dec] = log ? log_value : std::exp(log_value);
    }
    offset += occasions;
  }
  if (offset != x.size()) {
    stop("Panel lengths must sum to the number of observations.");
  }
  if (return_chunks) return chunk_output;
  return probabilities;
}
