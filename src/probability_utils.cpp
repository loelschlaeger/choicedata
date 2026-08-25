#include <RcppArmadillo.h>

#include "probability_utils.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;
using namespace choicedata;

// [[Rcpp::depends(RcppArmadillo)]]

namespace choicedata {

const R_xlen_t interrupt_size = 4096;

// Check for user interruption
void check_interrupt(R_xlen_t i, R_xlen_t step) {
  if (i % step == 0) {
    Rcpp::checkUserInterrupt();
  }
}

// Compute log(exp(x) - exp(y)) for x >= y
double log_diff_exp(double x, double y) {
  if (y == R_NegInf) {
    return x;
  }
  return x + std::log(-std::expm1(y - x));
}

// Compute log(exp(x) + exp(y))
double log_add_exp(double x, double y) {
  if (x == R_NegInf) {
    return y;
  }
  if (y == R_NegInf) {
    return x;
  }
  if (x == R_PosInf || y == R_PosInf) {
    return R_PosInf;
  }
  double xmax = std::max(x, y);
  double xmin = std::min(x, y);
  return xmax + std::log1p(std::exp(xmin - xmax));
}

namespace {

// Compute log-sum-exp
template <typename Vector>
double log_sum_exp_impl(const Vector& x, const std::vector<bool>& use,
                        bool interrupt) {
  double xmax = R_NegInf;
  for (R_xlen_t i = 0;
       i < static_cast<R_xlen_t>(x.size()); ++i) {
    if (interrupt) {
      check_interrupt(i);
    }
    if (use[i]) {
      xmax = std::max(xmax, x[i]);
    }
  }
  double total = 0;
  for (R_xlen_t i = 0;
       i < static_cast<R_xlen_t>(x.size()); ++i) {
    if (interrupt) {
      check_interrupt(i);
    }
    if (use[i]) {
      total += std::exp(x[i] - xmax);
    }
  }
  return xmax + std::log(total);
}

}

// Compute log-sum-exp of an R vector
double log_sum_exp(const NumericVector& x, const std::vector<bool>& use,
                   bool interrupt) {
  return log_sum_exp_impl(x, use, interrupt);
}

// Compute log-sum-exp of a standard vector
double log_sum_exp(const std::vector<double>& x,
                   const std::vector<bool>& use, bool interrupt) {
  return log_sum_exp_impl(x, use, interrupt);
}

// Convert validated one-based availability indices into boolean
std::vector<bool> make_availability_mask(
    int size, const IntegerVector& available, bool use_all) {
  std::vector<bool> use(size, use_all);
  if (use_all) {
    return use;
  }
  if (available.size() == 0) {
    stop("Availability sets must not be empty.");
  }
  for (int value : available) {
    int pos = value - 1;
    if (pos < 0 || pos >= size) {
      stop("Availability indices are outside the alternatives.");
    }
    if (use[pos]) {
      stop("Availability indices must be unique.");
    }
    use[pos] = true;
  }
  return use;
}

}

namespace {

// Expand occasion numbers
IntegerVector chunk_index(const IntegerVector& occasions, int block) {
  IntegerVector out(occasions.size() * block);
  int pos = 0;
  bool long_loop = out.size() >= interrupt_size;
  for (int t : occasions) {
    int start = (t - 1) * block + 1;
    for (int j = 0; j < block; ++j) {
      if (long_loop) {
        check_interrupt(pos);
      }
      out[pos++] = start + j;
    }
  }
  return out;
}

// Recursively enumerate partial rankings
void fill_rankings(
    const IntegerVector& values, int depth, std::vector<int>& current,
    std::vector<bool>& used, List& out, R_xlen_t& pos) {
  if (current.size() == static_cast<size_t>(depth)) {
    if (out.size() >= interrupt_size) {
      check_interrupt(pos);
    }
    IntegerVector ranking(depth);
    for (int r = 0; r < depth; ++r) {
      ranking[r] = values[current[r]];
    }
    out[pos++] = ranking;
    return;
  }
  for (R_xlen_t i = 0; i < values.size(); ++i) {
    if (!used[i]) {
      used[i] = true;
      current.push_back(i);
      fill_rankings(values, depth, current, used, out, pos);
      current.pop_back();
      used[i] = false;
    }
  }
}

}

// Compute log or ordinary softmax probabilities from utilities
// [[Rcpp::export]]
NumericVector cpp_softmax(NumericVector u, bool log = false) {
  double umax = max(u);
  NumericVector out(u.size());
  double total = 0;
  bool long_loop = u.size() >= interrupt_size;
  for (R_xlen_t i = 0; i < u.size(); ++i) {
    if (long_loop) {
      check_interrupt(i);
    }
    out[i] = std::exp(u[i] - umax);
    total += out[i];
  }
  double log_total = umax + std::log(total);
  for (R_xlen_t i = 0; i < u.size(); ++i) {
    if (long_loop) {
      check_interrupt(i);
    }
    out[i] = log ? u[i] - log_total : out[i] / total;
  }
  return out;
}

// Compute log-sum-exp of all vector entries
// [[Rcpp::export]]
double cpp_logsumexp(NumericVector x) {
  std::vector<bool> use(x.size(), true);
  bool interrupt = x.size() >= interrupt_size;
  return log_sum_exp(x, use, interrupt);
}

// Mix latent-class probabilities
// [[Rcpp::export]]
NumericVector cpp_lc_prob(List probs, NumericVector weights,
                          bool log = false) {
  if (probs.size() == 0 || probs.size() != weights.size()) {
    stop("Class probabilities and weights must have equal positive length.");
  }
  NumericVector first = probs[0];
  NumericVector out(first.size(), log ? R_NegInf : 0);
  for (R_xlen_t c = 0; c < probs.size(); ++c) {
    NumericVector current = probs[c];
    if (current.size() != first.size()) {
      stop("Class probability vectors must have equal lengths.");
    }
    if (!log) {
      if (out.size() >= interrupt_size) {
        Rcpp::checkUserInterrupt();
      }
      arma::vec out_view(out.begin(), out.size(), false, true);
      arma::vec current_view(current.begin(), current.size(), false, true);
      out_view += weights[c] * current_view;
      continue;
    }
    double log_weight = std::log(weights[c]);
    for (R_xlen_t start = 0; start < out.size();
         start += interrupt_size) {
      if (out.size() >= interrupt_size) {
        Rcpp::checkUserInterrupt();
      }
      R_xlen_t end = std::min(start + interrupt_size, out.size());
      for (R_xlen_t i = start; i < end; ++i) {
        out[i] = log_add_exp(out[i], current[i] + log_weight);
      }
    }
  }
  if (first.hasAttribute("dim")) {
    out.attr("dim") = first.attr("dim");
    out.attr("dimnames") = first.attr("dimnames");
  }
  return out;
}

// Preallocate partial rankings
// [[Rcpp::export]]
List cpp_rankings(IntegerVector values, int depth) {
  if (depth < 1 || depth > values.size()) {
    stop("Ranking depth must be between one and the number of alternatives.");
  }
  R_xlen_t size = 1;
  for (int r = 0; r < depth; ++r) {
    R_xlen_t factor = values.size() - r;
    if (size > std::numeric_limits<int>::max() / factor) {
      stop("The requested ranking set is too large.");
    }
    size *= factor;
  }
  List out(size);
  std::vector<int> current;
  std::vector<bool> used(values.size(), false);
  R_xlen_t pos = 0;
  fill_rankings(values, depth, current, used, out, pos);
  return out;
}

// Multiply observation probabilities within each panel in log space
// [[Rcpp::export]]
NumericVector cpp_panel_prod(NumericVector p, IntegerVector tp,
                             bool log = false,
                             bool input_log = false) {
  NumericVector out(tp.size());
  int pos = 0;
  bool long_loop = p.size() >= interrupt_size;
  for (R_xlen_t n = 0; n < tp.size(); ++n) {
    double value = 0;
    for (int t = 0; t < tp[n]; ++t) {
      if (long_loop) {
        check_interrupt(pos);
      }
      double item = p[pos++];
      value += input_log ? item : std::log(item);
    }
    out[n] = log ? value : std::exp(value);
  }
  return out;
}

// Aggregate observation log probabilities for panel CML
// [[Rcpp::export]]
NumericVector cpp_cml_log(
    NumericVector probability, IntegerVector tp, int type) {
  if (type < 0 || type > 2) {
    stop("Composite marginal likelihood type must be 0, 1, or 2.");
  }
  NumericVector out(tp.size());
  R_xlen_t pos = 0;
  bool long_loop = probability.size() >= interrupt_size;
  for (R_xlen_t n = 0; n < tp.size(); ++n) {
    if (tp[n] < 1) {
      stop("Panel lengths must be positive.");
    }
    if (long_loop) {
      check_interrupt(n);
    }
    double value = 0;
    if (type == 0) {
      for (int t = 0; t < tp[n]; ++t) {
        if (pos + t >= probability.size()) {
          stop("Panel lengths must sum to the probability length.");
        }
        value += probability[pos + t];
      }
    } else if (type == 1) {
      for (int first = 0; first < tp[n] - 1; ++first) {
        for (int second = first + 1; second < tp[n]; ++second) {
          if (pos + second >= probability.size()) {
            stop("Panel lengths must sum to the probability length.");
          }
          value += probability[pos + first] +
            probability[pos + second];
        }
      }
    } else {
      for (int t = 0; t < tp[n] - 1; ++t) {
        if (pos + t + 1 >= probability.size()) {
          stop("Panel lengths must sum to the probability length.");
        }
        value += probability[pos + t] + probability[pos + t + 1];
      }
    }
    out[n] = value;
    pos += tp[n];
  }
  if (pos != probability.size()) {
    stop("Panel lengths must sum to the probability length.");
  }
  return out;
}

// Transform and aggregate draws
// [[Rcpp::export]]
NumericVector cpp_average_draws(
    NumericMatrix draws, NumericVector beta, IntegerVector position,
    Function compute, bool log = false,
    Nullable<NumericMatrix> chol = R_NilValue,
    Nullable<IntegerVector> type = R_NilValue,
    Nullable<List> args = R_NilValue,
    Nullable<List> chunks = R_NilValue) {
  if (draws.ncol() != position.size()) {
    stop("Draw columns must match random effect positions.");
  }
  bool has_chol = chol.isNotNull();
  NumericMatrix chol_mat(0, 0);
  if (has_chol) {
    chol_mat = as<NumericMatrix>(chol);
    if (chol_mat.nrow() != position.size() ||
        chol_mat.ncol() != position.size()) {
      stop("The Cholesky factor must match random effect positions.");
    }
  }
  IntegerVector type_vec(position.size());
  if (type.isNotNull()) {
    type_vec = as<IntegerVector>(type);
    if (type_vec.size() != position.size()) {
      stop("Type codes must match random effect positions.");
    }
  }
  for (R_xlen_t j = 0; j < position.size(); ++j) {
    if (position[j] < 1 || position[j] > beta.size()) {
      stop("Random effect positions must index beta.");
    }
    if (type_vec[j] < -1 || type_vec[j] > 1) {
      stop("Type codes must be -1, 0, or 1.");
    }
  }
  List base_args = args.isNotNull() ? as<List>(args) : List::create();
  List chunk_list = chunks.isNotNull() ? as<List>(chunks) : List::create();
  Function do_call = Environment::base_env()["do.call"];
  NumericVector mean;
  for (int r = 0; r < draws.nrow(); ++r) {
    if (draws.nrow() >= interrupt_size) {
      check_interrupt(r, 64);
    }
    NumericVector beta_draw = clone(beta);
    for (R_xlen_t j = 0; j < position.size(); ++j) {
      int pos = position[j] - 1;
      double eta = beta[pos];
      if (has_chol) {
        for (R_xlen_t k = 0; k < position.size(); ++k) {
          eta += draws(r, k) * chol_mat(k, j);
        }
      } else {
        eta += draws(r, j);
      }
      beta_draw[pos] = type_vec[j] == 0 ? eta :
        type_vec[j] * std::exp(eta);
    }
    List call_args = clone(base_args);
    if (args.isNotNull()) {
      call_args["beta"] = beta_draw;
    } else {
      call_args.push_front(beta_draw);
    }
    NumericVector value = do_call(compute, call_args);
    if (chunks.isNotNull()) {
      NumericVector sums(chunk_list.size());
      for (R_xlen_t c = 0; c < chunk_list.size(); ++c) {
        IntegerVector index = chunk_list[c];
        for (int i : index) {
          if (i < 1 || i > value.size()) {
            stop("Draw aggregation indices are out of bounds.");
          }
          sums[c] += value[i - 1];
        }
      }
      value = sums;
    }
    if (r == 0) {
      mean = clone(value);
    } else {
      bool long_loop = mean.size() >= interrupt_size;
      for (R_xlen_t i = 0; i < mean.size(); ++i) {
        if (long_loop) {
          check_interrupt(i);
        }
        if (log) {
          mean[i] = log_add_exp(mean[i], value[i]);
        } else {
          mean[i] += (value[i] - mean[i]) / (r + 1);
        }
      }
    }
  }
  if (log) {
    double log_n = std::log(draws.nrow());
    for (R_xlen_t i = 0; i < mean.size(); ++i) {
      mean[i] -= log_n;
    }
  }
  return mean;
}

// Build observation-index chunks for full or pairwise panel CML
// [[Rcpp::export]]
List cpp_cml_chunks(int tp, int block, int type) {
  R_xlen_t size = 1;
  if (type == 1 && tp > 1) {
    size = static_cast<R_xlen_t>(tp) * (tp - 1) / 2;
  } else if (type == 2 && tp > 1) {
    size = tp - 1;
  }
  List out(size);
  if (tp == 1 || type == 0) {
    out[0] = chunk_index(seq_len(tp), block);
  } else if (type == 1) {
    R_xlen_t pair = 0;
    for (int first = 1; first < tp; ++first) {
      for (int second = first + 1; second <= tp; ++second) {
        if (pair >= interrupt_size) {
          check_interrupt(pair);
        }
        out[pair] = chunk_index(
          IntegerVector::create(first, second), block
        );
        ++pair;
      }
    }
  } else {
    R_xlen_t pair = 0;
    for (int first = 1; first < tp; ++first) {
      if (tp >= interrupt_size) {
        check_interrupt(first);
      }
      out[pair++] = chunk_index(
        IntegerVector::create(first, first + 1), block
      );
    }
  }
  return out;
}

// Multiply a probability vector and optionally return its logarithm
// [[Rcpp::export]]
double cpp_prob_prod(NumericVector p, bool log = false) {
  double value = 0;
  bool long_loop = p.size() >= interrupt_size;
  for (R_xlen_t i = 0; i < p.size(); ++i) {
    if (long_loop) {
      check_interrupt(i);
    }
    double prob = p[i];
    if (prob == 0) {
      if (!log) {
        return 0;
      }
      prob = std::numeric_limits<double>::min();
    }
    value += std::log(prob);
  }
  return log ? value : std::exp(value);
}
