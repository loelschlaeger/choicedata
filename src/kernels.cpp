#include <RcppArmadillo.h>

#include <algorithm>
#include <cmath>
#include <limits>
#include <vector>

using namespace Rcpp;

// [[Rcpp::depends(RcppArmadillo)]]

namespace {

const R_xlen_t interrupt_size = 4096;
const R_xlen_t interrupt_step = 1024;

void check_interrupt(R_xlen_t i, R_xlen_t step = interrupt_step) {
  if (i % step == 0) {
    Rcpp::checkUserInterrupt();
  }
}

double inv_logit(double x) {
  if (x >= 0) {
    return 1 / (1 + std::exp(-x));
  }
  double ex = std::exp(x);
  return ex / (1 + ex);
}

double interval_prob(double lower, double upper) {
  if (lower >= 0) {
    return inv_logit(-lower) - inv_logit(-upper);
  }
  return inv_logit(upper) - inv_logit(lower);
}

double log_inv_logit(double x) {
  if (x >= 0) {
    return -std::log1p(std::exp(-x));
  }
  return x - std::log1p(std::exp(x));
}

double log_diff_exp(double x, double y) {
  if (y == R_NegInf) {
    return x;
  }
  return x + std::log(-std::expm1(y - x));
}

double log_interval_prob(double lower, double upper) {
  if (lower >= 0) {
    return log_diff_exp(
      log_inv_logit(-lower), log_inv_logit(-upper)
    );
  }
  return log_diff_exp(
    log_inv_logit(upper), log_inv_logit(lower)
  );
}

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

template <typename Vector>
double log_sum_exp(const Vector& x, const std::vector<bool>& use,
                   bool interrupt = false) {
  double xmax = R_NegInf;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (interrupt) {
      check_interrupt(i);
    }
    if (use[i]) {
      xmax = std::max(xmax, x[i]);
    }
  }
  double total = 0;
  for (R_xlen_t i = 0; i < x.size(); ++i) {
    if (interrupt) {
      check_interrupt(i);
    }
    if (use[i]) {
      total += std::exp(x[i] - xmax);
    }
  }
  return xmax + std::log(total);
}

template <typename Vector>
double ranked_log_prob(const Vector& u, const IntegerVector& ranking,
                       std::vector<bool> use) {
  double value = 0;
  bool long_loop = ranking.size() >= interrupt_size;
  for (R_xlen_t r = 0; r < ranking.size(); ++r) {
    if (long_loop) {
      check_interrupt(r, 64);
    }
    int pos = ranking[r] - 1;
    if (pos < 0 || pos >= u.size() || !use[pos]) {
      stop("Rankings must contain available alternatives once.");
    }
    value += u[pos] - log_sum_exp(u, use);
    use[pos] = false;
  }
  return value;
}

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

} // namespace

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

// [[Rcpp::export]]
double cpp_logsumexp(NumericVector x) {
  std::vector<bool> use(x.size(), true);
  bool interrupt = x.size() >= interrupt_size;
  return log_sum_exp(x, use, interrupt);
}

// Mix classes without allocating a results-by-class matrix.
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

// Compute ordered utilities and probabilities in one pass.
// [[Rcpp::export]]
NumericVector cpp_ologit(
    List x, NumericVector beta, NumericVector gamma,
    Nullable<IntegerVector> y = R_NilValue, bool log = false) {
  int n = x.size();
  int last = gamma.size() + 1;
  bool observed = y.isNotNull();
  IntegerVector choices;
  if (observed) {
    choices = as<IntegerVector>(y);
    if (choices.size() != n) {
      stop("Ordered choices must have one value per observation.");
    }
  }
  NumericVector out(observed ? n : n * last);
  bool long_loop = out.size() >= interrupt_size;
  R_xlen_t pos = 0;
  for (int i = 0; i < n; ++i) {
    if (long_loop) {
      check_interrupt(pos);
    }
    NumericMatrix design = x[i];
    if (design.nrow() != 1 || design.ncol() != beta.size()) {
      stop("Ordered design matrices must have one row and match beta.");
    }
    double utility = 0;
    for (int p = 0; p < design.ncol(); ++p) {
      utility += design(0, p) * beta[p];
    }
    int first_choice = observed ? choices[i] - 1 : 0;
    int final_choice = observed ? choices[i] : last;
    if (first_choice < 0 || final_choice > last) {
      stop("Ordered choices are outside the available categories.");
    }
    for (int j = first_choice; j < final_choice; ++j) {
      if (long_loop) {
        check_interrupt(pos);
      }
      double lower = j == 0 ? R_NegInf : gamma[j - 1];
      double upper = j == last - 1 ? R_PosInf : gamma[j];
      lower -= utility;
      upper -= utility;
      R_xlen_t index = observed ? i : i + n * j;
      out[index] = log ? log_interval_prob(lower, upper) :
        interval_prob(lower, upper);
      ++pos;
    }
  }
  if (!observed) {
    out.attr("dim") = IntegerVector::create(n, last);
  }
  return out;
}

// [[Rcpp::export]]
double cpp_ranked_logit(NumericVector u, IntegerVector ranking,
                        bool log = false) {
  std::vector<bool> use(u.size(), true);
  double value = ranked_log_prob(u, ranking, use);
  return log ? value : std::exp(value);
}

// Preallocate partial rankings before filling them recursively.
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

// [[Rcpp::export]]
NumericVector cpp_mnl_chosen(
    List x, List y, NumericVector beta, bool ranked = false,
    bool log = false, Nullable<List> availability = R_NilValue) {
  R_xlen_t n = x.size();
  bool has_availability = availability.isNotNull();
  List availability_list(0);
  if (has_availability) {
    availability_list = as<List>(availability);
    if (availability_list.size() != n) {
      stop("Availability must have one entry per observation.");
    }
  }
  NumericVector out(n);
  bool long_loop = n >= interrupt_size;
  std::vector<double> u;
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix x_obs = x[obs];
    IntegerVector y_obs = y[obs];
    IntegerVector available;
    if (has_availability) {
      available = availability_list[obs];
    }
    std::vector<bool> use = make_availability_mask(
      x_obs.nrow(), available, !has_availability
    );
    bool long_design = x_obs.nrow() >= interrupt_size;
    u.assign(x_obs.nrow(), 0);
    for (int j = 0; j < x_obs.nrow(); ++j) {
      if (long_design) {
        check_interrupt(j);
      }
      for (int p = 0; p < x_obs.ncol(); ++p) {
        u[j] += x_obs(j, p) * beta[p];
      }
    }
    double value;
    if (ranked) {
      value = ranked_log_prob(u, y_obs, use);
    } else {
      int choice = y_obs[0] - 1;
      if (choice < 0 || choice >= x_obs.nrow() || !use[choice]) {
        stop("The chosen alternative must be available.");
      }
      value = u[choice] - log_sum_exp(u, use);
    }
    out[obs] = log ? value : std::exp(value);
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix cpp_mnl_all(
    List x, NumericVector beta, bool log = false,
    Nullable<List> availability = R_NilValue) {
  NumericMatrix first = x[0];
  NumericMatrix out(x.size(), first.nrow());
  std::fill(
    out.begin(), out.end(), log ? R_NegInf : 0
  );
  bool has_availability = availability.isNotNull();
  List availability_list(0);
  if (has_availability) {
    availability_list = as<List>(availability);
    if (availability_list.size() != x.size()) {
      stop("Availability must have one entry per observation.");
    }
  }
  bool long_loop = x.size() >= interrupt_size;
  for (R_xlen_t obs = 0; obs < x.size(); ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix x_obs = x[obs];
    if (x_obs.nrow() != first.nrow()) {
      stop("Design matrices must share the global alternatives.");
    }
    IntegerVector available;
    if (has_availability) {
      available = availability_list[obs];
    }
    std::vector<bool> use = make_availability_mask(
      x_obs.nrow(), available, !has_availability
    );
    NumericVector utility(x_obs.nrow());
    for (int j = 0; j < x_obs.nrow(); ++j) {
      for (int p = 0; p < x_obs.ncol(); ++p) {
        utility[j] += x_obs(j, p) * beta[p];
      }
    }
    double log_total = log_sum_exp(utility, use);
    for (int j = 0; j < x_obs.nrow(); ++j) {
      if (use[j]) {
        out(obs, j) = log ? utility[j] - log_total :
          std::exp(utility[j] - log_total);
      }
    }
  }
  return out;
}

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

// Transform and aggregate draws without a results-by-draw matrix.
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
