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
double ranked_log_prob(const Vector& u, const IntegerVector& ranking) {
  std::vector<bool> use(u.size(), true);
  double value = 0;
  bool long_loop = ranking.size() >= interrupt_size;
  for (R_xlen_t r = 0; r < ranking.size(); ++r) {
    if (long_loop) {
      check_interrupt(r, 64);
    }
    int pos = ranking[r] - 1;
    value += u[pos] - log_sum_exp(u, use);
    use[pos] = false;
  }
  return value;
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

// [[Rcpp::export]]
NumericVector cpp_ologit(NumericVector v, NumericVector gamma,
                         IntegerVector y, bool log = false) {
  NumericVector out(v.size());
  int last = gamma.size() + 1;
  bool long_loop = v.size() >= interrupt_size;
  for (R_xlen_t i = 0; i < v.size(); ++i) {
    if (long_loop) {
      check_interrupt(i);
    }
    int choice = y[i];
    double lower = choice == 1 ? R_NegInf : gamma[choice - 2];
    double upper = choice == last ? R_PosInf : gamma[choice - 1];
    lower -= v[i];
    upper -= v[i];
    out[i] = log ? log_interval_prob(lower, upper) :
      interval_prob(lower, upper);
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix cpp_ologit_all(NumericVector v, NumericVector gamma) {
  int n = v.size();
  int choices = gamma.size() + 1;
  NumericMatrix out(n, choices);
  R_xlen_t work = static_cast<R_xlen_t>(n) * choices;
  bool long_loop = work >= interrupt_size;
  R_xlen_t pos = 0;
  for (int i = 0; i < n; ++i) {
    for (int j = 0; j < choices; ++j) {
      if (long_loop) {
        check_interrupt(pos);
      }
      double lower = j == 0 ? R_NegInf : gamma[j - 1];
      double upper = j == choices - 1 ? R_PosInf : gamma[j];
      out(i, j) = interval_prob(lower - v[i], upper - v[i]);
      ++pos;
    }
  }
  return out;
}

// [[Rcpp::export]]
double cpp_ranked_logit(NumericVector u, IntegerVector ranking,
                        bool log = false) {
  double value = ranked_log_prob(u, ranking);
  return log ? value : std::exp(value);
}

// [[Rcpp::export]]
NumericVector cpp_mnl_chosen(List x, List y, NumericVector beta,
                             bool ranked = false, bool log = false) {
  R_xlen_t n = x.size();
  NumericVector out(n);
  bool long_loop = n >= interrupt_size;
  std::vector<double> u;
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix x_obs = x[obs];
    IntegerVector y_obs = y[obs];
    bool long_design = x_obs.nrow() >= interrupt_size;
    u.assign(x_obs.nrow(), 0);
    double umax = R_NegInf;
    for (int j = 0; j < x_obs.nrow(); ++j) {
      if (long_design) {
        check_interrupt(j);
      }
      for (int p = 0; p < x_obs.ncol(); ++p) {
        u[j] += x_obs(j, p) * beta[p];
      }
      umax = std::max(umax, u[j]);
    }
    double value;
    if (ranked) {
      value = ranked_log_prob(u, y_obs);
    } else {
      int choice = y_obs[0] - 1;
      double total = 0;
      for (int j = 0; j < x_obs.nrow(); ++j) {
        total += std::exp(u[j] - umax);
      }
      value = u[choice] - umax - std::log(total);
    }
    out[obs] = log ? value : std::exp(value);
  }
  return out;
}

// [[Rcpp::export]]
NumericMatrix cpp_mnl_all(List x, NumericVector beta, bool log = false) {
  NumericMatrix first = x[0];
  NumericMatrix out(x.size(), first.nrow());
  bool long_loop = x.size() >= interrupt_size;
  for (R_xlen_t obs = 0; obs < x.size(); ++obs) {
    if (long_loop) {
      check_interrupt(obs);
    }
    NumericMatrix x_obs = x[obs];
    double umax = R_NegInf;
    for (int j = 0; j < x_obs.nrow(); ++j) {
      double utility = 0;
      for (int p = 0; p < x_obs.ncol(); ++p) {
        utility += x_obs(j, p) * beta[p];
      }
      out(obs, j) = utility;
      umax = std::max(umax, utility);
    }
    double total = 0;
    for (int j = 0; j < x_obs.nrow(); ++j) {
      total += std::exp(out(obs, j) - umax);
    }
    double log_total = umax + std::log(total);
    for (int j = 0; j < x_obs.nrow(); ++j) {
      out(obs, j) = log ? out(obs, j) - log_total :
        std::exp(out(obs, j) - log_total);
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

// [[Rcpp::export]]
NumericVector cpp_average_draws(NumericMatrix draws, NumericVector beta,
                                IntegerVector position, Function compute,
                                bool log = false) {
  NumericVector mean;
  for (int r = 0; r < draws.nrow(); ++r) {
    if (draws.nrow() >= interrupt_size) {
      check_interrupt(r, 64);
    }
    NumericVector beta_draw = clone(beta);
    for (R_xlen_t j = 0; j < position.size(); ++j) {
      int pos = position[j] - 1;
      beta_draw[pos] += draws(r, j);
    }
    NumericVector value = compute(beta_draw);
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
List cpp_probit_d(NumericVector v, IntegerVector y, bool ranked) {
  int j = v.size();
  int rows = ranked ? y.size() - 1 : j - 1;
  NumericMatrix d(rows, j);
  NumericVector upper(rows);
  bool long_loop = rows >= interrupt_size;
  if (ranked) {
    for (int r = 0; r < rows; ++r) {
      if (long_loop) {
        check_interrupt(r);
      }
      int first = y[r] - 1;
      int second = y[r + 1] - 1;
      d(r, first) = -1;
      d(r, second) = 1;
      upper[r] = v[first] - v[second];
    }
  } else {
    int ref = y[0] - 1;
    int row = 0;
    for (int alt = 0; alt < j; ++alt) {
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
