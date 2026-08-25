#include "probability_utils.h"

#include <algorithm>
#include <cmath>
#include <vector>

using namespace Rcpp;
using namespace choicedata;

namespace {

// Evaluate the logistic CDF
double inv_logit(double x) {
  if (x >= 0) {
    return 1 / (1 + std::exp(-x));
  }
  double ex = std::exp(x);
  return ex / (1 + ex);
}

// Compute logistic probability mass between two bounds
double interval_prob(double lower, double upper) {
  if (lower >= 0) {
    return inv_logit(-lower) - inv_logit(-upper);
  }
  return inv_logit(upper) - inv_logit(lower);
}

// Evaluate the logarithm of the logistic CDF
double log_inv_logit(double x) {
  if (x >= 0) {
    return -std::log1p(std::exp(-x));
  }
  return x - std::log1p(std::exp(x));
}

// Compute log logistic probability mass between two bounds
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

// Compute a sequential ranked logit probability in log space
template <typename Vector, typename Ranking>
double ranked_log_prob(const Vector& u, const Ranking& ranking,
                       std::vector<bool> use) {
  double value = 0;
  bool long_loop = static_cast<R_xlen_t>(ranking.size()) >= interrupt_size;
  for (R_xlen_t r = 0;
       r < static_cast<R_xlen_t>(ranking.size()); ++r) {
    if (long_loop) {
      check_interrupt(r, 64);
    }
    int pos = ranking[r] - 1;
    if (pos < 0 || pos >= static_cast<int>(u.size()) || !use[pos]) {
      stop("Rankings must contain available alternatives once.");
    }
    value += u[pos] - log_sum_exp(u, use);
    use[pos] = false;
  }
  return value;
}

}

// Compute ordered logit probabilities from utility intervals
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

// Compute a ranked logit probability for one utility vector
// [[Rcpp::export]]
double cpp_ranked_logit(NumericVector u, IntegerVector ranking,
                        bool log = false) {
  std::vector<bool> use(u.size(), true);
  double value = ranked_log_prob(u, ranking, use);
  return log ? value : std::exp(value);
}

// Compute observed multinomial or ranked logit probabilities
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

// Compute multinomial logit probabilities for every alternative
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

// Compute mixed logit probabilities
// [[Rcpp::export]]
NumericVector cpp_mmnl(
    List x, Nullable<List> y, NumericVector beta, NumericMatrix chol,
    NumericMatrix draws, IntegerVector position, IntegerVector type,
    Nullable<IntegerVector> tp = R_NilValue, bool ranked = false,
    bool log = false, Nullable<List> availability = R_NilValue) {
  R_xlen_t n = x.size();
  bool observed = y.isNotNull();
  bool panel = tp.isNotNull();
  R_xlen_t dim_random = position.size();

  if (draws.nrow() < 1) {
    stop("At least one draw is required.");
  }
  if (draws.ncol() != dim_random ||
      chol.nrow() != dim_random || chol.ncol() != dim_random ||
      type.size() != dim_random) {
    stop("Draw, covariance, type, and position dimensions must match.");
  }
  std::vector<int> random_position(dim_random);
  std::vector<bool> position_used(beta.size(), false);
  for (R_xlen_t d = 0; d < dim_random; ++d) {
    int pos = position[d] - 1;
    if (pos < 0 || pos >= beta.size()) {
      stop("Random effect positions must index beta.");
    }
    if (position_used[pos]) {
      stop("Random effect positions must be unique.");
    }
    if (type[d] < -1 || type[d] > 1) {
      stop("Type codes must be -1, 0, or 1.");
    }
    position_used[pos] = true;
    random_position[d] = pos;
  }
  if (ranked && !observed) {
    stop("Ranked probabilities require observed unordered choices.");
  }
  if (panel && !observed) {
    stop("Panel aggregation requires observed choices.");
  }

  List response = observed ? as<List>(y) : List::create();
  if (observed && response.size() != n) {
    stop("Choices must have one entry per observation.");
  }
  bool has_availability = availability.isNotNull();
  List availability_list = has_availability ?
    as<List>(availability) : List::create();
  if (has_availability && availability_list.size() != n) {
    stop("Availability must have one entry per observation.");
  }

  int alternatives = 0;
  if (!observed && n > 0) {
    NumericMatrix first = x[0];
    alternatives = first.nrow();
  }

  std::vector<std::vector<double>> mean_utility(n);
  std::vector<std::vector<bool>> use(n);
  std::vector<std::vector<int>> choices(n);
  for (R_xlen_t obs = 0; obs < n; ++obs) {
    NumericMatrix design = x[obs];
    if (design.ncol() != beta.size()) {
      stop("Design matrix columns must match beta.");
    }
    if (!observed && design.nrow() != alternatives) {
      stop("Design matrices must share the global alternatives.");
    }

    mean_utility[obs].assign(design.nrow(), 0);
    for (int j = 0; j < design.nrow(); ++j) {
      for (int p = 0; p < design.ncol(); ++p) {
        mean_utility[obs][j] += design(j, p) * beta[p];
      }
    }

    IntegerVector available;
    if (has_availability) {
      available = availability_list[obs];
    }
    use[obs] = make_availability_mask(
      design.nrow(), available, !has_availability
    );
    if (observed) {
      IntegerVector choice = response[obs];
      if (choice.size() == 0) {
        stop("Observed choices must not be empty.");
      }
      choices[obs].assign(choice.begin(), choice.end());
    }
  }

  IntegerVector tp_vec;
  std::vector<int> panel_index(n);
  int panels = 0;
  if (panel) {
    tp_vec = as<IntegerVector>(tp);
    panels = tp_vec.size();
    R_xlen_t obs = 0;
    for (int decider = 0; decider < panels; ++decider) {
      if (tp_vec[decider] < 1) {
        stop("Panel lengths must be positive.");
      }
      for (int t = 0; t < tp_vec[decider]; ++t) {
        if (obs >= n) {
          stop("Panel lengths must sum to the number of observations.");
        }
        panel_index[obs++] = decider;
      }
    }
    if (obs != n) {
      stop("Panel lengths must sum to the number of observations.");
    }
  }

  R_xlen_t output_size = !observed ? n * alternatives :
    (panel ? panels : n);
  NumericVector out(output_size);
  if (log) {
    std::fill(out.begin(), out.end(), R_NegInf);
  }
  std::vector<double> deviation(dim_random);
  bool long_loop =
    static_cast<double>(draws.nrow()) * std::max<R_xlen_t>(n, 1) >=
    interrupt_size;

  for (int r = 0; r < draws.nrow(); ++r) {
    if (long_loop && r % 16 == 0) {
      Rcpp::checkUserInterrupt();
    }
    for (R_xlen_t d = 0; d < dim_random; ++d) {
      int pos = random_position[d];
      double eta = beta[pos];
      for (R_xlen_t k = 0; k < dim_random; ++k) {
        eta += draws(r, k) * chol(k, d);
      }
      double coefficient = type[d] == 0 ? eta :
        type[d] * std::exp(eta);
      deviation[d] = coefficient - beta[pos];
    }

    std::vector<double> current(
      output_size, log && !panel ? R_NegInf : 0
    );
    for (R_xlen_t obs = 0; obs < n; ++obs) {
      NumericMatrix design = x[obs];
      std::vector<double> utility = mean_utility[obs];
      for (int j = 0; j < design.nrow(); ++j) {
        for (R_xlen_t d = 0; d < dim_random; ++d) {
          utility[j] += design(j, random_position[d]) * deviation[d];
        }
      }

      if (observed) {
        double log_prob;
        if (ranked) {
          log_prob = ranked_log_prob(
            utility, choices[obs], use[obs]
          );
        } else {
          int choice = choices[obs][0] - 1;
          if (choice < 0 || choice >= design.nrow() ||
              !use[obs][choice]) {
            stop("The chosen alternative must be available.");
          }
          log_prob = utility[choice] -
            log_sum_exp(utility, use[obs]);
        }
        if (panel) {
          current[panel_index[obs]] += log_prob;
        } else {
          current[obs] = log ? log_prob : std::exp(log_prob);
        }
      } else {
        double log_total = log_sum_exp(utility, use[obs]);
        for (int j = 0; j < design.nrow(); ++j) {
          if (use[obs][j]) {
            double log_prob = utility[j] - log_total;
            R_xlen_t index = obs + n * j;
            current[index] = log ? log_prob : std::exp(log_prob);
          }
        }
      }
    }

    if (panel && !log) {
      for (double& value : current) {
        value = std::exp(value);
      }
    }
    if (r == 0) {
      std::copy(current.begin(), current.end(), out.begin());
    } else {
      for (R_xlen_t i = 0; i < output_size; ++i) {
        out[i] = log ? log_add_exp(out[i], current[i]) :
          out[i] + (current[i] - out[i]) / (r + 1);
      }
    }
  }

  if (log) {
    double log_draws = std::log(draws.nrow());
    for (double& value : out) {
      value -= log_draws;
    }
  }
  if (!observed) {
    out.attr("dim") = IntegerVector::create(n, alternatives);
  }
  return out;
}
