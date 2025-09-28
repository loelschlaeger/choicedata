test_that("MNP probabilities can be computed", {

  ### meta settings
  set.seed(1)
  J <- 3
  P <- 3
  N <- 100
  beta <- rnorm(P)
  Sigma <- oeli::sample_covariance_matrix(dim = J, df = J, scale = diag(J) * 0.5)
  true_pars <- list("beta" = beta, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
    V_n <- X_n %*% beta
    eps_n <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
    U_n <- V_n + eps_n
    y_n <- which.max(U_n)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  theta_true <- c(beta, oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1])
  ind_beta <- seq_len(P)
  ind_Sigma <- P + seq_len(J * (J - 1) / 2 - 1)

  ### calculate MNP probabilities
  probs <- choiceprob_mnp(
    X = data$X,
    y = data$y,
    beta = beta,
    Sigma = Sigma
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mnp(
    X = data$X,
    y = NULL,
    beta = beta,
    Sigma = Sigma
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
  expect_equal(
    probs,
    probs_all[cbind(seq_len(nrow(probs_all)), unlist(data$y))]
  )

  ### verify via MLE
  skip() # because Rprobit:::SJ is not exported
  nll <- function(theta, data) {
    beta <- theta[ind_beta]
    Sigma <- oeli::undiff_cov(oeli::chol_to_cov(c(1, theta[ind_Sigma])))
    probs <- choiceprob_mnp(
      X = data$X,
      y = data$y,
      beta = beta,
      Sigma = Sigma,
      gcdf = function(upper, corr) exp(Rprobit:::SJ(x = upper, r = corr)),
      lower_bound = 1e-6
    )
    -sum(log(probs))
  }
  out <- suppressWarnings(
    nlm(nll, theta_true, data, print.level = 0, iterlim = 100)
  )

  ### check deviation
  beta_true <- true_pars$beta
  beta_estimate <- out$estimate[ind_beta] * scale
  expect_lt(sqrt(sum(beta_true - beta_estimate)^2), 2)
  Sigma_true <- rbind(0, cbind(0, oeli::diff_cov(true_pars$Sigma, ref = 1)))
  Sigma_estimate <- oeli::undiff_cov(oeli::chol_to_cov(c(1, out$estimate[ind_Sigma]))) * scale^2
  expect_lt(sqrt(sum(Sigma_true - Sigma_estimate)^2), 2)
})

test_that("MNP ordered probabilities can be computed", {

  ### meta settings
  J <- 5
  P <- 3
  N <- 100
  beta <- rnorm(P)
  d <- rnorm(J - 2)
  # gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
  gamma <- c(0, cumsum(exp(d)))
  Sigma <- 2
  true_pars <- list("beta" = beta, "d" = d)

  ### normalize parameters
  scale <- sqrt(Sigma)
  beta <- beta / scale
  gamma <- gamma / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  gamma_augmented <- c(-Inf, gamma, +Inf)
  data <- list("X" = list(), "y" = list())
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
    V_n <- as.numeric(X_n %*% beta)
    eps_n <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
    U_n <- V_n + eps_n
    y_n <- findInterval(U_n, gamma_augmented, all.inside = TRUE, left.open = TRUE)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  d <- log(diff(gamma))
  theta_true <- c(beta, d)
  ind_beta <- seq_len(P)
  ind_d <- P + seq_len(J - 2)

  ### calculate MNP ordered probabilities
  probs <- choiceprob_mnp_ordered(
    X = data$X,
    y = data$y,
    beta = beta,
    Sigma = Sigma,
    gamma = gamma
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mnp_ordered(
    X = data$X,
    y = NULL,
    beta = beta,
    Sigma = Sigma,
    gamma = gamma
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
  expect_equal(
    probs,
    probs_all[cbind(seq_len(nrow(probs_all)), unlist(data$y))]
  )
})

test_that("MNP ranked probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 3
  N <- 100
  beta <- rnorm(P)
  Sigma <- oeli::sample_covariance_matrix(dim = J, df = J, scale = diag(J) * 0.5)
  true_pars <- list("beta" = beta, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
    V_n <- X_n %*% beta
    eps_n <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
    U_n <- V_n + eps_n
    y_n <- order(as.numeric(U_n), decreasing = TRUE)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  theta_true <- c(beta, oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1])
  ind_beta <- seq_len(P)
  ind_Sigma <- P + seq_len(J * (J - 1) / 2 - 1)

  ### calculate MNP probabilities
  probs <- choiceprob_mnp(
    X = data$X,
    y = data$y,
    beta = beta,
    Sigma = Sigma,
    ranked = TRUE
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mnp(
    X = data$X,
    y = NULL,
    beta = beta,
    Sigma = Sigma
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
})

test_that("probability evaluation helper matches direct computation", {

  data(train_choice)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 2, alternatives = c("A", "B"))
  )

  ch_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  params <- generate_choice_parameters(choice_effects)

  direct <- compute_choice_probabilities(
    choice_parameters = params,
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_only = TRUE
  )

  identifiers <- extract_choice_identifiers(ch_data)
  design <- design_matrices(
    x = ch_data,
    choice_effects = choice_effects,
    choice_identifiers = identifiers
  )
  indices <- extract_choice_indices(
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_identifiers = identifiers
  )

  evaluated <- evaluate_choice_probabilities(
    design_list = design,
    choice_identifiers = identifiers,
    choice_effects = choice_effects,
    choice_parameters = params,
    choice_only = TRUE,
    choice_indices = indices
  )

  expect_s3_class(evaluated, "choice_probabilities")
  expect_equal(evaluated$choice_probability, direct$choice_probability)
})

test_that("evaluate_choice_probabilities validates probability row counts", {

  data(train_choice)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 2, alternatives = c("A", "B"))
  )

  ch_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  params <- generate_choice_parameters(choice_effects)

  identifiers <- extract_choice_identifiers(ch_data)
  design <- design_matrices(
    x = ch_data,
    choice_effects = choice_effects,
    choice_identifiers = identifiers
  )
  indices <- extract_choice_indices(
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_identifiers = identifiers
  )

  truncated_identifiers <- identifiers[seq_len(nrow(identifiers) - 1), , drop = FALSE]
  attr(truncated_identifiers, "column_decider") <- attr(identifiers, "column_decider")
  attr(truncated_identifiers, "column_occasion") <- attr(identifiers, "column_occasion")
  attr(truncated_identifiers, "cross_section") <- attr(identifiers, "cross_section")

  expect_error(
    evaluate_choice_probabilities(
      design_list = design,
      choice_identifiers = truncated_identifiers,
      choice_effects = choice_effects,
      choice_parameters = params,
      choice_only = TRUE,
      choice_indices = indices
    ),
    "mismatched number of rows"
  )
})

test_that("compute_choice_probabilities returns per-alternative probit probabilities", {

  data(train_choice)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(J = 2, alternatives = c("A", "B"))
  )

  ch_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  params <- generate_choice_parameters(choice_effects)

  all_probabilities <- compute_choice_probabilities(
    choice_parameters = params,
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_only = FALSE
  )

  expect_s3_class(all_probabilities, "choice_probabilities")

  alternative_columns <- c("A", "B")
  expect_true(all(alternative_columns %in% colnames(all_probabilities)))

  probability_matrix <- as.matrix(all_probabilities[alternative_columns])
  expect_equal(
    unname(rowSums(probability_matrix)),
    rep(1, nrow(probability_matrix)),
    tolerance = 1e-8
  )

  choice_only_probabilities <- compute_choice_probabilities(
    choice_parameters = params,
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_only = TRUE
  )

  chosen_alternatives <- as.character(train_choice$choice)
  chosen_indices <- match(chosen_alternatives, alternative_columns)
  chosen_probabilities <- probability_matrix[cbind(seq_len(nrow(probability_matrix)), chosen_indices)]

  expect_equal(
    chosen_probabilities,
    choice_only_probabilities$choice_probability,
    tolerance = 1e-8
  )
})

test_that("MNL probabilities can be computed", {
  data(train_choice)

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | time,
      error_term = "logit"
    ),
    choice_alternatives = choice_alternatives(J = 2, alternatives = c("A", "B"))
  )

  ch_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  params <- choice_parameters(
    beta = rep(0.2, nrow(choice_effects))
  )

  probs <- compute_choice_probabilities(
    choice_parameters = params,
    choice_data = ch_data,
    choice_effects = choice_effects,
    choice_only = TRUE
  )

  expect_s3_class(probs, "choice_probabilities")
  expect_true(all(probs$choice_probability >= 0))
  expect_true(all(probs$choice_probability <= 1))
})

test_that("logit ordered probabilities cover full and choice-only outputs", {

  X <- list(
    matrix(c(0.3, -0.7), nrow = 1),
    matrix(c(-0.4, 0.2), nrow = 1)
  )
  beta <- c(0.5, -0.2)
  gamma <- c(-0.25, 0.75)
  gamma_augmented <- c(-Inf, gamma, Inf)

  all_probs <- choiceprob_logit(
    X = X, beta = beta, gamma = gamma
  )

  expect_equal(nrow(all_probs), length(X))
  expect_equal(
    rowSums(all_probs),
    rep(1, length(X)),
    tolerance = 1e-10
  )

  y <- list(1L, 3L)
  chosen_probs <- choiceprob_logit(
    X = X, y = y, beta = beta, gamma = gamma
  )

  expected <- vapply(seq_along(X), function(n) {
    V_n <- as.numeric(X[[n]] %*% beta)
    upper <- stats::plogis(gamma_augmented[y[[n]] + 1] - V_n)
    lower <- stats::plogis(gamma_augmented[y[[n]]] - V_n)
    upper - lower
  }, numeric(1))

  expect_equal(chosen_probs, expected, tolerance = 1e-10)
})

test_that("logit ranked probabilities honour rankings", {

  X <- list(
    matrix(c(
      0.2, -0.1,
      -0.3, 0.4,
      0.1, 0.2
    ), nrow = 3, byrow = TRUE),
    matrix(c(
      -0.5, 0.1,
      0.4, -0.3,
      0.6, 0.2
    ), nrow = 3, byrow = TRUE)
  )
  beta <- c(0.3, -0.4)
  rankings <- list(c(2L, 1L, 3L), c(1L, 3L, 2L))

  manual_ranked_prob <- function(utilities, ranking) {
    available <- seq_along(utilities)
    prob <- 1
    for (pos in seq_along(ranking)) {
      probs <- exp(utilities[available] - max(utilities[available]))
      probs <- probs / sum(probs)
      idx <- match(ranking[pos], available)
      prob <- prob * probs[idx]
      available <- available[-idx]
    }
    prob
  }

  expected <- vapply(seq_along(X), function(n) {
    utilities <- as.numeric(X[[n]] %*% beta)
    manual_ranked_prob(utilities, rankings[[n]])
  }, numeric(1))

  probs <- choiceprob_logit(
    X = X, y = rankings, beta = beta
  )

  expect_equal(probs, expected, tolerance = 1e-10)
  expect_true(all(probs > 0 & probs <= 1))
})

test_that("logit panel probabilities respect Tp", {

  X <- list(
    matrix(c(
      0.2, -0.1,
      -0.3, 0.5
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      -0.6, 0.2,
      0.4, -0.3
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      0.1, 0.3,
      -0.2, -0.4
    ), nrow = 2, byrow = TRUE)
  )
  y <- list(1L, 2L, 1L)
  Tp <- c(2L, 1L)
  beta <- c(0.4, -0.2)

  per_observation <- vapply(seq_along(X), function(n) {
    utilities <- as.numeric(X[[n]] %*% beta)
    probs <- exp(utilities - max(utilities))
    probs <- probs / sum(probs)
    probs[y[[n]]]
  }, numeric(1))
  expected <- c(prod(per_observation[1:2]), per_observation[3])

  panel_probs <- choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta
  )

  expect_equal(panel_probs, expected, tolerance = 1e-10)
  expect_true(all(panel_probs > 0 & panel_probs <= 1))
})

test_that("latent class logit probabilities combine class panels correctly", {

  X <- list(
    matrix(c(
      0.1, -0.2,
      -0.3, 0.1
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      0.4, 0.2,
      -0.5, -0.1
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      -0.2, 0.3,
      0.1, -0.4
    ), nrow = 2, byrow = TRUE)
  )
  y <- list(1L, 2L, 1L)
  Tp <- c(2L, 1L)
  beta <- list(c(0.25, -0.15), c(-0.35, 0.3))
  weights <- c(0.4, 0.6)

  class_probs <- vapply(seq_along(beta), function(c) {
    per_obs <- vapply(seq_along(X), function(n) {
      utilities <- as.numeric(X[[n]] %*% beta[[c]])
      probs <- exp(utilities - max(utilities))
      probs <- probs / sum(probs)
      probs[y[[n]]]
    }, numeric(1))
    c(prod(per_obs[1:2]), per_obs[3])
  }, numeric(length(Tp)))

  expected <- Reduce(
    `+`,
    Map(function(idx) weights[idx] * class_probs[, idx], seq_along(weights))
  )

  lc_probs <- choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta, weights = weights
  )

  expect_equal(lc_probs, expected, tolerance = 1e-10)
  expect_true(all(lc_probs > 0 & lc_probs <= 1))

  ### also ensure per-alternative probabilities mix correctly
  alt_probs <- choiceprob_logit(
    X = X, beta = beta, weights = weights
  )
  expect_equal(nrow(alt_probs), length(X))
  expect_equal(
    rowSums(alt_probs),
    rep(1, length(X)),
    tolerance = 1e-10
  )
})

test_that("mixed logit probabilities average over draws", {

  X <- list(
    matrix(c(
      1, 0,
      1, 1
    ), nrow = 2, byrow = TRUE)
  )
  y <- list(1L)
  beta <- c(0.3, -0.2)
  Omega <- matrix(0.04, nrow = 1)
  draws <- matrix(sqrt(0.04) * c(-1, 0, 1), ncol = 1)

  manual_probs <- Reduce(
    `+`,
    lapply(seq_len(nrow(draws)), function(i) {
      beta_draw <- beta
      beta_draw[2] <- beta_draw[2] + draws[i, 1]
      utilities <- X[[1]] %*% beta_draw
      probs <- exp(utilities - max(utilities))
      probs <- probs / sum(probs)
      probs
    })
  ) / nrow(draws)

  chosen <- choiceprob_logit(
    X = X, y = y, beta = beta, Omega = Omega, draws = draws
  )
  expect_equal(chosen, manual_probs[1], tolerance = 1e-10)

  all_probs <- choiceprob_logit(
    X = X, beta = beta, Omega = Omega, draws = draws
  )
  expect_equal(all_probs, matrix(manual_probs, nrow = 1), tolerance = 1e-10)
  expect_equal(rowSums(all_probs), 1, tolerance = 1e-10)
})

test_that("mixed logit panel probabilities average products over draws", {

  X <- list(
    matrix(c(
      0.2, -0.1,
      -0.3, 0.5
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      -0.6, 0.2,
      0.4, -0.3
    ), nrow = 2, byrow = TRUE),
    matrix(c(
      0.1, 0.3,
      -0.2, -0.4
    ), nrow = 2, byrow = TRUE)
  )
  y <- list(1L, 2L, 1L)
  Tp <- c(2L, 1L)
  beta <- c(0.4, -0.2)
  Omega <- matrix(0.01, nrow = 1)
  draws <- matrix(sqrt(0.01) * c(-1, 0.5, 1.5), ncol = 1)

  manual <- Reduce(
    `+`,
    lapply(seq_len(nrow(draws)), function(i) {
      beta_draw <- beta
      beta_draw[2] <- beta_draw[2] + draws[i, 1]
      per_obs <- vapply(seq_along(X), function(n) {
        utilities <- as.numeric(X[[n]] %*% beta_draw)
        probs <- exp(utilities - max(utilities))
        probs <- probs / sum(probs)
        probs[y[[n]]]
      }, numeric(1))
      c(prod(per_obs[1:2]), per_obs[3])
    })
  ) / nrow(draws)

  panel_probs <- choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta,
    Omega = Omega, draws = draws
  )

  expect_equal(panel_probs, manual, tolerance = 1e-10)
  expect_true(all(panel_probs > 0 & panel_probs <= 1))
})

test_that("choice probability computation supports ordered data", {
  ordered_df <- data.frame(
    deciderID = 1:4,
    choice = factor(
      c("low", "medium", "high", "medium"),
      levels = c("low", "medium", "high"),
      ordered = TRUE
    )
  )

  ch_data <- choice_data(
    data_frame = ordered_df,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    choice_type = "ordered"
  )

  ordered_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ 0 | 0,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(
      J = 3,
      alternatives = c("low", "medium", "high"),
      ordered = TRUE
    )
  )

  params <- choice_parameters(
    Sigma = 1,
    gamma = c(0, 1)
  )

  probs <- compute_choice_probabilities(
    choice_parameters = params,
    choice_data = ch_data,
    choice_effects = ordered_effects,
    choice_only = TRUE
  )

  expect_s3_class(probs, "choice_probabilities")
  expect_true(all(probs$choice_probability >= 0))
})

test_that("MMNP probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 1000
  beta <- rnorm(P)
  Omega <- matrix(0.5, 1, 1)
  P_r <- nrow(Omega)
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)
  true_pars <- list("beta" = beta, "Omega" = Omega, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Omega <- Omega / scale^2
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
    V_n <- X_n %*% preferences[[n]]
    eps_n <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
    U_n <- V_n + eps_n
    y_n <- which.max(U_n)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  theta_true <- c(
    beta,
    oeli::cov_to_chol(Omega),
    oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1]
  )
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_Sigma <- P + P_r * (P_r + 1) / 2 + (1:(J * (J - 1) / 2 - 1))

  ### calculate MMNP probabilities
  probs <- choiceprob_mmnp(
    X = data$X,
    y = data$y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    re_position = seq_len(P_r)
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mmnp(
    X = data$X,
    y = NULL,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    re_position = seq_len(P_r)
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
})

test_that("MMNP ranked probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  beta <- rnorm(P)
  Omega <- matrix(0.5, 1, 1)
  P_r <- nrow(Omega)
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)
  true_pars <- list("beta" = beta, "Omega" = Omega, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Omega <- Omega / scale^2
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
    V_n <- X_n %*% preferences[[n]]
    eps_n <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
    U_n <- V_n + eps_n
    y_n <- order(as.numeric(U_n), decreasing = TRUE)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  theta_true <- c(
    beta,
    oeli::cov_to_chol(Omega),
    oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1]
  )
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_Sigma <- P + P_r * (P_r + 1) / 2 + (1:(J * (J - 1) / 2 - 1))

  ### calculate MMNP probabilities
  probs <- choiceprob_mmnp(
    X = data$X,
    y = data$y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    re_position = seq_len(P_r),
    ranked = TRUE
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mmnp(
    X = data$X,
    y = NULL,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    re_position = seq_len(P_r),
    ranked = TRUE
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
})

test_that("MMNP ordered probabilities can be computed", {

  ### meta settings
  J <- 5
  P <- 3
  N <- 100
  beta <- c(-1, 0.5, 2)
  Omega <- matrix(1, 1, 1)
  P_r <- nrow(Omega)
  d <- rnorm(J - 2)
  # gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
  gamma <- c(0, cumsum(exp(d)))
  Sigma <- 2
  true_pars <- list("beta" = beta, "Omega" = Omega, "d" = d)

  ### normalize parameters
  scale <- sqrt(Sigma)
  beta <- beta / scale
  Omega <- Omega / scale^2
  gamma <- gamma / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  gamma_augmented <- c(-Inf, gamma, +Inf)
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    X_n <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
    V_n <- as.numeric(X_n %*% preferences[[n]])
    eps_n <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
    U_n <- V_n + eps_n
    y_n <- findInterval(U_n, gamma_augmented, all.inside = TRUE, left.open = TRUE)
    data[["X"]][[n]] <- X_n
    data[["y"]][[n]] <- y_n
  }
  d <- log(diff(gamma))
  theta_true <- c(beta, oeli::cov_to_chol(Omega), d)
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_d <- P + P_r * (P_r + 1) / 2 + seq_len(J - 2)

  ### calculate MNP ordered probabilities
  probs <- choiceprob_mmnp_ordered(
    X = data$X,
    y = data$y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    gamma = gamma,
    re_position = seq_len(P_r)
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  probs_all <- choiceprob_mmnp_ordered(
    X = data$X,
    y = NULL,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    gamma = gamma,
    re_position = seq_len(P_r)
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )
  expect_equal(
    probs,
    probs_all[cbind(seq_len(nrow(probs_all)), unlist(data$y))]
  )
})

test_that("MMNP latent class probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 250
  weights <- c(0.35, 0.65)
  C <- length(weights)
  beta <- lapply(seq_len(C), function(c) rnorm(P))
  P_r <- 1
  Omega <- lapply(seq_len(C), function(c) matrix(runif(P_r), P_r, P_r))
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- lapply(beta, `/`, scale)
  Omega <- lapply(Omega, `/`, scale^2)
  Sigma <- Sigma / scale^2

  ### simulate data
  class_id <- sample.int(C, size = N, replace = TRUE, prob = weights)
  data <- list("X" = vector("list", length = N), "y" = vector("list", length = N))
  for (n in seq_len(N)) {
    Omega_completed <- matrix(0, P, P)
    Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega[[class_id[n]]]
    pref_n <- oeli::rmvnorm(mean = beta[[class_id[n]]], Sigma = Omega_completed)
    X_n <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
    V_n <- X_n %*% pref_n
    eps_n <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + diag(J))
    U_n <- V_n + eps_n
    data$X[[n]] <- X_n
    data$y[[n]] <- which.max(U_n)
  }

  ### calculate MMNP latent class probabilities
  probs <- choiceprob_mmnp_lc(
    X = data$X,
    y = data$y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    weights = weights,
    re_position = seq_len(P_r)
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )

  probs_all <- choiceprob_mmnp_lc(
    X = data$X,
    y = NULL,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    weights = weights,
    re_position = seq_len(P_r)
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )

  expect_equal(
    probs,
    probs_all[cbind(seq_len(nrow(probs_all)), unlist(data$y))]
  )

  expect_equal(
    probs,
    choiceprob_probit(
      X = data$X,
      y = data$y,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      weights = weights,
      re_position = seq_len(P_r)
    )
  )
})

test_that("latent class weights are validated", {

  X <- list(matrix(c(1, 0), nrow = 2, ncol = 1))
  y <- list(1L)
  beta <- list(0.2, -0.1)
  Omega <- list(matrix(0.3, 1, 1), matrix(0.4, 1, 1))
  Sigma <- diag(2)
  base_args <- list(
    X = X,
    y = y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    weights = c(0.6, 0.4),
    re_position = 1L
  )

  expect_error(do.call(choiceprob_probit, base_args), NA)

  expect_error(
    do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(weights = c(0.6, -0.2)))
    ),
    "weights"
  )
  expect_error(
    do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(weights = c(0.5, 0.25, 0.25)))
    ),
    "weights"
  )
  expect_warning(
    normalized <- do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(weights = c(3, 1)))
    ),
    "normalized"
  )
  expect_equal(
    normalized,
    do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(weights = c(0.75, 0.25)))
    )
  )
})

test_that("MMNP ordered latent class probabilities can be computed", {

  ### meta settings
  J <- 4
  P <- 3
  N <- 150
  weights <- c(0.4, 0.6)
  C <- length(weights)
  beta <- lapply(seq_len(C), function(c) rnorm(P))
  P_r <- 1
  Omega <- lapply(seq_len(C), function(c) matrix(runif(P_r), P_r, P_r))
  d <- rnorm(J - 2)
  gamma <- c(0, cumsum(exp(d)))
  Sigma <- 2

  ### normalize parameters
  scale <- sqrt(Sigma)
  beta <- lapply(beta, `/`, scale)
  Omega <- lapply(Omega, `/`, scale^2)
  gamma <- gamma / scale
  Sigma <- Sigma / scale^2
  gamma_augmented <- c(-Inf, gamma, +Inf)

  ### simulate data
  class_id <- sample.int(C, size = N, replace = TRUE, prob = weights)
  data <- list("X" = vector("list", length = N), "y" = vector("list", length = N))
  for (n in seq_len(N)) {
    Omega_completed <- matrix(0, P, P)
    Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega[[class_id[n]]]
    pref_n <- oeli::rmvnorm(mean = beta[[class_id[n]]], Sigma = Omega_completed)
    X_n <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
    V_n <- as.numeric(X_n %*% pref_n)
    eps_n <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
    U_n <- V_n + eps_n
    data$X[[n]] <- X_n
    data$y[[n]] <- findInterval(
      U_n,
      gamma_augmented,
      all.inside = TRUE,
      left.open = TRUE
    )
  }

  ### calculate ordered MMNP latent class probabilities
  probs <- choiceprob_mmnp_ordered_lc(
    X = data$X,
    y = data$y,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    gamma = gamma,
    weights = weights,
    re_position = seq_len(P_r)
  )
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )

  probs_all <- choiceprob_mmnp_ordered_lc(
    X = data$X,
    y = NULL,
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    gamma = gamma,
    weights = weights,
    re_position = seq_len(P_r)
  )
  checkmate::expect_matrix(
    probs_all, mode = "numeric", any.missing = FALSE, nrows = N, ncols = J
  )

  expect_equal(
    probs,
    probs_all[cbind(seq_len(nrow(probs_all)), unlist(data$y))]
  )

  expect_equal(
    probs,
    choiceprob_probit(
      X = data$X,
      y = data$y,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      gamma = gamma,
      weights = weights,
      re_position = seq_len(P_r)
    )
  )
})

test_that("MMNP panel probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  beta <- rnorm(P)
  Omega <- matrix(0.5, 1, 1)
  P_r <- nrow(Omega)
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)
  true_pars <- list("beta" = beta, "Omega" = Omega, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Omega <- Omega / scale^2
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
      V_nt <- X_nt %*% preferences[[n]]
      eps_nt <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
      U_nt <- V_nt + eps_nt
      y_nt <- which.max(U_nt)
      ind <- length(data[["X"]]) + 1
      data[["X"]][[ind]] <- X_nt
      data[["y"]][[ind]] <- y_nt
    }
  }
  theta_true <- c(
    beta,
    oeli::cov_to_chol(Omega),
    oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1]
  )
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_Sigma <- P + P_r * (P_r + 1) / 2 + (1:(J * (J - 1) / 2 - 1))

  ### calculate MMNP probabilities
  for (cml in c("no", "fp", "ap")) {
    probs <- choiceprob_mmnp_panel(
      X = data$X,
      y = data$y,
      Tp = Tp,
      cml = cml,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      re_position = seq_len(P_r)
    )
    checkmate::expect_numeric(
      probs, lower = 0, upper = 1, any.missing = FALSE, len = N
    )
  }
})

test_that("MMNP ranked panel probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  beta <- rnorm(P)
  Omega <- matrix(0.5, 1, 1)
  P_r <- nrow(Omega)
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)
  true_pars <- list("beta" = beta, "Omega" = Omega, "Sigma" = Sigma)

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- beta / scale
  Omega <- Omega / scale^2
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
      V_nt <- X_nt %*% preferences[[n]]
      eps_nt <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
      U_nt <- V_nt + eps_nt
      y_nt <- order(as.numeric(U_nt), decreasing = TRUE)
      ind <- length(data[["X"]]) + 1
      data[["X"]][[ind]] <- X_nt
      data[["y"]][[ind]] <- y_nt
    }
  }
  theta_true <- c(
    beta,
    oeli::cov_to_chol(Omega),
    oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1]
  )
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_Sigma <- P + P_r * (P_r + 1) / 2 + (1:(J * (J - 1) / 2 - 1))

  ### calculate MMNP probabilities
  for (cml in c("no", "fp", "ap")) {
    probs <- choiceprob_mmnp_panel(
      X = data$X,
      y = data$y,
      Tp = Tp,
      cml = cml,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      re_position = seq_len(P_r),
      ranked = TRUE
    )
    checkmate::expect_numeric(
      probs, lower = 0, upper = 1, any.missing = FALSE, len = N
    )
  }
})

test_that("MMNP ordered panel probabilities can be computed", {

  ### meta settings
  J <- 5
  P <- 3
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  beta <- c(-1, 0.5, 2)
  Omega <- matrix(1, 1, 1)
  P_r <- nrow(Omega)
  d <- rnorm(J - 2)
  # gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
  gamma <- c(0, cumsum(exp(d)))
  Sigma <- 2
  true_pars <- list("beta" = beta, "Omega" = Omega, "d" = d)

  ### normalize parameters
  scale <- sqrt(Sigma)
  beta <- beta / scale
  Omega <- Omega / scale^2
  gamma <- gamma / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  gamma_augmented <- c(-Inf, gamma, +Inf)
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  Omega_completed <- matrix(0, P, P)
  Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega
  for (n in seq_len(N)) {
    preferences[[n]] <- oeli::rmvnorm(mean = beta, Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
      V_nt <- as.numeric(X_nt %*% preferences[[n]])
      eps_nt <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
      U_nt <- V_nt + eps_nt
      y_nt <- findInterval(U_nt, gamma_augmented, all.inside = TRUE, left.open = TRUE)
      ind <- length(data[["X"]]) + 1
      data[["X"]][[ind]] <- X_nt
      data[["y"]][[ind]] <- y_nt
    }
  }
  d <- log(diff(gamma))
  theta_true <- c(beta, oeli::cov_to_chol(Omega), d)
  ind_beta <- seq_len(P)
  ind_Omega <- P + seq_len(P_r * (P_r + 1) / 2)
  ind_d <- P + P_r * (P_r + 1) / 2 + seq_len(J - 2)

  ### calculate MMNP probabilities
  for (cml in c("no", "fp", "ap")) {
    probs <- choiceprob_mmnp_ordered_panel(
      X = data$X,
      y = data$y,
      Tp = Tp,
      cml = cml,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      gamma = gamma,
      re_position = seq_len(P_r)
    )
    checkmate::expect_numeric(
      probs, lower = 0, upper = 1, any.missing = FALSE, len = N
    )
  }
})

test_that("MMNP panel latent class probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  C <- length(weights)
  stopifnot(all(diff(weights) >= 0)) # ensure that weights are non-decr. for ident.
  beta <- lapply(seq_len(C), function(c) rnorm(P))
  P_r <- 1
  Omega <- lapply(seq_len(C), function(c) matrix(runif(P_r), 1, 1))
  Sigma <- matrix(c(1.8, -1, -0.2, -1, 1.1, 0.4, -0.2, 0.4, 0.2), 3, 3)
  true_pars <- list(
    "beta" = beta, "Omega" = Omega, "Sigma" = Sigma, "weights" = weights
  )

  ### normalize parameters
  Sigma <- rbind(0, cbind(0, oeli::diff_cov(Sigma, ref = 1)))
  scale <- sqrt(Sigma[2, 2])
  beta <- lapply(beta, `/`, scale)
  Omega <- lapply(Omega, `/`, scale^2)
  Sigma <- Sigma / scale^2

  ### simulate data
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  for (n in seq_len(N)) {
    class_n <- sample.int(C, size = 1, prob = weights)
    Omega_completed <- matrix(0, P, P)
    Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega[[class_n]]
    preferences[[n]] <- oeli::rmvnorm(mean = beta[[class_n]], Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(J * P, sd = 2), nrow = J, ncol = P)
      V_nt <- X_nt %*% preferences[[n]]
      eps_nt <- oeli::rmvnorm(n = 1, mean = 0, Sigma = Sigma + 1)
      U_nt <- V_nt + eps_nt
      y_nt <- which.max(U_nt)
      ind <- length(data[["X"]]) + 1
      data[["X"]][[ind]] <- X_nt
      data[["y"]][[ind]] <- y_nt
    }
  }
  weights_uncon_to_con <- function(weights_uncon) {
    ew <- exp(weights_uncon)
    c(1 / (1 + sum(ew)), ew / (1 + sum(ew)))
  }
  weights_con_to_uncon <- function(weights_con) {
    log(weights_con[-1] / weights_con[1])
  }
  theta_true <- c(
    unlist(beta),
    sapply(Omega, oeli::cov_to_chol),
    oeli::cov_to_chol(oeli::diff_cov(Sigma))[-1],
    weights_con_to_uncon(weights)
  )
  ind_beta <- seq_len(P * C)
  ind_Omega <- P * C + seq_len(C * P_r * (P_r + 1) / 2)
  ind_Sigma <- P * C + C * P_r * (P_r + 1) / 2 + (1:(J * (J - 1) / 2 - 1))
  ind_weights <- P * C + C * P_r * (P_r + 1) / 2 + J * (J - 1) / 2 - 1 + (1:(C - 1))

  ### calculate MMNP probabilities
  for (cml in c("no", "fp", "ap")) {
    probs <- choiceprob_mmnp_panel_lc(
      X = data$X,
      y = data$y,
      Tp = Tp,
      cml = cml,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      weights = weights,
      re_position = seq_len(P_r)
    )
    checkmate::expect_numeric(
      probs, lower = 0, upper = 1, any.missing = FALSE, len = N
    )
  }
})

test_that("latent class panel inputs are validated", {

  Tp <- c(2)
  X <- rep(list(matrix(c(1, 0), nrow = 2, ncol = 1)), sum(Tp))
  y <- list(1L, 2L)
  beta <- list(0.2, -0.1)
  Omega <- list(matrix(0.3, 1, 1), matrix(0.4, 1, 1))
  Sigma <- diag(2)
  base_args <- list(
    X = X,
    y = y,
    Tp = Tp,
    cml = "no",
    beta = beta,
    Omega = Omega,
    Sigma = Sigma,
    weights = c(0.6, 0.4),
    re_position = 1L
  )

  expect_error(do.call(choiceprob_probit, base_args), NA)

  expect_error(
    do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(Tp = c(3)))
    ),
    "Sum of"
  )
})

test_that("MMNP ordered panel latent class probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  C <- length(weights)
  stopifnot(all(diff(weights) >= 0)) # ensure that weights are non-decr. for ident.
  beta <- lapply(seq_len(C), function(c) rnorm(P))
  P_r <- 1
  Omega <- lapply(seq_len(C), function(c) matrix(runif(P_r), 1, 1))
  d <- rnorm(J - 2)
  # gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
  gamma <- c(0, cumsum(exp(d)))
  Sigma <- 2
  true_pars <- list(
    "beta" = beta, "Omega" = Omega, "d" = d, "weights" = weights
  )

  ### normalize parameters
  scale <- sqrt(Sigma)
  beta <- lapply(beta, `/`, scale)
  Omega <- lapply(Omega, `/`, scale^2)
  gamma <- gamma / scale
  Sigma <- Sigma / scale^2

  ### simulate data
  gamma_augmented <- c(-Inf, gamma, +Inf)
  data <- list("X" = list(), "y" = list())
  preferences <- list()
  for (n in seq_len(N)) {
    class_n <- sample.int(C, size = 1, prob = weights)
    Omega_completed <- matrix(0, P, P)
    Omega_completed[seq_len(P_r), seq_len(P_r)] <- Omega[[class_n]]
    preferences[[n]] <- oeli::rmvnorm(mean = beta[[class_n]], Sigma = Omega_completed)
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
      V_nt <- as.numeric(X_nt %*% preferences[[n]])
      eps_nt <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
      U_nt <- V_nt + eps_nt
      y_nt <- findInterval(U_nt, gamma_augmented, all.inside = TRUE, left.open = TRUE)
      ind <- length(data[["X"]]) + 1
      data[["X"]][[ind]] <- X_nt
      data[["y"]][[ind]] <- y_nt
    }
  }
  d <- log(diff(gamma))
  weights_uncon_to_con <- function(weights_uncon) {
    ew <- exp(weights_uncon)
    c(1 / (1 + sum(ew)), ew / (1 + sum(ew)))
  }
  weights_con_to_uncon <- function(weights_con) {
    log(weights_con[-1] / weights_con[1])
  }
  theta_true <- c(
    unlist(beta),
    sapply(Omega, oeli::cov_to_chol),
    d,
    weights_con_to_uncon(weights)
  )
  ind_beta <- seq_len(P * C)
  ind_Omega <- P * C + seq_len(C * P_r * (P_r + 1) / 2)
  ind_d <- P * C + C * P_r * (P_r + 1) / 2 + seq_len(J - 2)
  ind_weights <- P * C + C * P_r * (P_r + 1) / 2 + J - 2 + (1:(C - 1))

  ### calculate MMNP probabilities
  for (cml in c("no", "fp", "ap")) {
    probs <- choiceprob_mmnp_ordered_panel_lc(
      X = data$X,
      y = data$y,
      Tp = Tp,
      cml = cml,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      gamma = gamma,
      weights = weights,
      re_position = seq_len(P_r)
    )
    checkmate::expect_numeric(
      probs, lower = 0, upper = 1, any.missing = FALSE, len = N
    )
  }
})

test_that("default Gaussian CDF helper relies on covariance matrices", {

  skip_if_not_installed("Matrix")

  corr <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  upper <- c(0.5, -0.2)
  expect_equal(
    as.numeric(choicedata:::pmvnorm_cdf_default(upper = upper, corr = corr)),
    as.numeric(mvtnorm::pmvnorm(
      upper = upper, sigma = corr, algorithm = mvtnorm::GenzBretz()
    ))
  )

  expect_equal(
    as.numeric(choicedata:::pmvnorm_cdf_default(
      upper = 0.7,
      corr = Matrix::Matrix(1)
    )),
    stats::pnorm(0.7)
  )

  expect_identical(
    choicedata:::pmvnorm_cdf_default(
      upper = numeric(),
      corr = matrix(numeric(0), nrow = 0)
    ),
    1
  )
})

test_that("panel helper utilities cover edge cases", {

  expect_error(choicedata:::build_panel_chunks(Tp_n = 2, cml_type = 3L))

  expect_identical(
    choicedata:::compute_chunk_product(
      upper = numeric(),
      corr = matrix(numeric(0), nrow = 0),
      gcdf = function(...) 0,
      lower_bound = 0,
      chunk_indices = list()
    ),
    1
  )
})

test_that("choiceprob_probit validates random effect covariance inputs", {

  X <- list(matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE))
  y <- list(1L)
  beta <- c(0.1, -0.1)
  bad_Omega <- matrix(c(1, 2, 3, 4), nrow = 2)
  Sigma <- diag(2)

  expect_error(
    choiceprob_probit(
      X = X,
      y = y,
      beta = beta,
      Omega = bad_Omega,
      Sigma = Sigma
    ),
    "Omega",
    fixed = FALSE
  )

  good_Omega <- diag(2)

  expect_error(
    choiceprob_probit(
      X = X,
      y = y,
      beta = beta,
      Omega = good_Omega,
      Sigma = Sigma
    ),
    NA
  )
})

test_that("choiceprob_probit validates random effect positions", {

  X <- list(matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE))
  y <- list(1L)
  beta <- c(0.2, -0.2)
  Omega <- diag(2)
  Sigma <- diag(2)

  expect_error(
    choiceprob_probit(
      X = X,
      y = y,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      re_position = c(1L, 1L)
    ),
    "Random effect positions",
    fixed = FALSE
  )

  expect_error(
    choiceprob_probit(
      X = X,
      y = y,
      beta = beta,
      Omega = Omega,
      Sigma = Sigma,
      re_position = c(1L, 2L)
    ),
    NA
  )
})
