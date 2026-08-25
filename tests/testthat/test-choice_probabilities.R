test_that("MNP probabilities can be computed", {

  ### meta settings
  set.seed(1)
  J <- 3
  P <- 3
  N <- 100
  beta <- rnorm(P)
  Sigma <- oeli::sample_covariance_matrix(
    dim = J, df = J, scale = diag(J) * 0.5
  )
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
  expect_equal(rowSums(probs_all), rep(1, N))

  X0 <- list(matrix(c(0, 1), ncol = 1))
  S0 <- diag(c(1, 4))
  expect_equal(
    choiceprob_mnp(X0, list(1L), beta = 1, Sigma = S0),
    stats::pnorm(-1 / sqrt(5))
  )
  perm <- 2:1
  expect_equal(
    choiceprob_mnp(
      list(X0[[1]][perm, , drop = FALSE]),
      list(2L),
      beta = 1,
      Sigma = S0[perm, perm]
    ),
    choiceprob_mnp(X0, list(1L), beta = 1, Sigma = S0)
  )
})

test_that("MNP ordered probabilities can be computed", {

  ### meta settings
  J <- 5
  P <- 3
  N <- 100
  beta <- rnorm(P)
  d <- rnorm(J - 2)
  ### gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
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
    y_n <- findInterval(
      U_n, gamma_augmented, all.inside = TRUE, left.open = TRUE
    )
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
  expect_equal(rowSums(probs_all), rep(1, N))

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
  Sigma <- oeli::sample_covariance_matrix(
    dim = J, df = J, scale = diag(J) * 0.5
  )
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
  expect_equal(rowSums(probs_all), rep(1, N))

  X0 <- list(matrix(c(1, 0, 0, 1, 1, 1), nrow = 3))
  b0 <- c(0.4, -0.2)
  S0 <- diag(c(1, 2, 3))
  rank0 <- 1:3
  M0 <- oeli::M(ranking = rank0, dim = 3)
  ref <- mvtnorm::pmvnorm(
    upper = as.numeric(-M0 %*% X0[[1]] %*% b0),
    sigma = M0 %*% S0 %*% t(M0)
  )
  expect_equal(
    choiceprob_mnp(X0, list(rank0), b0, S0, ranked = TRUE),
    as.numeric(ref)
  )

  Xp <- list(matrix(c(1, 0, 0, 1, 1, 1, 1, 2), nrow = 4, byrow = TRUE))
  Sp <- diag(c(1, 2, 3, 4))
  available <- list(c(1L, 2L, 4L))
  partial <- list(c(2L, 1L))
  Vp <- as.numeric(Xp[[1]] %*% b0)
  delta <- cpp_probit_d(Vp, partial[[1]], TRUE, available[[1]])
  partial_ref <- mvtnorm::pmvnorm(
    upper = delta$upper,
    sigma = delta$D %*% Sp %*% t(delta$D)
  )
  expect_equal(
    choiceprob_mnp(
      Xp, partial, b0, Sp, ranked = TRUE,
      availability = available
    ),
    as.numeric(partial_ref)
  )
  available_prob <- choiceprob_mnp(
    Xp, NULL, b0, Sp, availability = available
  )
  expect_equal(available_prob[1, 3], 0)
  expect_equal(sum(available_prob), 1)

  expect_error(
    choiceprob_probit(
      X = rep(X0, 2), y = list(1L, rank0), beta = b0, Sigma = S0
    ),
    "scalar"
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

  X <- list(
    matrix(c(1, 0, 0, 1, 1, 1), nrow = 3),
    matrix(c(-1, 2, 1, 0, 0, -1), nrow = 3)
  )
  beta <- c(0.4, -0.3)
  utility <- lapply(X, function(x) as.numeric(x %*% beta))
  ref <- t(vapply(utility, function(u) {
    z <- exp(u - max(u))
    z / sum(z)
  }, numeric(3)))

  expect_equal(cpp_mnl_all(X, beta), ref)
  expect_equal(cpp_mnl_all(X, beta, log = TRUE), log(ref))
  expect_equal(
    cpp_mnl_chosen(X, list(2L, 3L), beta),
    ref[cbind(1:2, c(2, 3))]
  )

  extreme <- c(-1000, 0, 1000)
  ref_log <- extreme - max(extreme)
  ref_log <- ref_log - log(sum(exp(ref_log)))
  expect_equal(cpp_softmax(extreme), exp(ref_log))
  expect_equal(cpp_softmax(extreme, log = TRUE), ref_log)
  expect_equal(cpp_logsumexp(extreme), 1000)

  perm <- c(3L, 1L, 2L)
  perm_X <- lapply(X, function(x) x[perm, , drop = FALSE])
  perm_probs <- cpp_mnl_all(perm_X, beta)
  expect_equal(perm_probs, ref[, perm])

  large_u <- rep(c(-1, 1), 2048)
  large_softmax <- cpp_softmax(large_u)
  expect_equal(sum(large_softmax), 1)
  expect_equal(
    cpp_logsumexp(large_u),
    max(large_u) + log(sum(exp(large_u - max(large_u))))
  )

  large_X <- rep(
    list(matrix(c(0, 1), ncol = 1)),
    4096
  )
  large_y <- rep(list(1L), length(large_X))
  expect_length(cpp_mnl_chosen(large_X, large_y, 1), 4096)
  large_all <- cpp_mnl_all(large_X, 1)
  expect_equal(rowSums(large_all), rep(1, 4096))

  long_X <- list(matrix(seq_len(4096), ncol = 1))
  expect_length(cpp_mnl_chosen(long_X, list(1L), 0), 1)

  many_draws <- matrix(0, nrow = 4096, ncol = 1)
  draw_mean <- cpp_average_draws(
    many_draws, 0, 1L, function(b) b + 1
  )
  expect_equal(draw_mean, 1)
  long_mean <- cpp_average_draws(
    matrix(c(0, 1), ncol = 1),
    0,
    1L,
    function(b) rep(b + 1, 4096)
  )
  expect_equal(long_mean, rep(1.5, 4096))
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
  expect_warning(
    normalized <- choiceprob_logit(
      X = X, beta = beta, weights = 10 * weights
    ),
    "normalized"
  )
  expect_equal(normalized, alt_probs)
  expect_error(
    choiceprob_logit(X = X, beta = beta, weights = c(0, 0)),
    "positive"
  )

  expect_error(
    choiceprob_logit(
      X = X, y = y, beta = beta[[1]], weights = weights
    ),
    "class-specific"
  )
  expect_error(
    choiceprob_logit(
      X = X, y = y, beta = list(beta[[1]], beta[[2]][1]),
      weights = weights
    ),
    "equal lengths"
  )

  omega_lc <- list(matrix(0.1, nrow = 1), matrix(0.1, nrow = 1))
  expect_error(
    choiceprob_logit(
      X = X, y = y, beta = beta, Omega = omega_lc[1],
      weights = weights, draws = matrix(0, nrow = 1)
    ),
    "matching the number of classes"
  )
  expect_error(
    choiceprob_logit(
      X = X, y = y, beta = beta, Omega = omega_lc,
      weights = weights, draws = list(matrix(0, nrow = 1))
    ),
    "matrix"
  )
  expect_error(
    choiceprob_logit(
      X = X, y = y, beta = beta[[1]], Omega = omega_lc[[1]],
      n_draws = 0
    ),
    "n_draws"
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
  draws <- matrix(c(-1, 0, 1), ncol = 1)

  manual_probs <- Reduce(
    `+`,
    lapply(seq_len(nrow(draws)), function(i) {
      beta_draw <- beta
      beta_draw[2] <- beta_draw[2] +
        as.numeric(draws[i, , drop = FALSE] %*% chol(Omega))
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
  chosen_log <- choiceprob_logit(
    X = X, y = y, beta = beta, Omega = Omega, draws = draws,
    logarithm = TRUE
  )
  expect_equal(exp(chosen_log), chosen, tolerance = 1e-10)

  all_probs <- choiceprob_logit(
    X = X, beta = beta, Omega = Omega, draws = draws
  )
  expect_equal(all_probs, matrix(manual_probs, nrow = 1), tolerance = 1e-10)
  expect_equal(rowSums(all_probs), 1, tolerance = 1e-10)

  X_mix <- list(matrix(c(
    1, 0.2, -0.3,
    1, -0.4, 0.5
  ), nrow = 2, byrow = TRUE))
  beta_mix <- c(0.1, -0.2, 0.3)
  omega_mix <- matrix(c(
    0.09, 0.02, -0.01,
    0.02, 0.16, 0.03,
    -0.01, 0.03, 0.25
  ), nrow = 3, byrow = TRUE)
  draws_mix <- matrix(c(
    -1, 0, 1,
    0, 1, -1,
    1, -1, 0
  ), nrow = 3, byrow = TRUE)
  manual_mix <- mean(vapply(seq_len(nrow(draws_mix)), function(i) {
    eta <- beta_mix + as.numeric(
      draws_mix[i, , drop = FALSE] %*% chol(omega_mix)
    )
    beta_draw <- c(eta[1], exp(eta[2]), -exp(eta[3]))
    choiceprob_mnl(X_mix, list(2L), beta_draw)
  }, numeric(1)))
  expect_equal(
    choiceprob_logit(
      X = X_mix,
      y = list(2L),
      beta = beta_mix,
      Omega = omega_mix,
      re_mixing = c("cn", "cln", "cln-"),
      draws = draws_mix
    ),
    manual_mix
  )
  omega_independent <- diag(diag(omega_mix))
  expect_equal(
    choiceprob_logit(
      X = X_mix, y = list(2L), beta = beta_mix,
      Omega = omega_independent, re_mixing = c("n", "ln", "ln-"),
      draws = draws_mix
    ),
    choiceprob_logit(
      X = X_mix, y = list(2L), beta = beta_mix,
      Omega = omega_independent, re_mixing = c("cn", "cln", "cln-"),
      draws = draws_mix
    )
  )

  beta_lc <- list(beta, -beta)
  omega_lc <- list(Omega, Omega)
  weights <- c(0.25, 0.75)
  lc_probs <- choiceprob_logit(
    X = X, y = y, beta = beta_lc, Omega = omega_lc,
    weights = weights, draws = draws
  )
  lc_ref <- weights[1] * chosen + weights[2] * choiceprob_logit(
    X = X, y = y, beta = -beta, Omega = Omega, draws = draws
  )
  expect_equal(lc_probs, lc_ref)

  set.seed(1)
  generated <- choiceprob_logit(
    X = X, y = y, beta = beta, Omega = Omega, n_draws = 1
  )
  set.seed(1)
  repeated <- choiceprob_logit(
    X = X, y = y, beta = beta, Omega = Omega, n_draws = 1
  )
  expect_equal(generated, repeated)
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
  draws <- matrix(c(-1, 0.5, 1.5), ncol = 1)

  manual <- Reduce(
    `+`,
    lapply(seq_len(nrow(draws)), function(i) {
      beta_draw <- beta
      beta_draw[2] <- beta_draw[2] +
        as.numeric(draws[i, , drop = FALSE] %*% chol(Omega))
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
  panel_log <- choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta,
    Omega = Omega, draws = draws, logarithm = TRUE
  )
  expect_equal(exp(panel_log), panel_probs, tolerance = 1e-10)

  obs_probs <- choiceprob_logit(X = X, y = y, beta = beta)
  panel_ref <- c(prod(obs_probs[1:2]), obs_probs[3])
  expect_equal(cpp_panel_prod(obs_probs, Tp), panel_ref)
  expect_equal(
    cpp_panel_prod(
      log(obs_probs), Tp, log = TRUE, input_log = TRUE
    ),
    log(panel_ref)
  )

  X_ord <- lapply(X, function(x) x[1, , drop = FALSE])
  y_ord <- list(1L, 2L, 3L)
  gamma <- c(-0.5, 0.5)
  ordered <- choiceprob_logit(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta,
    Omega = Omega, gamma = gamma, draws = draws
  )
  expected_ordered <- choiceprob_mmnl(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta, Omega = Omega,
    gamma = gamma, re_position = 2L, draws = draws
  )
  expect_equal(ordered, expected_ordered)

  beta_lc <- list(beta, -beta)
  omega_lc <- list(Omega, Omega)
  weights <- c(0.4, 0.6)
  ordered_lc <- choiceprob_logit(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta_lc,
    Omega = omega_lc, gamma = gamma, weights = weights,
    draws = draws
  )
  expected_lc <- choiceprob_mmnl_lc(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta_lc,
    Omega = omega_lc, gamma = gamma, weights = weights,
    re_position = 2L, draws = draws
  )
  expect_equal(ordered_lc, expected_lc)

  panel_lc <- choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta_lc,
    Omega = omega_lc, weights = weights, draws = draws
  )
  panel_lc_ref <- weights[1] * choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = beta,
    Omega = Omega, draws = draws
  ) + weights[2] * choiceprob_logit(
    X = X, y = y, Tp = Tp, beta = -beta,
    Omega = Omega, draws = draws
  )
  expect_equal(panel_lc, panel_lc_ref)

  fixed_ordered_lc <- choiceprob_logit(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta_lc,
    gamma = gamma, weights = weights
  )
  fixed_ordered_ref <- weights[1] * choiceprob_logit(
    X = X_ord, y = y_ord, Tp = Tp, beta = beta,
    gamma = gamma
  ) + weights[2] * choiceprob_logit(
    X = X_ord, y = y_ord, Tp = Tp, beta = -beta,
    gamma = gamma
  )
  expect_equal(fixed_ordered_lc, fixed_ordered_ref)
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
  expect_equal(rowSums(probs_all), rep(1, N))

  gcdf <- function(upper, corr) {
    mvtnorm::pmvnorm(
      upper = upper,
      sigma = corr,
      algorithm = mvtnorm::Miwa()
    )
  }
  args <- list(
    X = data$X[1:2], y = data$y[1:2], beta = beta,
    Omega = Omega, Sigma = Sigma,
    re_position = seq_len(P_r), gcdf = gcdf
  )
  small <- do.call(choiceprob_mmnp, args)
  small_log <- do.call(
    choiceprob_mmnp,
    c(args, list(logarithm = TRUE))
  )
  expect_equal(exp(small_log), small, tolerance = 1e-10)
  expect_equal(do.call(choiceprob_probit, args), small)

  x <- data$X[[1]]
  v <- as.numeric(x %*% beta)
  delta <- cpp_probit_d(v, as.integer(data$y[[1]]), FALSE)
  omega <- matrix(0, nrow = P, ncol = P)
  omega[seq_len(P_r), seq_len(P_r)] <- Omega
  native_cov <- cpp_probit_cov(x, omega, Sigma, delta$D, 1L)
  utility_cov <- x %*% omega %*% t(x) + Sigma
  ref_cov <- delta$D %*% utility_cov %*% t(delta$D)
  expect_equal(native_cov$cov, ref_cov)
  expect_equal(native_cov$corr, stats::cov2cor(ref_cov))

  X_mix <- list(matrix(c(
    1, 0,
    1, 1
  ), nrow = 2, byrow = TRUE))
  beta_mix <- c(0.2, -0.3)
  omega_mix <- matrix(c(0.09, 0.03, 0.03, 0.16), nrow = 2)
  draws_mix <- matrix(c(-1, 0, 0, 1, 1, -1), nrow = 3, byrow = TRUE)
  manual_mix <- mean(vapply(seq_len(nrow(draws_mix)), function(i) {
    eta <- beta_mix + as.numeric(
      draws_mix[i, , drop = FALSE] %*% chol(omega_mix)
    )
    choiceprob_mnp(
      X_mix,
      list(1L),
      c(eta[1], exp(eta[2])),
      diag(2)
    )
  }, numeric(1)))
  expect_equal(
    choiceprob_probit(
      X = X_mix,
      y = list(1L),
      beta = beta_mix,
      Omega = omega_mix,
      Sigma = diag(2),
      re_mixing = c("cn", "cln"),
      draws = draws_mix
    ),
    manual_mix
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
  expect_equal(
    choiceprob_probit(
      X = data$X[1:2], y = data$y[1:2], beta = beta,
      Omega = Omega, Sigma = Sigma,
      re_position = seq_len(P_r)
    ),
    choiceprob_mmnp(
      X = data$X[1:2], y = data$y[1:2], beta = beta,
      Omega = Omega, Sigma = Sigma,
      re_position = seq_len(P_r), ranked = TRUE
    )
  )

  v <- as.numeric(data$X[[1]] %*% beta)
  d <- cpp_probit_d(v, as.integer(data$y[[1]]), TRUE)
  ranking <- unlist(data$y[[1]])
  expect_equal(
    d$upper,
    v[ranking[-length(ranking)]] - v[ranking[-1]]
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
  ### gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
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
    y_n <- findInterval(
      U_n, gamma_augmented, all.inside = TRUE, left.open = TRUE
    )
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
  log_probs <- choiceprob_mmnp_ordered(
    X = data$X, y = data$y, beta = beta, Omega = Omega,
    Sigma = Sigma, gamma = gamma,
    re_position = seq_len(P_r), logarithm = TRUE
  )
  expect_equal(exp(log_probs), probs, tolerance = 1e-12)
  expect_equal(
    choiceprob_probit(
      X = data$X, y = data$y, beta = beta, Omega = Omega,
      Sigma = Sigma, gamma = gamma,
      re_position = seq_len(P_r)
    ),
    probs
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
  data <- list(
    "X" = vector("list", length = N),
    "y" = vector("list", length = N)
  )
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
  default_probs <- choiceprob_probit(
    X = data$X, y = data$y, beta = beta, Omega = Omega,
    Sigma = Sigma, weights = weights
  )
  default_ref <- choiceprob_mmnp_lc(
    X = data$X, y = data$y, beta = beta, Omega = Omega,
    Sigma = Sigma, weights = weights, re_position = P
  )
  expect_equal(default_probs, default_ref)

  fixed <- choiceprob_probit(
    X = data$X, y = data$y, beta = beta,
    Sigma = Sigma, weights = weights
  )
  fixed_ref <- choiceprob_mmnp_lc(
    X = data$X, y = data$y, beta = beta, Omega = NULL,
    Sigma = Sigma, weights = weights, re_position = NULL
  )
  expect_equal(fixed, fixed_ref)

  fixed_all <- choiceprob_probit(
    X = data$X, beta = beta, Sigma = Sigma, weights = weights
  )
  expect_equal(rowSums(fixed_all), rep(1, N))

  rank_X <- data$X[1]
  ranking <- list(seq_len(J))
  fixed_rank <- choiceprob_probit(
    X = rank_X, y = ranking, beta = beta,
    Sigma = Sigma, weights = weights
  )
  rank_ref <- choiceprob_mmnp_lc(
    X = rank_X, y = ranking, beta = beta, Omega = NULL,
    Sigma = Sigma, weights = weights, re_position = NULL,
    ranked = TRUE
  )
  expect_equal(fixed_rank, rank_ref)

  set.seed(1)
  mixed_rank <- choiceprob_probit(
    X = rank_X, y = ranking, beta = beta, Omega = Omega,
    Sigma = Sigma, weights = weights,
    re_position = seq_len(P_r)
  )
  set.seed(1)
  mixed_rank_ref <- choiceprob_mmnp_lc(
    X = rank_X, y = ranking, beta = beta, Omega = Omega,
    Sigma = Sigma, weights = weights,
    re_position = seq_len(P_r), ranked = TRUE
  )
  expect_equal(mixed_rank, mixed_rank_ref)
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
  data <- list(
    "X" = vector("list", length = N),
    "y" = vector("list", length = N)
  )
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

  fixed <- choiceprob_probit(
    X = data$X, y = data$y, beta = beta, Sigma = Sigma,
    gamma = gamma, weights = weights
  )
  fixed_ref <- choiceprob_mmnp_ordered_lc(
    X = data$X, y = data$y, beta = beta, Omega = NULL,
    Sigma = Sigma, gamma = gamma, weights = weights,
    re_position = NULL
  )
  expect_equal(fixed, fixed_ref)
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

  dx <- c(1, 2, 0.5)
  X0 <- lapply(dx, function(d) matrix(c(0, d), ncol = 1))
  y0 <- rep(list(1L), 3)
  cov0 <- 0.7 * tcrossprod(dx) + diag(3) * 5
  corr0 <- stats::cov2cor(cov0)
  rho <- corr0[lower.tri(corr0)]
  pair <- 0.25 + asin(rho) / (2 * pi)
  ref <- c(
    no = 0.125 + sum(asin(rho)) / (4 * pi),
    fp = prod(pair), ap = prod(pair[c(1, 3)])
  )
  gcdf0 <- function(upper, corr) {
    mvtnorm::pmvnorm(upper = upper, sigma = corr,
                     algorithm = mvtnorm::Miwa())
  }
  got <- vapply(c("no", "fp", "ap"), function(cml) {
    choiceprob_mmnp_panel(
      X0, y0, 3L, cml, 0, matrix(0.7), diag(c(1, 4)), 1L,
      gcdf = gcdf0
    )
  }, numeric(1))
  expect_equal(got, ref, tolerance = 1e-7)
  expect_equal(
    choiceprob_probit(
      X = X0, y = y0, Tp = 3L, beta = 0,
      Omega = matrix(0.7), Sigma = diag(c(1, 4)),
      gcdf = gcdf0
    ),
    unname(got["no"])
  )

  all_probs <- choiceprob_probit(
    X = X0, y = NULL, Tp = 3L, beta = 0,
    Omega = matrix(0.7), Sigma = diag(c(1, 4))
  )
  checkmate::expect_matrix(all_probs, nrows = 3, ncols = 2)
  expect_equal(rowSums(all_probs), rep(1, 3))
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
  gcdf <- function(upper, corr) {
    mvtnorm::pmvnorm(
      upper = upper,
      sigma = corr,
      algorithm = mvtnorm::Miwa()
    )
  }
  ref <- choiceprob_mmnp_panel(
    X = data$X[1:2], y = data$y[1:2], Tp = 2L,
    cml = "no", beta = beta, Omega = Omega, Sigma = Sigma,
    re_position = seq_len(P_r), gcdf = gcdf, ranked = TRUE
  )
  expect_equal(
    choiceprob_probit(
      X = data$X[1:2], y = data$y[1:2], Tp = 2L,
      beta = beta, Omega = Omega, Sigma = Sigma,
      re_position = seq_len(P_r), gcdf = gcdf
    ),
    ref
  )

  beta_lc <- list(beta, -beta)
  omega_lc <- list(Omega, Omega)
  lc_args <- list(
    X = data$X[1:2], y = data$y[1:2], Tp = 2L,
    cml = "no", beta = beta_lc, Omega = omega_lc,
    Sigma = Sigma, weights = c(0.4, 0.6),
    re_position = seq_len(P_r), gcdf = gcdf
  )
  set.seed(1)
  lc_prob <- do.call(choiceprob_probit, lc_args)
  lc_args$ranked <- TRUE
  set.seed(1)
  lc_ref <- do.call(choiceprob_mmnp_panel_lc, lc_args)
  expect_equal(lc_prob, lc_ref)
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
  ### gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
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
      y_nt <- findInterval(
        U_nt, gamma_augmented, all.inside = TRUE, left.open = TRUE
      )
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

  X0 <- lapply(c(1, 2), matrix, nrow = 1)
  y0 <- rep(list(2L), 2)
  X_mat <- do.call(rbind, X0)
  V0 <- as.numeric(X_mat * 0.3)
  cov0 <- X_mat %*% 0.7 %*% t(X_mat) + diag(2) * 2
  lower0 <- -V0
  upper0 <- 1 - V0
  gcdf0 <- function(upper, corr, lower = -Inf) {
    mvtnorm::pmvnorm(
      lower = lower, upper = upper, sigma = corr,
      algorithm = mvtnorm::Miwa()
    )
  }
  ref <- mvtnorm::pmvnorm(
    lower = lower0, upper = upper0, sigma = cov0,
    algorithm = mvtnorm::Miwa()
  )
  got <- choiceprob_mmnp_ordered_panel(
    X0, y0, 2L, "no", 0.3, matrix(0.7), 2, c(0, 1), 1L,
    gcdf = gcdf0
  )
  expect_equal(got, as.numeric(ref))
  expect_equal(
    choiceprob_probit(
      X = X0, y = y0, Tp = 2L, beta = 0.3,
      Omega = matrix(0.7), Sigma = 2, gamma = c(0, 1),
      gcdf = gcdf0
    ),
    got
  )

  all_probs <- choiceprob_probit(
    X = X0, y = NULL, Tp = 2L, beta = 0.3,
    Omega = matrix(0.7), Sigma = 2, gamma = c(0, 1)
  )
  checkmate::expect_matrix(all_probs, nrows = 2, ncols = 3)
  expect_equal(rowSums(all_probs), rep(1, 2))
})

test_that("MMNP panel latent class probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  C <- length(weights)
  stopifnot(all(diff(weights) >= 0))
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
    preferences[[n]] <- oeli::rmvnorm(
      mean = beta[[class_n]], Sigma = Omega_completed
    )
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
  weight_offset <- P * C + C * P_r * (P_r + 1) / 2
  ind_weights <- weight_offset + J * (J - 1) / 2 - 1 + (1:(C - 1))

  ### calculate MMNP probabilities
  lc_args <- list(
    X = data$X, y = data$y, Tp = Tp, cml = "no", beta = beta,
    Omega = Omega, Sigma = Sigma, weights = weights,
    re_position = seq_len(P_r)
  )
  set.seed(1)
  probs <- do.call(choiceprob_mmnp_panel_lc, lc_args)
  set.seed(1)
  generic <- do.call(choiceprob_probit, lc_args)
  expect_equal(generic, probs)
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  no_position <- lc_args
  no_position$re_position <- NULL
  tail_position <- lc_args
  tail_position$re_position <- P
  set.seed(1)
  default_position <- do.call(choiceprob_mmnp_panel_lc, no_position)
  set.seed(1)
  explicit_position <- do.call(choiceprob_mmnp_panel_lc, tail_position)
  expect_equal(
    default_position,
    explicit_position
  )

  fixed_args <- list(
    X = data$X[1:2], y = data$y[1:2], Tp = 2L, cml = "no",
    beta = beta[1:2], Omega = NULL, Sigma = Sigma,
    weights = c(0.4, 0.6), re_position = NULL
  )
  fixed <- do.call(choiceprob_mmnp_panel_lc, fixed_args)
  expect_equal(do.call(choiceprob_probit, fixed_args), fixed)
  for (cml in c("fp", "ap")) {
    cml_args <- utils::modifyList(lc_args, list(cml = cml))
    set.seed(1)
    specialized <- do.call(choiceprob_mmnp_panel_lc, cml_args)
    set.seed(1)
    expect_equal(
      do.call(choiceprob_probit, cml_args),
      specialized
    )
    expect_true(all(is.finite(specialized)))
  }
})

test_that("latent class panel inputs are validated", {

  Tp <- 3L
  X <- rep(list(matrix(c(1, 0), nrow = 2, ncol = 1)), sum(Tp))
  y <- list(1L, 2L, 1L)
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
      utils::modifyList(base_args, list(Tp = 4L))
    ),
    "Sum of"
  )
  fixed_args <- utils::modifyList(base_args, list(Omega = NULL))
  class_prob <- lapply(beta, function(beta_c) {
    choiceprob_mnp(X = X, y = y, beta = beta_c, Sigma = Sigma)
  })
  for (cml in c("fp", "ap")) {
    pairs <- if (cml == "fp") {
      utils::combn(seq_len(Tp), 2L, simplify = FALSE)
    } else {
      list(1:2, 2:3)
    }
    expected <- prod(vapply(pairs, function(pair) {
      sum(base_args$weights * vapply(class_prob, function(prob_c) {
        prod(prob_c[pair])
      }, numeric(1)))
    }, numeric(1)))
    cml_args <- utils::modifyList(fixed_args, list(cml = cml))
    expect_equal(do.call(choiceprob_probit, cml_args), expected)
    expect_true(is.finite(do.call(
      choiceprob_probit,
      utils::modifyList(base_args, list(cml = cml))
    )))
  }
})

test_that("MMNP ordered panel latent class probabilities can be computed", {

  ### meta settings
  J <- 3
  P <- 2
  N <- 100
  Tp <- sample.int(5, size = N, replace = TRUE)
  weights <- c(0.2, 0.3, 0.5)
  C <- length(weights)
  stopifnot(all(diff(weights) >= 0))
  beta <- lapply(seq_len(C), function(c) rnorm(P))
  P_r <- 1
  Omega <- lapply(seq_len(C), function(c) matrix(runif(P_r), 1, 1))
  d <- rnorm(J - 2)
  ### gamma_0 = -Inf, gamma_1 = 0, gamma_2, ..., gamma_J = Inf
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
    preferences[[n]] <- oeli::rmvnorm(
      mean = beta[[class_n]], Sigma = Omega_completed
    )
  }
  for (n in seq_len(N)) {
    for (t in seq_len(Tp[n])) {
      X_nt <- matrix(rnorm(P, sd = 2), nrow = 1, ncol = P)
      V_nt <- as.numeric(X_nt %*% preferences[[n]])
      eps_nt <- stats::rnorm(n = 1, mean = 0, sd = Sigma^2)
      U_nt <- V_nt + eps_nt
      y_nt <- findInterval(
        U_nt, gamma_augmented, all.inside = TRUE, left.open = TRUE
      )
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
  lc_args <- list(
    X = data$X, y = data$y, Tp = Tp, cml = "no", beta = beta,
    Omega = Omega, Sigma = Sigma, gamma = gamma, weights = weights,
    re_position = seq_len(P_r)
  )
  set.seed(1)
  probs <- do.call(choiceprob_mmnp_ordered_panel_lc, lc_args)
  set.seed(1)
  generic <- do.call(choiceprob_probit, lc_args)
  expect_equal(generic, probs)
  checkmate::expect_numeric(
    probs, lower = 0, upper = 1, any.missing = FALSE, len = N
  )
  no_position <- lc_args
  no_position$re_position <- NULL
  tail_position <- lc_args
  tail_position$re_position <- P
  set.seed(1)
  default_position <- do.call(
    choiceprob_mmnp_ordered_panel_lc,
    no_position
  )
  set.seed(1)
  explicit_position <- do.call(
    choiceprob_mmnp_ordered_panel_lc,
    tail_position
  )
  expect_equal(
    default_position,
    explicit_position
  )

  fixed_args <- list(
    X = data$X[1:2], y = data$y[1:2], Tp = 2L, cml = "no",
    beta = beta[1:2], Omega = NULL, Sigma = Sigma,
    gamma = gamma, weights = c(0.4, 0.6), re_position = NULL
  )
  fixed <- do.call(choiceprob_mmnp_ordered_panel_lc, fixed_args)
  expect_equal(do.call(choiceprob_probit, fixed_args), fixed)
  for (cml in c("fp", "ap")) {
    cml_args <- utils::modifyList(lc_args, list(cml = cml))
    set.seed(1)
    specialized <- do.call(
      choiceprob_mmnp_ordered_panel_lc,
      cml_args
    )
    set.seed(1)
    expect_equal(
      do.call(choiceprob_probit, cml_args),
      specialized
    )
    expect_true(all(is.finite(specialized)))
  }
})

test_that("default Gaussian CDF helper relies on covariance matrices", {
  corr <- matrix(c(1, 0.3, 0.3, 1), nrow = 2)
  upper <- c(0.5, -0.2)
  expect_equal(
    as.numeric(pmvnorm_cdf_default(upper = upper, corr = corr)),
    as.numeric(mvtnorm::pmvnorm(
      upper = upper, sigma = corr, algorithm = mvtnorm::GenzBretz()
    ))
  )

  expect_equal(
    as.numeric(pmvnorm_cdf_default(
      upper = 0.7,
      corr = Matrix::Matrix(1)
    )),
    stats::pnorm(0.7)
  )

  expect_identical(
    pmvnorm_cdf_default(
      upper = numeric(),
      corr = matrix(numeric(0), nrow = 0)
    ),
    1
  )
})

test_that("panel helper utilities cover edge cases", {

  expect_error(build_panel_chunks(Tp_n = 2, cml_type = 3L))
  expect_identical(build_panel_chunks(0, 0L), list())
  expect_equal(
    build_panel_chunks(3, 0L, block = 2L),
    list(1:6)
  )
  expect_equal(
    build_panel_chunks(3, 1L),
    list(c(1L, 2L), c(1L, 3L), c(2L, 3L))
  )
  expect_equal(
    build_panel_chunks(3, 2L),
    list(c(1L, 2L), c(2L, 3L))
  )

  expect_identical(
    compute_chunk_product(
      upper = numeric(),
      corr = matrix(numeric(0), nrow = 0),
      gcdf = function(...) 0,
      chunk_indices = list()
    ),
    1
  )
  expect_equal(
    compute_chunk_product(
      upper = 0,
      corr = matrix(1),
      gcdf = function(...) 0,
      chunk_indices = list(1L),
      logarithm = TRUE
    ),
    log(.Machine$double.xmin)
  )
  expect_identical(cpp_prob_prod(c(0, 0.5)), 0)
  expect_equal(
    cpp_prob_prod(c(0, 0.5), log = TRUE),
    log(.Machine$double.xmin) + log(0.5)
  )

  independent_cdf <- function(upper, corr) {
    prod(stats::pnorm(upper))
  }
  rect <- compute_chunk_product(
    upper = c(0.5, 1),
    corr = diag(2),
    gcdf = independent_cdf,
    chunk_indices = list(1:2),
    lower = c(-0.5, 0)
  )
  rect_ref <- prod(
    stats::pnorm(c(0.5, 1)) - stats::pnorm(c(-0.5, 0))
  )
  expect_equal(rect, rect_ref)

  many_probs <- rep(0.5, 4096)
  expect_equal(
    cpp_panel_prod(many_probs, rep(1L, 4096)),
    many_probs
  )
  expect_identical(cpp_prob_prod(rep(1, 4096)), 1)
  expect_length(cpp_cml_chunks(4096L, 1L, 0L)[[1]], 4096)
  expect_length(cpp_cml_chunks(92L, 1L, 1L), 4186)
  expect_length(cpp_cml_chunks(4096L, 1L, 2L), 4095)

  large_x <- matrix(seq_len(512) / 512, ncol = 1)
  large_d <- matrix(rep(1 / sqrt(512), 512), nrow = 1)
  large_cov <- cpp_probit_cov(
    large_x,
    matrix(0, nrow = 1),
    matrix(1, nrow = 1),
    large_d,
    512L
  )
  expect_equal(as.numeric(large_cov$cov), 1)
})
