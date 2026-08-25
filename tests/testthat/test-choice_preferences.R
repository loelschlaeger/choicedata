test_that("choice preferences can be generated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price | income | comfort,
      error_term = "probit",
      random_effects = c(
        "price" = "cn",
        "income" = "cln+",
        "comfort" = "cln-"
      )
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  P <- compute_P(choice_effects)
  re_position <- which(!is.na(choice_effects$mixing))
  P_r <- length(re_position)
  Omega <- matrix(0.2, nrow = P_r, ncol = P_r)
  diag(Omega) <- 1
  params <- choice_parameters(
    beta = seq_len(P),
    Omega = Omega,
    Sigma = diag(3)
  )
  ids <- generate_choice_identifiers(N = 4)
  expected <- matrix(params$beta, nrow = 4, ncol = P, byrow = TRUE)
  set.seed(1)
  latent <- oeli::rmvnorm(
    n = 4,
    mean = params$beta[re_position],
    Sigma = params$Omega
  )
  mixing <- as.character(choice_effects$mixing[re_position])
  latent[, mixing == "cln+"] <- exp(latent[, mixing == "cln+"])
  latent[, mixing == "cln-"] <- -exp(latent[, mixing == "cln-"])
  expected[, re_position] <- latent
  set.seed(1)
  choice_preferences <- generate_choice_preferences(
    choice_effects = choice_effects,
    choice_parameters = params,
    choice_identifiers = ids
  )
  expect_true(
    is.choice_preferences(choice_preferences)
  )
  expect_s3_class(choice_preferences, "tbl_df")
  expect_equal(
    unname(as.matrix(choice_preferences[-1])),
    expected
  )
  prefs_list <- split_choice_preferences(choice_preferences)
  expect_length(prefs_list, nrow(choice_preferences))
  expect_equal(
    prefs_list[[1]],
    stats::setNames(
      as.numeric(choice_preferences[1, -1, drop = TRUE]),
      colnames(choice_preferences)[-1]
    )
  )

  beta_lc <- list(params$beta, params$beta + 1)
  weights <- c(0.4, 0.6)
  panel_ids <- generate_choice_identifiers(N = 4, Tp = rep(2L, 4))
  N <- length(read_Tp(panel_ids))
  params_lc <- choice_parameters(
    beta = beta_lc,
    Omega = list(matrix(0, P_r, P_r), matrix(0, P_r, P_r)),
    Sigma = diag(3),
    weights = weights
  )
  set.seed(1)
  class <- sample.int(2, N, replace = TRUE, prob = weights)
  expected_lc <- do.call(rbind, beta_lc[class])
  positive <- re_position[mixing == "cln+"]
  negative <- re_position[mixing == "cln-"]
  expected_lc[, positive] <- exp(
    expected_lc[, positive, drop = FALSE]
  )
  expected_lc[, negative] <- -exp(
    expected_lc[, negative, drop = FALSE]
  )
  set.seed(1)
  preferences_lc <- generate_choice_preferences(
    choice_effects, params_lc, panel_ids
  )
  expect_equal(unname(as.matrix(preferences_lc[-1])), expected_lc)

  fixed_effects <- choice_effects(
    choice_formula(choice ~ price, error_term = "probit"),
    choice_alternatives(J = 3)
  )
  P_fixed <- compute_P(fixed_effects)
  beta_fixed <- list(rep(-1, P_fixed), rep(1, P_fixed))
  fixed_params <- choice_parameters(
    beta = beta_fixed, Sigma = diag(3), weights = weights
  )
  set.seed(1)
  class <- sample.int(2, N, replace = TRUE, prob = weights)
  set.seed(1)
  preferences_fixed <- generate_choice_preferences(
    fixed_effects, fixed_params, panel_ids
  )
  expect_equal(
    unname(as.matrix(preferences_fixed[-1])),
    do.call(rbind, beta_fixed[class])
  )

  draws <- matrix(
    c(-1, 0.5, 0.25, 0.75, -0.5, 0.1),
    nrow = 2,
    byrow = TRUE
  )
  chol <- diag(c(0.5, 0.4, 0.3))
  chol[1, 2] <- 0.1
  beta <- c(0.2, -0.3, 0.1)
  type <- c(0L, 1L, -1L)
  eta <- sweep(draws %*% chol, 2, beta, "+")
  eta[, 2] <- exp(eta[, 2])
  eta[, 3] <- -exp(eta[, 3])
  averaged <- cpp_average_draws(
    draws = draws,
    beta = beta,
    position = seq_along(beta),
    compute = function(x) x,
    chol = chol,
    type = type
  )
  expect_equal(averaged, colMeans(eta))
})
