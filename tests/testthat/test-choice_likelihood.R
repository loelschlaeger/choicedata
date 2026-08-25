test_that("choice_likelihood precomputes sufficient statistics", {

  choice_data <- choice_data(
    data_frame = train_choice[1:4, ],
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price + time + change + comfort | 0,
      error_term = "probit",
      random_effects = c(price = "cn")
    ),
    choice_alternatives = choice_alternatives(
      J = 2, alternatives = c("A", "B")
    ),
    choice_data = choice_data
  )

  set.seed(1)
  likelihood <- choice_likelihood(
    choice_data = choice_data,
    choice_effects = choice_effects
  )

  expect_s3_class(likelihood, "choice_likelihood")
  expect_true(is.choice_likelihood(likelihood))
  expect_length(likelihood$design_matrices, nrow(choice_data))
  expect_length(likelihood$choice_indices, nrow(choice_data))

  missing_frame <- train_choice[1:4, ]
  missing_frame$choice[2] <- NA_character_
  missing_data <- choice_data(
    data_frame = missing_frame,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  missing_likelihood <- choice_likelihood(
    choice_data = missing_data,
    choice_effects = choice_effects
  )
  expect_length(missing_likelihood$design_matrices, 3L)
  expect_length(missing_likelihood$choice_indices, 3L)
  expect_equal(attr(missing_likelihood, "Tp"), 3L)
  expect_identical(
    attr(missing_likelihood$design_matrices, "choice_type"),
    "discrete"
  )

  choice_parameters <- generate_choice_parameters(choice_effects)

  all_missing_frame <- train_choice[1:4, ]
  all_missing_frame$choice <- NA_character_
  all_missing_data <- choice_data(
    data_frame = all_missing_frame,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  all_missing_likelihood <- choice_likelihood(
    choice_data = all_missing_data,
    choice_effects = choice_effects
  )
  expect_equal(
    compute_choice_likelihood(
      choice_parameters,
      all_missing_likelihood
    ),
    0
  )
  expect_equal(
    compute_choice_likelihood(
      choice_parameters,
      all_missing_likelihood,
      logarithm = FALSE
    ),
    1
  )

  set.seed(1)
  precomp_eval <- compute_choice_likelihood(
    choice_parameters = choice_parameters,
    choice_likelihood = likelihood,
    logarithm = TRUE
  )

  optim_space <- switch_parameter_space(choice_parameters, choice_effects)
  set.seed(1)
  optim_eval <- compute_choice_likelihood(
    choice_parameters = optim_space,
    choice_likelihood = likelihood,
    logarithm = TRUE
  )
  expect_equal(precomp_eval, optim_eval)
  set.seed(1)
  lc_parameters <- generate_choice_parameters(
    choice_effects,
    n_classes = 2L
  )
  lc_vector <- as.numeric(
    switch_parameter_space(lc_parameters, choice_effects)
  )
  set.seed(1)
  lc_eval <- compute_choice_likelihood(
    choice_parameters = lc_parameters,
    choice_likelihood = likelihood
  )
  set.seed(1)
  expect_equal(
    compute_choice_likelihood(
      choice_parameters = lc_vector,
      choice_likelihood = likelihood
    ),
    lc_eval
  )
  set.seed(1)
  expect_equal(
    compute_choice_likelihood(
      choice_parameters = choice_parameters,
      choice_likelihood = likelihood,
      logarithm = FALSE
    ),
    exp(precomp_eval)
  )

  logit_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price + time + change + comfort | 0,
      error_term = "logit",
      random_effects = c(price = "cn")
    ),
    choice_alternatives = choice_alternatives(
      J = 2, alternatives = c("A", "B")
    ),
    choice_data = choice_data
  )
  set.seed(1)
  logit_likelihood <- choice_likelihood(
    choice_data = choice_data,
    choice_effects = logit_effects,
    n_draws = 8L
  )
  stored_args <- get(
    "prob_args",
    envir = environment(logit_likelihood$objective)
  )
  expect_equal(dim(stored_args$draws), c(8L, 1L))
  P <- compute_P(logit_effects)
  logit_parameters <- choice_parameters(
    beta = list(numeric(P), rep(0.1, P)),
    Omega = list(matrix(0.2), matrix(0.3)),
    weights = c(0.4, 0.6)
  )
  logit_vector <- as.numeric(
    switch_parameter_space(logit_parameters, logit_effects)
  )
  first_value <- compute_choice_likelihood(
    choice_parameters = logit_vector,
    choice_likelihood = logit_likelihood
  )
  second_value <- compute_choice_likelihood(
    choice_parameters = logit_vector,
    choice_likelihood = logit_likelihood
  )
  expect_identical(second_value, first_value)
  expect_equal(
    compute_choice_likelihood(
      choice_parameters = logit_vector,
      choice_likelihood = logit_likelihood,
      cml = "no"
    ),
    first_value
  )
  expect_error(
    compute_choice_likelihood(
      choice_parameters = logit_vector,
      choice_likelihood = logit_likelihood,
      cml = "fp"
    ),
    "only available for Probit"
  )
})

test_that("choice likelihood handles ordered data", {
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

  likelihood <- choice_likelihood(
    choice_data = ch_data,
    choice_effects = ordered_effects
  )
  expect_s3_class(likelihood, "choice_likelihood")

  params <- choice_parameters(
    Sigma = 1,
    gamma = c(0, 1)
  )
  log_value <- compute_choice_likelihood(
    choice_parameters = params,
    choice_likelihood = likelihood
  )
  expect_true(is.finite(log_value))
  expect_equal(
    compute_choice_likelihood(
      choice_parameters = params,
      choice_likelihood = likelihood,
      logarithm = FALSE
    ),
    exp(log_value)
  )
})

test_that("choice likelihood enables estimation across model families", {
  estimation_specs <- list(
    list(
      label = "multinomial logit",
      N = 300L,
      choice_type = "discrete",
      choice_formula = choice_formula(
        formula = choice ~ x1 + x2 | 0 | 0,
        error_term = "logit"
      ),
      choice_alternatives = choice_alternatives(
        J = 3,
        alternatives = c("A", "B", "C")
      ),
      parameters = choice_parameters(
        beta = c(0.5, -0.35)
      )
    ),
    list(
      label = "multinomial probit",
      N = 300L,
      choice_type = "discrete",
      choice_formula = choice_formula(
        formula = choice ~ z1 + z2 | 0 | 0,
        error_term = "probit"
      ),
      choice_alternatives = choice_alternatives(
        J = 2,
        alternatives = c("A", "B")
      ),
      parameters = choice_parameters(
        beta = c(-0.4, 0.3),
        Sigma = matrix(c(0, 0, 0, 1), nrow = 2)
      )
    ),
    list(
      label = "ordered logit",
      N = 400L,
      choice_type = "ordered",
      choice_formula = choice_formula(
        formula = choice ~ q1 + q2 | 0 | 0,
        error_term = "logit"
      ),
      choice_alternatives = choice_alternatives(
        J = 4,
        alternatives = c("low", "medium", "high", "top"),
        ordered = TRUE
      ),
      parameters = choice_parameters(
        beta = c(0.25, -0.5),
        gamma = c(0, 0.6, 1.4)
      )
    ),
    list(
      label = "ordered probit",
      N = 400L,
      choice_type = "ordered",
      choice_formula = choice_formula(
        formula = choice ~ r1 + r2 | 0 | 0,
        error_term = "probit"
      ),
      choice_alternatives = choice_alternatives(
        J = 4,
        alternatives = c("low", "medium", "high", "top"),
        ordered = TRUE
      ),
      parameters = choice_parameters(
        beta = c(-0.3, 0.45),
        Sigma = 1.3,
        gamma = c(0, 0.5, 1.2)
      )
    ),
    list(
      label = "ranked logit",
      N = 250L,
      choice_type = "ranked",
      choice_formula = choice_formula(
        formula = choice ~ s1 + s2 | 0 | 0,
        error_term = "logit"
      ),
      choice_alternatives = choice_alternatives(
        J = 3,
        alternatives = c("A", "B", "C")
      ),
      parameters = choice_parameters(
        beta = c(0.35, -0.25)
      )
    ),
    list(
      label = "ranked probit",
      N = 150L,
      choice_type = "ranked",
      choice_formula = choice_formula(
        formula = choice ~ t1 + t2 | 0 | 0,
        error_term = "probit"
      ),
      choice_alternatives = choice_alternatives(
        J = 3,
        alternatives = c("A", "B", "C")
      ),
      parameters = choice_parameters(
        beta = c(-0.45, 0.4),
        Sigma = matrix(
          c(0, 0, 0,
            0, 1, 0.2,
            0, 0.2, 1.5),
          nrow = 3,
          byrow = TRUE
        )
      )
    )
  )

  for (spec in estimation_specs) {
    set.seed(1)

    choice_effects <- choice_effects(
      choice_formula = spec$choice_formula,
      choice_alternatives = spec$choice_alternatives
    )

    sample_size <- if (!is.null(spec$N)) spec$N else 200L
    identifiers <- generate_choice_identifiers(N = sample_size)
    choice_data <- generate_choice_data(
      choice_effects = choice_effects,
      choice_identifiers = identifiers,
      choice_parameters = spec$parameters,
      column_choice = "choice",
      choice_type = spec$choice_type
    )

    likelihood <- choice_likelihood(
      choice_data = choice_data,
      choice_effects = choice_effects
    )

    true_vector <- switch_parameter_space(spec$parameters, choice_effects)
    optim_start <- as.numeric(true_vector)
    expect_true(length(optim_start) > 0L, info = spec$label)
    optim_start <- optim_start + stats::rnorm(length(optim_start), sd = 0.1)

    objective <- function(par) {
      likelihood$objective(
        choice_parameters = par,
        logarithm = TRUE,
        negative = TRUE
      )
    }
    start_value <- objective(optim_start)

    set.seed(1)
    fit <- stats::optim(
      par = optim_start,
      fn = objective,
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    )
    set.seed(1)
    repeated_fit <- stats::optim(
      par = optim_start,
      fn = objective,
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    )

    expect_identical(fit$convergence, 0L, info = spec$label)
    expect_true(is.finite(fit$value), info = spec$label)
    expect_true(fit$value <= start_value, info = spec$label)
    expect_equal(repeated_fit$par, fit$par, info = spec$label)
    expect_equal(repeated_fit$value, fit$value, info = spec$label)

    estimated <- switch_parameter_space(fit$par, choice_effects)
    expect_true(is.choice_parameters(estimated), info = spec$label)
  }
})
