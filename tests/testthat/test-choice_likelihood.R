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
    "unordered"
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
    C = 2L
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
    "only available for probit"
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

