test_that("input checks for choice parameters work", {
  expect_error(
    choice_parameters(beta = "not_a_numeric"),
    "Input `beta` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(Omega = "not_a_numeric"),
    "Input `Omega` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(Sigma = "not_a_numeric"),
    "Input `Sigma` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(gamma = "not_a_numeric"),
    "Input `gamma` is bad: Must be of type 'numeric', not 'character'"
  )
  expect_error(
    choice_parameters(weights = "not_a_numeric"),
    "Input `weights` is bad"
  )
  expect_error(choice_parameters(beta = numeric()), "length >= 1")
  expect_error(choice_parameters(weights = 1), "length >= 2")
  expect_error(choice_parameters(beta = Inf), "finite")
  expect_error(
    choice_parameters(beta = list(1, 1:2)),
    "equal lengths"
  )
})

test_that("choice parameter can be created", {
  expect_true(is.choice_parameters(choice_parameters()))
  x <- choice_parameters(
    beta = list(c(0.2, -0.1), c(-0.3, 0.4)),
    weights = c(0.4, 0.6)
  )
  expect_true(is.choice_parameters(x))
  expect_s3_class(x, "choice_parameters")
  expect_length(x$beta, 2L)
  expect_equal(x$weights, c(0.4, 0.6))
})

test_that("choice parameters can be generated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0 + B,
      random_effects = c("B" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  set.seed(1)
  x <- generate_choice_parameters(choice_effects)
  expect_s3_class(x, "choice_parameters")
  set.seed(1)
  x_lc <- generate_choice_parameters(choice_effects, n_classes = 2L)
  expect_length(x_lc$beta, 2L)
  expect_length(x_lc$Omega, 2L)
  expect_equal(x_lc$weights, c(0.5, 0.5))

  ordered_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0
    ),
    choice_alternatives = choice_alternatives(J = 3, ordered = TRUE)
  )

  y <- generate_choice_parameters(ordered_effects)
  expect_s3_class(y, "choice_parameters")
  expect_length(y$gamma, 2L)
  expect_true(is.numeric(y$Sigma))
})

test_that("choice parameter can be validated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0 + B,
      random_effects = c("B" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(),
      choice_effects = choice_effects
    ),
    "Parameter `beta` is required"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(beta = 1:3),
      choice_effects = choice_effects
    ),
    "Parameter `Omega` is required"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        beta = list(1:3, 3:1)
      ),
      choice_effects = choice_effects
    ),
    "require latent class weights"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(beta = 1:3, Omega = diag(2)),
      choice_effects = choice_effects
    ),
    "Parameter `Sigma` is required"
  )
  lc_parameters <- choice_parameters(
    beta = list(1:3, 3:1),
    Omega = list(diag(2), 2 * diag(2)),
    Sigma = diag(3),
    weights = c(2, 3)
  )
  expect_warning(
    lc_parameters <- validate_choice_parameters(
      lc_parameters,
      choice_effects
    ),
    "normalized"
  )
  expect_equal(lc_parameters$weights, c(0.4, 0.6))
  expect_error(
    validate_choice_parameters(
      choice_parameters(
        beta = list(1:3, 3:1),
        Omega = list(diag(2), 2 * diag(2)),
        Sigma = diag(3),
        weights = c(1, 0)
      ),
      choice_effects
    ),
    "strictly positive"
  )

  ordered_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0
    ),
    choice_alternatives = choice_alternatives(J = 3, ordered = TRUE)
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(beta = 1),
      choice_effects = ordered_effects
    ),
    "Parameter `Sigma` is required"
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(beta = 1, Sigma = 1),
      choice_effects = ordered_effects
    ),
    "Parameter `gamma` is required"
  )
  expect_true(is.choice_parameters(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        beta = 1, Sigma = 1, gamma = c(0, 1)
      ),
      choice_effects = ordered_effects
    )
  ))
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(
        beta = 1, Sigma = 1, gamma = c(0, 0.5, 0.5)
      ),
      choice_effects = ordered_effects
    ),
    "strictly increasing"
  )
})

test_that("not required choice parameters are set to NULL", {
  choice_effects_no_covariates <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ 0 | 0
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  expect_true(is.choice_parameters(
    validate_choice_parameters(
      choice_parameters = choice_parameters(Sigma = diag(3)),
      choice_effects = choice_effects_no_covariates
    )
  ))
})

test_that("choice parameters can switch parameter spaces", {

  ### MNP model
  J <- 3
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B
    ),
    choice_alternatives = choice_alternatives(J = J)
  )
  choice_parameters <- generate_choice_parameters(
    choice_effects = choice_effects,
    fixed_parameters = choice_parameters(
      Sigma = diag(c(0, rep(1, J - 1))) # scale and level normalization
    )
  )
  o_space <- switch_parameter_space(choice_parameters, choice_effects)
  i_space <- switch_parameter_space(o_space, choice_effects)
  expect_identical(
    choice_parameters, i_space
  )

  ### MMNP model
  J <- 3
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B,
      random_effects = c("A" = "cn")
    ),
    choice_alternatives = choice_alternatives(J = J)
  )
  choice_parameters <- generate_choice_parameters(
    choice_effects = choice_effects,
    fixed_parameters = choice_parameters(
      Sigma = diag(c(0, rep(1, J - 1))) # scale and level normalization
    )
  )
  o_space <- switch_parameter_space(choice_parameters, choice_effects)
  i_space <- switch_parameter_space(o_space, choice_effects)
  expect_identical(
    choice_parameters, i_space
  )

  ### Latent class MMNP model
  lc_parameters <- choice_parameters(
    beta = list(choice_parameters$beta, -choice_parameters$beta),
    Omega = list(choice_parameters$Omega, 2 * choice_parameters$Omega),
    Sigma = choice_parameters$Sigma,
    weights = c(0.35, 0.65)
  )
  lc_o_space <- switch_parameter_space(lc_parameters, choice_effects)
  lc_i_space <- switch_parameter_space(
    as.numeric(lc_o_space),
    choice_effects
  )
  expect_equal(lc_i_space, lc_parameters)
  expect_true("w_2" %in% names(lc_o_space))

  ### Ordered model
  J <- 3
  ordered_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0
    ),
    choice_alternatives = choice_alternatives(J = J, ordered = TRUE)
  )
  ordered_parameters <- generate_choice_parameters(
    choice_effects = ordered_effects,
    fixed_parameters = choice_parameters(
      Sigma = 1,
      gamma = c(0, 1)
    )
  )
  o_space_ord <- switch_parameter_space(ordered_parameters, ordered_effects)
  i_space_ord <- switch_parameter_space(o_space_ord, ordered_effects)
  expect_equal(ordered_parameters$Sigma, i_space_ord$Sigma)
  expect_equal(ordered_parameters$gamma, i_space_ord$gamma)
  ordered_lc <- choice_parameters(
    beta = list(ordered_parameters$beta, -ordered_parameters$beta),
    Sigma = ordered_parameters$Sigma,
    gamma = ordered_parameters$gamma,
    weights = c(0.25, 0.75)
  )
  ordered_lc_o <- switch_parameter_space(ordered_lc, ordered_effects)
  expect_equal(
    switch_parameter_space(as.numeric(ordered_lc_o), ordered_effects),
    ordered_lc
  )
})
