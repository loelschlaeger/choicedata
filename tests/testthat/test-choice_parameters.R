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
      random_effects = c("B" = "cn"),
      latent_class_effects = "B"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  set.seed(1)
  x <- generate_choice_parameters(choice_effects)
  expect_s3_class(x, "choice_parameters")
  set.seed(1)
  x_lc <- generate_choice_parameters(choice_effects, C = 2L)
  expect_length(x_lc$beta, 2L)
  expect_length(x_lc$Omega, 2L)
  expect_equal(x_lc$weights, c(0.5, 0.5))
  expect_identical(x_lc$beta[[1]][["A"]], x_lc$beta[[2]][["A"]])
  expect_false(identical(x_lc$beta[[1]][["B_B"]], x_lc$beta[[2]][["B_B"]]))
  expect_error(
    generate_choice_parameters(
      choice_effects(
        choice_formula = choice_formula(formula = choice ~ A | 0 + B),
        choice_alternatives = choice_alternatives(J = 3)
      ),
      C = 2L
    ),
    "latent_class_effects"
  )

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

test_that("a named partial beta fixes only the named effects", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ x | y | z, latent_class_effects = "x"
    ),
    choice_alternatives = choice_alternatives(J = 3)
  )
  set.seed(1)
  parameters <- generate_choice_parameters(
    choice_effects = choice_effects,
    fixed_parameters = choice_parameters(beta = c(x = 1))
  )
  expect_length(parameters$beta, nrow(choice_effects))
  expect_identical(parameters$beta[["x"]], 1)
  mixture <- generate_choice_parameters(
    choice_effects = choice_effects,
    fixed_parameters = choice_parameters(beta = list(c(x = 1), c(x = -1))),
    C = 2
  )
  expect_identical(mixture$beta[[2]][["x"]], -1)
  reordered <- validate_choice_parameters(
    choice_parameters = choice_parameters(beta = rev(parameters$beta)),
    choice_effects = choice_effects,
    allow_missing = TRUE
  )
  expect_identical(reordered$beta, parameters$beta)
  misnamed <- parameters$beta
  names(misnamed) <- rep("w", length(misnamed))
  expect_error(
    validate_choice_parameters(
      choice_parameters = choice_parameters(beta = misnamed),
      choice_effects = choice_effects,
      allow_missing = TRUE
    ),
    "beta"
  )
  expect_error(
    generate_choice_parameters(
      choice_effects = choice_effects,
      fixed_parameters = choice_parameters(beta = c(w = 1))
    ),
    "beta"
  )
})

test_that("choice parameter can be validated", {
  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0 + B,
      random_effects = c("B" = "cn"),
      latent_class_effects = c("A", "B")
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

  ### multinomial probit model
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
      ### apply scale and level normalization
      Sigma = diag(c(0, rep(1, J - 1)))
    )
  )
  o_space <- switch_parameter_space(choice_parameters, choice_effects)
  i_space <- switch_parameter_space(o_space, choice_effects)
  expect_identical(
    choice_parameters, i_space
  )

  ### mixed multinomial probit model
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
      ### apply scale and level normalization
      Sigma = diag(c(0, rep(1, J - 1)))
    )
  )
  o_space <- switch_parameter_space(choice_parameters, choice_effects)
  i_space <- switch_parameter_space(o_space, choice_effects)
  expect_identical(
    choice_parameters, i_space
  )

  ### latent class mixed multinomial probit model
  class_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | B,
      random_effects = c("A" = "cn"),
      latent_class_effects = c("A", "B", "ASC")
    ),
    choice_alternatives = choice_alternatives(J = J)
  )
  lc_parameters <- choice_parameters(
    beta = list(choice_parameters$beta, -choice_parameters$beta),
    Omega = list(choice_parameters$Omega, 2 * choice_parameters$Omega),
    Sigma = choice_parameters$Sigma,
    weights = c(0.35, 0.65)
  )
  lc_o_space <- switch_parameter_space(lc_parameters, class_effects)
  lc_i_space <- switch_parameter_space(
    as.numeric(lc_o_space),
    class_effects
  )
  expect_equal(lc_i_space, lc_parameters)
  expect_true("w_2" %in% names(lc_o_space))
  expect_error(
    switch_parameter_space(lc_parameters, choice_effects),
    "latent_class_effects"
  )

  ### effects with and without latent classes
  mixed_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A + C | 0,
      random_effects = c("A" = "cn", "C" = "cn"),
      latent_class_effects = "C"
    ),
    choice_alternatives = choice_alternatives(J = J)
  )
  set.seed(1)
  mixed_parameters <- generate_choice_parameters(mixed_effects, C = 2)
  expect_identical(
    mixed_parameters$Omega[[1]]["A", "A"], mixed_parameters$Omega[[2]]["A", "A"]
  )
  expect_identical(mixed_parameters$Omega[[1]]["A", "C"], 0)
  mixed_o_space <- switch_parameter_space(mixed_parameters, mixed_effects)
  expect_named(
    mixed_o_space,
    c(
      "beta_1_1", "beta_2_1", "beta_1", "o_1_1", "o_2_1", "o_1", "l_2", "l_3",
      "w_2"
    )
  )
  expect_equal(
    switch_parameter_space(as.numeric(mixed_o_space), mixed_effects),
    mixed_parameters
  )
  expect_error(
    validate_choice_parameters(
      choice_parameters(
        beta = list(c(A = 1, C = 1), c(A = 2, C = 1)),
        Omega = mixed_parameters$Omega, Sigma = mixed_parameters$Sigma,
        weights = c(0.5, 0.5)
      ),
      mixed_effects
    ),
    "must not differ by class"
  )
  correlated <- mixed_parameters$Omega
  correlated[[1]]["A", "C"] <- correlated[[1]]["C", "A"] <- 0.1
  expect_error(
    validate_choice_parameters(
      choice_parameters(
        beta = mixed_parameters$beta, Omega = correlated,
        Sigma = mixed_parameters$Sigma, weights = c(0.5, 0.5)
      ),
      mixed_effects
    ),
    "uncorrelated"
  )

  ### ordered model
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
  ordered_class_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ A | 0, latent_class_effects = "A"
    ),
    choice_alternatives = choice_alternatives(J = J, ordered = TRUE)
  )
  ordered_lc <- choice_parameters(
    beta = list(ordered_parameters$beta, -ordered_parameters$beta),
    Sigma = ordered_parameters$Sigma,
    gamma = ordered_parameters$gamma,
    weights = c(0.25, 0.75)
  )
  ordered_lc_o <- switch_parameter_space(ordered_lc, ordered_class_effects)
  expect_equal(
    switch_parameter_space(
      as.numeric(ordered_lc_o), ordered_class_effects
    ),
    ordered_lc
  )
})

test_that("uncorrelated random effects constrain Omega", {
  choice_effects <- choice_effects(
    choice_formula(
      choice ~ A + B + C + D | 0,
      random_effects = c(A = "cn", B = "n", C = "ln", D = "cln-")
    ),
    choice_alternatives(J = 2)
  )
  Omega <- diag(1:4)
  Omega[1, 4] <- Omega[4, 1] <- 0.2
  parameters <- choice_parameters(
    beta = seq_len(4),
    Omega = Omega,
    Sigma = diag(c(0, 1))
  )

  validated <- validate_choice_parameters(parameters, choice_effects)
  optimization <- switch_parameter_space(validated, choice_effects)
  expect_length(optimization[startsWith(names(optimization), "o_")], 5L)
  expect_equal(
    switch_parameter_space(optimization, choice_effects),
    validated
  )

  invalid <- parameters
  invalid$Omega[1, 2] <- invalid$Omega[2, 1] <- 0.1
  expect_error(
    validate_choice_parameters(invalid, choice_effects),
    'Omega\\["A", "B"\\]'
  )

  generated <- generate_choice_parameters(
    choice_effects,
    fixed_parameters = choice_parameters(Sigma = diag(c(0, 1)))
  )
  expect_equal(unname(generated$Omega[2, -2]), rep(0, 3))
  expect_equal(unname(generated$Omega[3, -3]), rep(0, 3))
})
