test_that("choice_likelihood precomputes sufficient statistics", {

  choice_data <- choice_data(
    data_frame = train_choice,
    format = "wide",
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )

  choice_effects <- choice_effects(
    choice_formula = choice_formula(
      formula = choice ~ price + time + change + comfort | 0,
      error_term = "probit"
    ),
    choice_alternatives = choice_alternatives(
      J = 2, alternatives = c("A", "B")
    )
  )

  likelihood <- choice_likelihood(
    choice_data = choice_data,
    choice_effects = choice_effects
  )

  expect_s3_class(likelihood, "choice_likelihood")
  expect_true(is.choice_likelihood(likelihood))
  expect_length(likelihood$design_matrices, nrow(choice_data))
  expect_length(likelihood$choice_indices, nrow(choice_data))

  choice_parameters <- generate_choice_parameters(choice_effects)

  direct_eval <- compute_choice_likelihood(
    choice_parameters = choice_parameters,
    choice_data = choice_data,
    choice_effects = choice_effects,
    logarithm = TRUE
  )
  precomp_eval <- compute_choice_likelihood(
    choice_parameters = choice_parameters,
    choice_data = likelihood,
    choice_effects = choice_effects,
    logarithm = TRUE
  )
  expect_equal(direct_eval, precomp_eval)

  optim_space <- switch_parameter_space(choice_parameters, choice_effects)
  optim_eval <- compute_choice_likelihood(
    choice_parameters = optim_space,
    choice_data = likelihood,
    choice_effects = choice_effects,
    logarithm = TRUE
  )
  expect_equal(precomp_eval, optim_eval)
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
  expect_silent(compute_choice_likelihood(
    choice_parameters = params,
    choice_data = likelihood,
    choice_effects = ordered_effects
  ))
})

test_that("choice likelihood enables estimation across model families", {
  skip()

  estimation_specs <- list(
    list(
      label = "multinomial logit",
      seed = 101L,
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
      ),
      tol = list(beta = 0.15)
    ),
    list(
      label = "multinomial probit",
      seed = 102L,
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
      ),
      tol = list(beta = 0.25)
    ),
    list(
      label = "ordered logit",
      seed = 103L,
      N = 400L,
      choice_type = "ordered",
      choice_formula = choice_formula(
        formula = choice ~ q1 + q2 | 0 | 0,
        error_term = "logit"
      ),
      choice_alternatives = choice_alternatives(
        J = 4,
        alternatives = c("low", "medium", "high", "very_high"),
        ordered = TRUE
      ),
      parameters = choice_parameters(
        beta = c(0.25, -0.5),
        gamma = c(0, 0.6, 1.4)
      ),
      tol = list(beta = 0.2, gamma = 0.2)
    ),
    list(
      label = "ordered probit",
      seed = 104L,
      N = 400L,
      choice_type = "ordered",
      choice_formula = choice_formula(
        formula = choice ~ r1 + r2 | 0 | 0,
        error_term = "probit"
      ),
      choice_alternatives = choice_alternatives(
        J = 4,
        alternatives = c("low", "medium", "high", "very_high"),
        ordered = TRUE
      ),
      parameters = choice_parameters(
        beta = c(-0.3, 0.45),
        Sigma = 1.3,
        gamma = c(0, 0.5, 1.2)
      ),
      tol = list(beta = 0.2, gamma = 0.2, Sigma = 0.25)
    ),
    list(
      label = "ranked logit",
      seed = 105L,
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
      ),
      tol = list(beta = 0.25)
    ),
    list(
      label = "ranked probit",
      seed = 106L,
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
      ),
      tol = list(beta = 0.3)
    )
  )

  for (spec in estimation_specs) {
    set.seed(spec$seed)

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
    if (length(optim_start) == 0L) {
      next
    }
    optim_start <- optim_start + stats::rnorm(length(optim_start), sd = 0.1)

    objective <- function(par) {
      likelihood$objective(
        choice_parameters = par,
        logarithm = TRUE,
        negative = TRUE
      )
    }

    fit <- stats::optim(
      par = optim_start,
      fn = objective,
      method = "BFGS",
      control = list(maxit = 200, reltol = 1e-8)
    )

    expect_identical(fit$convergence, 0L, info = spec$label)

    estimated <- switch_parameter_space(fit$par, choice_effects)

    expect_equal(
      estimated$beta,
      spec$parameters$beta,
      tolerance = spec$tol$beta,
      check.attributes = FALSE,
      info = sprintf("beta - %s", spec$label)
    )

    if (!is.null(spec$parameters$gamma)) {
      expect_equal(
        estimated$gamma,
        spec$parameters$gamma,
        tolerance = spec$tol$gamma,
        check.attributes = FALSE,
        info = sprintf("gamma - %s", spec$label)
      )
    }

    if (!is.null(spec$parameters$Sigma) && !is.null(spec$tol$Sigma)) {
      expect_equal(
        estimated$Sigma,
        spec$parameters$Sigma,
        tolerance = spec$tol$Sigma,
        check.attributes = FALSE,
        info = sprintf("Sigma - %s", spec$label)
      )
    }
  }
})
