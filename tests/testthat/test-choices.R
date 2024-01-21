test_that("simulation of probit choices works", {

  J <- 2
  N <- 100
  formula <- choice ~ X | Y | Z

  choice_parameters <- sample_choice_parameters(formula = formula, J = J, N = N)

  choice_alternatives <- choice_alternatives(J = J)

  choice_formula <- choice_formula(formula = formula)

  choice_covariates <- sample_choice_covariates(
    choice_formula = choice_formula,
    N = N,
    choice_alternatives = choice_alternatives
  )

  choice_set <- choice_set(choice_alternatives = choice_alternatives)

  choices <- simulate_choices(
    choice_parameters = choice_parameters,
    choice_covariates = choice_covariates,
    choice_set = choice_set
  )
})
