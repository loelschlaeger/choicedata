test_that("choice probabilities for binary probit model can be computed", {

  formula <- choice ~ X | Y | Z
  J <- 2
  N <- 100


  choice_parameters <- sample_choice_parameters(formula = formula, J = J, N = N)

  choice_formula <- choice_formula(formula = formula)

  choice_alternatives <- choice_alternatives(J = J)

  choice_covariates <- sample_choice_covariates(
    choice_formula = choice_formula,
    N = N,
    choice_alternatives = choice_alternatives
  )

  choice_set <- choice_set(choice_alternatives = choice_alternatives)

  choice_data <- simulate_choice_data(
    choice_covariates = choice_covariates,
    choice_parameters = choice_parameters,
    choice_set = choice_set
  )
})
