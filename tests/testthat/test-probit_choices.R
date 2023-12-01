test_that("simulation of probit choices works", {

  formula <- choice ~ A | B
  J <- 3
  N <- 10
  probit_alternatives <- probit_alternatives(J = J)

  probit_parameter <- sample_probit_parameter(
    x = probit_parameter(),
    formula = formula,
    J = J,
    N = N
  )

  probit_covariates <- sample_probit_covariates(
    probit_formula = probit_formula(formula = formula),
    N = N,
    probit_alternatives = probit_alternatives
  )

  probit_choice_set <- probit_choice_set(
    probit_alternatives = probit_alternatives
  )

  simulate_probit_choices(
    probit_parameter = probit_parameter,
    probit_covariates = probit_covariates,
    probit_choice_set = probit_choice_set
  )


})
