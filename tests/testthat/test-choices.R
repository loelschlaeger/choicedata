test_that("simulation of probit choices works", {
  J <- 3
  choice_alternatives <- choice_alternatives(J = J)
  choice_formula <- choice_formula(formula <- choice ~ X | Y | Z)
  choice_parameters <- sample_choice_parameters(
    choice_formula = choice_formula, J = 3
  )
  choice_covariates <- sample_choice_covariates(
    choice_formula = choice_formula,
    N = 10,
    Tp = 1:10,
    choice_alternatives = choice_alternatives
  )
  choices <- simulate_choices(
    choice_parameters = choice_parameters,
    choice_covariates = choice_covariates
  )
  choices <- as.data.frame(choices)
  expect_s3_class(choices, "choices")
})
