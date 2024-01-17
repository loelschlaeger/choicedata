test_that("probit data can be simulated", {
  choice_covariates = sample_choice_covariates(
    choice_formula = choice_formula(formula = choice ~ A | 0 | C),
    N = 10, Tp = 1:10,
    choice_alternatives = choice_alternatives(J = 3)
  )
  choice_parameters <- choice_parameters()
  ranked <- FALSE
  column_choice <- "choice"
})

