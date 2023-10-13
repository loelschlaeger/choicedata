test_that("probit data can be simulated", {
  probit_covariates = sample_probit_covariates(
    probit_formula = probit_formula(formula = choice ~ A | 0 | C), N = 10, Tp = 1:10,
    probit_alternatives = probit_alternatives(J = 3)
  )
  probit_parameter <- probit_parameter()
  ranked <- FALSE
  column_choice <- "choice"
})

