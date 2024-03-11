# test_that("probit data can be simulated", {
#
#
#   choice_formula <- choice_formula(formula = choice ~ A | 0 | C)
#
#   J <- 3
#
#   choice_alternatives <- choice_alternatives(J = J)
#
#   choice_covariates <- sample_choice_covariates(
#     choice_formula = choice_formula,
#     N = 10,
#     Tp = 1:10,
#     choice_alternatives = choice_alternatives
#   )
#
#   choice_parameters <- sample_choice_parameters(
#     choice_formula = choice_formula,
#     J = J
#   )
#
#   ranked <- FALSE
#
#   column_choice <- "choice"
#
#
#
#   choice_effects <- choice_effects(
#     choice_formula = choice_formula,
#     choice_alternatives = choice_alternatives
#   )
#
#
#   xx <- simulate_choice_data(
#     choice_covariates = choice_covariates,
#     choice_parameters = choice_parameters
#   )
#
# })

