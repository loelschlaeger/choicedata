
rm(list = ls())
devtools::load_all()
devtools::document()

# specify number and names of alternatives and base alternative

(choice_alternatives <- choice_alternatives(
  J = 3,
  alternatives = c("bike", "bus", "car"),
  base = "bike"
))

# specify choice formula, types of covariates and random effects

(choice_formula <- choice_formula(
  formula = transport ~ cost | income | time,
  error_term = "probit"
  # re = c("income", "cost+")
))

# use formula and alternatives to derive model effects

(choice_effects <- choice_effects(
  choice_formula = choice_formula,
  choice_alternatives = choice_alternatives
))

# using the formula and the alternatives, generate covariates

choice_covariates <- generate_choice_covariates(choice_effects)

head(choice_covariates)

design_matrices <- df_to_design_matrices(choice_covariates, choice_effects)

# generate choice parameters

choice_parameters <- generate_choice_parameters(
  choice_effects = choice_effects
)

print(choice_parameters)

# TODO
# transform_choice_parameters(
#   choice_parameters, choice_effects = choice_effects, mode = "vector"
# )

# simulate choice preferences

choice_preferences <- simulate_choice_preferences(
  choice_parameters, choice_effects
)

# simulate choices

choice_response <- simulate_choice_response(
  choice_effects = choice_effects,
  choice_covariates = choice_covariates,
  choice_parameters = choice_parameters,
  choice_preferences = choice_preferences
)

# simulate choice data

choice_data <- simulate_choice_data(
  choice_covariates = choice_covariates,
  choice_parameters = choice_parameters
)

head(choice_data)



# tmp

rm(list=ls())
devtools::load_all()


parameters <- list(
  beta = c(-1, 1, 3),
  Omega = matrix(0, 3, 3),
  Sigma = matrix(c(0, 0, 0, 0, 1, 0.5, 0, 0.5, 1), 3, 3) + 1,
  mix = FALSE
)
data <- data_sim_tmp(
  parameters
)



pars_2_x <- function(beta, Omega, Sigma, mix) {
  o <- if(mix) oeli::cov_to_chol(Omega) else numeric()
  l <- oeli::cov_to_chol(Sigma[-1, -1, drop = FALSE])[-1]
  c(beta, o, l)
}

x_2_pars <- function(x, J, K, mix) {
  at <- if (mix) cumsum(c(K + 1, J * (J + 1) / 2)) else K + 1
  pars <- oeli::split_vector_at(x, at)
  names(pars) <- if (mix) c("beta", "Omega", "Sigma") else c("beta", "Sigma")
  if (mix) {
    pars[["Omega"]] <- oeli::chol_to_cov(pars[["Omega"]])
  } else {
    pars[["Omega"]] <- matrix(0, K, K)
  }
  pars[["Sigma"]] <- oeli::undiff_cov(oeli::chol_to_cov(c(1, pars[["Sigma"]])))
  return(pars)
}

x <- pars_2_x(parameters$beta, parameters$Omega, parameters$Sigma, parameters$mix)

x_init <- x


nlm_out <- nlm(choice_likelihood_tmp, x_init, data = data, print.level = 2, iterlim = 1000)

estimation <- x_2_pars(nlm_out$estimate, J = 3, K = 3, mix = F)

true_pars <- x_2_pars(x, J = 3, K = 3, F)


(result <- list("estimation" = estimation, "true" = true_pars))










