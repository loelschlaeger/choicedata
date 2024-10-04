
rm(list = ls())
devtools::load_all()

# specify choice formula, types of covariates and random effects

choice_formula <- choice_formula(
  formula = transport ~ cost | income | time,
  error_term = "probit"
  # re = c("income", "cost+")
)

# specify number and names of alternatives and base alternative

choice_alternatives <- choice_alternatives(
  J = 3,
  alternatives = c("bike", "bus", "car"),
  base = "bike"
)

# use formula and alternatives to derive model effects

choice_effects <- choice_effects(choice_formula, choice_alternatives)

# using the formula and the alternatives, generate covariates

choice_covariates <- generate_choice_covariates(choice_effects)

head(choice_covariates)

design_matrices <- design_matrices(choice_covariates, choice_effects)

as.data.frame(design_matrices)

# generate choice parameters

choice_parameters <- generate_choice_parameters(
  choice_effects = choice_effects
)

print(choice_parameters)

as.vector(choice_parameters)

# simulate choice data

choice_data <- simulate_choice_data(
  choice_covariates = choice_covariates,
  choice_parameters = choice_parameters
)

head(choice_data)
