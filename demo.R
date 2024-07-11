
# specify choice formula, types of covariates and random effects

choice_formula <- choice_formula(
  formula = transport ~ cost | income | time,
  re = c("income", "cost+")
)

# specify number and names of alternatives and base alternative

choice_alternatives <- choice_alternatives(
  J = 3,
  alternatives = c("bike", "bus", "car"),
  base = "bike"
)

# use formula and alternatives to derive model effects

choice_effects(choice_formula, choice_alternatives)

# using the formula and the alternatives, simulate covariates

choice_covariates <- generate_choice_covariates(
  choice_formula = choice_formula,
  N = 100,
  Tp = 1,
  choice_alternatives = choice_alternatives
)

head(choice_covariates)

choice_parameters <- choice_parameters(
  model_type = "probit"
)

choice_parameters <- generate_choice_parameters(
  fixed_parameters = choice_parameters,
  choice_formula = choice_formula,
  J = 3,
  N = 100
)

print(choice_parameters)

as.vector(choice_parameters)

choice_data <- simulate_choice_data(
  choice_covariates = choice_covariates,
  choice_parameters = choice_parameters
)

head(choice_data)
