choice_likelihood <- function(
  choice_parameters,
  choice_data,
  model_type = "probit"
) {

  probs <- choice_probabilities(
    choice_parameters = choice_parameters,
    choice_data = choice_data,
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    model_type = model_type
  )

}

