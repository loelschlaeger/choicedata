# Define and compute choice likelihood

These functions prepare and evaluate the likelihood contribution of
observed choices for a given choice model.

- `choice_likelihood()` pre-computes the design matrices and choice
  indices implied by `choice_data` and `choice_effects`. The returned
  object stores these quantities so that repeated likelihood evaluations
  during maximum likelihood estimation avoid redundant work. Occasions
  without a response are omitted; an entirely unobserved data set has
  neutral likelihood one.

- `compute_choice_likelihood()` evaluates the (log-)likelihood for given
  `choice_parameters` and a pre-computed `choice_likelihood` object.

## Usage

``` r
choice_likelihood(
  choice_data,
  choice_effects,
  choice_identifiers = extract_choice_identifiers(choice_data),
  input_checks = TRUE,
  ...
)

compute_choice_likelihood(
  choice_parameters,
  choice_likelihood,
  logarithm = TRUE,
  negative = FALSE,
  ...
)
```

## Arguments

- choice_data:

  \[`choice_data`\]  
  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  object with the observed choices.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object that determines the model effects.

- choice_identifiers:

  \[`choice_identifiers`\]  
  A
  [`choice_identifiers`](https://loelschlaeger.de/choicedata/reference/choice_identifiers.md)
  object. The default extracts identifiers from `choice_data`.

- input_checks:

  \[`logical(1)`\]  
  Forwarded to the underlying probability engine to control additional
  input validation.

- ...:

  Additional probability arguments. Common choices are `draws` or
  `n_draws` for simulated mixed models and `cml = "no"`, `"fp"`, or
  `"ap"` for Probit panels. Arguments supplied while computing override
  those stored by `choice_likelihood()`. Omitted simulation draws are
  generated once as standard normal draws and reused for every
  evaluation.

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A
  [`choice_parameters`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  object or a numeric vector in optimization space. Numeric input is
  converted with
  [`switch_parameter_space()`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md).

- choice_likelihood:

  \[`choice_likelihood`\]  
  A pre-computed object returned by `choice_likelihood()`.

- logarithm:

  \[`logical(1)`\]  
  Return the log-likelihood? If `FALSE`, the likelihood is returned.

- negative:

  \[`logical(1)`\]  
  Return the negative (log-)likelihood? Useful for minimization
  routines.

## Value

`choice_likelihood()` returns an object of class `choice_likelihood`,
which is a `list` containing the design matrices, choice indices, and
identifiers. `compute_choice_likelihood()` returns a single numeric
value with the (negative) log-likelihood or likelihood, depending on
`logarithm` and `negative`.

## Examples

``` r
data(train_choice)

choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | time,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(
    J = 2, alternatives = c("A", "B")
  )
)

choice_data <- choice_data(
  data_frame = train_choice,
  format = "wide",
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = "occasionID"
)

likelihood <- choice_likelihood(
  choice_data = choice_data,
  choice_effects = choice_effects
)

choice_parameters <- generate_choice_parameters(choice_effects)

compute_choice_likelihood(
  choice_parameters = choice_parameters,
  choice_likelihood = likelihood,
  logarithm = TRUE
)
#> [1] -2946469
```
