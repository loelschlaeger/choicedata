# Define and compute choice likelihood

These functions prepare and evaluate the likelihood contribution of
observed choices for a given choice model.

- `choice_likelihood()` pre-computes the design matrices and choice
  indices implied by `choice_data` and `choice_effects`. The returned
  object stores these quantities so that repeated likelihood evaluations
  during maximum likelihood estimation avoid redundant work.

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
  aggregate = c("total", "decider", "occasion"),
  ...
)
```

## Arguments

- choice_data:

  \[`choice_data`\]  
  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  object.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object.

- choice_identifiers:

  \[`choice_identifiers`\]  
  A
  [`choice_identifiers`](https://loelschlaeger.de/choicedata/reference/choice_identifiers.md)
  object. The default is extracted from `choice_data`.

- input_checks:

  \[`logical(1)`\]  
  Check inputs?

- ...:

  Additional arguments.

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A
  [`choice_parameters`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  object.

  A numeric vector in optimization space is also accepted and converted
  with
  [`switch_parameter_space()`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md).

  A list of either representation is evaluated as a batch of parameter
  draws.

- choice_likelihood:

  \[`choice_likelihood`\]  
  A `choice_likelihood` object.

- logarithm:

  \[`logical(1)`\]  
  Return the log-likelihood? If `FALSE`, the likelihood is returned.

- negative:

  \[`logical(1)`\]  
  Return the negative (log-)likelihood? Useful for minimization
  routines.

- aggregate:

  \[`character(1)`\]  
  Unit of the returned likelihood:

  - `"occasion"` returns one contribution per observed choice occasion,

  - `"decider"` returns one joint contribution per decider,

  - `"total"` sums the decider contributions.

## Value

`choice_likelihood()` returns an object of class `choice_likelihood`,
which is a `list` containing the design matrices, choice indices, and
identifiers.

`compute_choice_likelihood()` returns a numeric scalar for
`aggregate = "total"` and a named numeric vector otherwise.

## Examples

``` r
### compute choice likelihood
data(list = "train_choice")

choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | time,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(
    J = 2,
    alternatives = c("A", "B")
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

choice_parameters <- generate_choice_parameters(
  choice_effects = choice_effects
)

compute_choice_likelihood(
  choice_parameters = choice_parameters,
  choice_likelihood = likelihood,
  logarithm = TRUE
)
#> [1] -141614.3
```
