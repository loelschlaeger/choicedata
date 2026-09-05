# Define choice probabilities

The `choice_probabilities` object defines the choice probabilities.

- `compute_choice_probabilities()` calculates the choice probabilities
  based on the choice parameters and the choice data.

## Usage

``` r
choice_probabilities(
  data_frame,
  choice_only = TRUE,
  column_decider = "deciderID",
  column_occasion = NULL,
  cross_section = is.null(column_occasion),
  column_probabilities = NULL,
  logarithm = FALSE,
  aggregate = c("occasion", "decider")
)

compute_choice_probabilities(
  choice_parameters,
  choice_data,
  choice_effects,
  choice_only = TRUE,
  input_checks = TRUE,
  aggregate = c("occasion", "decider"),
  logarithm = FALSE,
  ...
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice probabilities.

- choice_only:

  \[`logical(1)`\]  
  Only the probabilities for the chosen alternatives?

- column_decider:

  \[`character(1)`\]  
  The name of the identifier column for deciders.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  The name of the identifier column for choice occasions (panel data).
  Can be `NULL` for the cross-sectional case.

- cross_section:

  \[`logical(1)`\]  
  Treat choice data as cross-sectional?

- column_probabilities:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  The column name of the `data_frame` with the choice probabilities for
  all choice alternatives.

  If `choice_only = TRUE`, it is the name of a single column that
  contains the probabilities for the chosen alternatives.

- logarithm:

  \[`logical(1)`\]  
  Are the supplied or requested values log-probabilities?

- aggregate:

  \[`character(1)`\]  
  Probability unit. `"occasion"` returns one result per choice occasion.
  `"decider"` returns the joint result for each decider's observed
  sequence of choices.

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html) \|
  [`list()`](https://rdrr.io/r/base/list.html)\]  
  A
  [`choice_parameters`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  object. A numeric vector in optimization space, as created by
  [`switch_parameter_space`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md),
  is also accepted.

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

- input_checks:

  \[`logical(1)`\]  
  Should additional internal input checks be performed before computing
  the probabilities?

- ...:

  Additional arguments for the probability computation:

  - `n_draws` \[`integer(1)`\]: The number of draws for simulated
    probabilities, which are required for mixed logit models and for
    mixed probit models with non-normal random effects. The default is
    `200`.

  - `draws` \[`matrix`\]: A matrix of standard normal draws with one
    column per random effect that replaces the generated draws.

  - `cml` \[`character(1)`\]: Composite marginal likelihood for panel
    probit models. Either `"no"` (default, the full likelihood), `"fp"`
    (all pairs of choice occasions), or `"ap"` (adjacent pairs of choice
    occasions).

  - `ghk_draws` \[`integer(1)`\]: The number of draws of the GHK
    simulator for multivariate normal probabilities in probit models,
    see [`pmvnorm`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).
    Probabilities of up to three dimensions are computed exactly; higher
    dimensions use the GHK simulator on a fixed sequence of quasi-random
    Halton points. The default is `500`.

## Value

A `choice_probabilities` tibble with the identifier columns followed by
the probability column(s). If `choice_only = TRUE`, there is a single
column `choice_probability` (or `log_choice_probability` if
`logarithm = TRUE`). Otherwise, there is one column per choice
alternative. The attributes `column_decider`, `column_occasion`,
`cross_section`, `column_probabilities`, `choice_only`, `logarithm`, and
`aggregate` store the column roles and the probability type.

If `choice_only = FALSE` and `aggregate = "decider"`, the tibble instead
has one row per possible outcome sequence and decider, with the sequence
in the list column `outcome`.

## Examples

``` r
### multinomial logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(beta = c(0.2, -0.1))
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.520

### multinomial probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Sigma = diag(c(0, 1))
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.468

### ordered logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit"
  ),
  choice_alternatives = choice_alternatives(
    J = 3,
    ordered = TRUE
  )
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  gamma = c(0, 1)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters,
  choice_type = "ordered"
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.213

### ordered probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(
    J = 3,
    ordered = TRUE
  )
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Sigma = 1,
  gamma = c(0, 1)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters,
  choice_type = "ordered"
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.288

### ranked logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(beta = c(0.2, -0.1))
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters,
  choice_type = "ranked"
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.520

### ranked probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Sigma = diag(c(0, 1))
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters,
  choice_type = "ranked"
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.468

### mixed logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit",
    random_effects = c(
      x = "cn",
      z = "cn"
    )
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.508

### mixed probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit",
    random_effects = c(
      x = "cn",
      z = "cn"
    )
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2),
  Sigma = diag(c(0, 1))
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.525

### panel logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit",
    random_effects = c(
      x = "cn",
      z = "cn"
    )
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(
    N = 1L,
    Tp = 2L
  ),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 2 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                       0.509
#> 2 1         2                       0.565

### panel probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit",
    random_effects = c(
      x = "cn",
      z = "cn"
    )
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = c(0.2, -0.1),
  Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2),
  Sigma = diag(c(0, 1))
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(
    N = 1L,
    Tp = 2L
  ),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects,
  aggregate = "decider",
  cml = "ap"
)
#> # A tibble: 1 × 2
#>   deciderID choice_probability
#> * <chr>                  <dbl>
#> 1 1                      0.292

### latent class logit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "logit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = list(c(0.2, -0.1), c(-0.2, 0.1)),
  weights = c(0.5, 0.5)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                         0.5

### latent class probit
set.seed(1)
effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x + z | 0,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(J = 2)
)
parameters <- choice_parameters(
  beta = list(c(0.2, -0.1), c(-0.2, 0.1)),
  Sigma = diag(c(0, 1)),
  weights = c(0.5, 0.5)
)
simulated_data <- generate_choice_data(
  choice_effects = effects,
  choice_identifiers = generate_choice_identifiers(N = 1L),
  choice_parameters = parameters
)
compute_choice_probabilities(
  choice_parameters = parameters,
  choice_data = simulated_data,
  choice_effects = effects
)
#> # A tibble: 1 × 3
#>   deciderID occasionID choice_probability
#> * <chr>     <chr>                   <dbl>
#> 1 1         1                         0.5
```
