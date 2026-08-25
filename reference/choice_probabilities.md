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
  column_probabilities = if (choice_only) "choice_probability"
)

compute_choice_probabilities(
  choice_parameters,
  choice_data,
  choice_effects,
  choice_only = TRUE,
  input_checks = TRUE,
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

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  Either a
  [`choice_parameters`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  object or a numeric vector in optimization space, as created by
  [`switch_parameter_space`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md).

- choice_data:

  \[`choice_data`\]  
  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  object providing responses and covariates.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object defining the specification.

- input_checks:

  \[`logical(1)`\]  
  Should additional internal input checks be performed before computing
  the probabilities?

- ...:

  Additional probability arguments. Common choices are `draws` or
  `n_draws` for simulated mixed models and `cml = "no"`, `"fp"`, or
  `"ap"` for Probit panels. Supplied draws are standard normal and are
  transformed using the current random-effect covariance matrix.

## Value

A `choice_probabilities` tibble. With `choice_only = TRUE`, it contains
one row per occasion and a `choice_probability` column. A joint panel
probability is repeated over the observed occasions of its decider;
missing responses receive `NA`.

With `choice_only = FALSE`, non-ranked cross-sectional models contain
one row per occasion and one column per alternative. An unavailable
alternative has probability zero. Ranked and panel models instead
contain one row per possible outcome, an `outcome` list-column, and
`choice_probability`. These rows cover complete or partial rankings and
complete joint panel sequences, and their probabilities sum to one per
decider. Enumerating them can grow combinatorially with ranking depth
and panel length. For a missing ranked response, a common observed depth
of the same decider is reused; without one, all available alternatives
are ranked.

## Supported models

The public API supports every combination of Logit or Probit errors,
fixed, correlated normal, or correlated log-normal coefficients,
discrete, ordered, or ranked responses, and cross-sectional or panel
data. It also supports latent classes with or without random effects.
Probit panels can use the full likelihood (`cml = "no"`), full pairwise
CML (`"fp"`), or adjacent pairwise CML (`"ap"`). All-outcome output
always uses the full likelihood.

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
choice_parameters <- generate_choice_parameters(choice_effects)
choice_data <- choice_data(
  data_frame = train_choice,
  format = "wide",
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = "occasionID"
)
compute_choice_probabilities(
  choice_parameters = choice_parameters,
  choice_data = choice_data,
  choice_effects = choice_effects,
  choice_only = TRUE
)
#> # A tibble: 2,929 × 3
#>    deciderID occasionID choice_probability
#>  * <chr>     <chr>                   <dbl>
#>  1 1         1                    3.65e-66
#>  2 1         2                    3.65e-66
#>  3 1         3                    3.65e-66
#>  4 1         4                    3.65e-66
#>  5 1         5                    3.65e-66
#>  6 1         6                    3.65e-66
#>  7 1         7                    3.65e-66
#>  8 1         8                    3.65e-66
#>  9 1         9                    3.65e-66
#> 10 1         10                   3.65e-66
#> # ℹ 2,919 more rows
```
