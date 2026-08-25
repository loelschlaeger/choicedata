# Define choice response

The `choice_responses` object defines the observed discrete responses.
Additional response columns (for example ranked choice indicators) are
preserved so they can be merged with covariates downstream.

- `generate_choice_responses()` simulates choices

## Usage

``` r
choice_responses(
  data_frame,
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = NULL,
  cross_section = is.null(column_occasion)
)

generate_choice_responses(
  choice_effects,
  choice_covariates = generate_choice_covariates(choice_effects = choice_effects),
  choice_parameters = generate_choice_parameters(choice_effects = choice_effects),
  choice_identifiers = extract_choice_identifiers(choice_covariates),
  choice_preferences = generate_choice_preferences(choice_parameters = choice_parameters,
    choice_effects = choice_effects, choice_identifiers = choice_identifiers),
  column_choice = "choice",
  choice_type = c("auto", "discrete", "ordered", "ranked")
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice responses.

- column_choice:

  \[`character(1)`\]  
  The column name of `data_frame` with the choice responses.

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

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object describing the model structure.

- choice_covariates:

  \[`choice_covariates`\]  
  Covariates used to construct utilities.

- choice_parameters:

  \[`choice_parameters`\]  
  Model parameters supplying the mean and covariance components.

- choice_identifiers:

  \[`choice_identifiers`\]  
  Identifiers describing the panel or cross-sectional structure.

- choice_preferences:

  \[`choice_preferences`\]  
  Preference draws to simulate the choices.

- choice_type:

  \[`character(1)`\]  
  The response type to simulate. Use `"auto"` (default) to derive the
  type from `choice_alternatives`, or explicitly request `"discrete"`,
  `"ordered"`, or `"ranked"` outcomes.

## Value

A `choice_responses` tibble.

## Examples

``` r
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | time,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(J = 5)
)
generate_choice_responses(choice_effects = choice_effects)
#> # A tibble: 100 × 3
#>    deciderID occasionID choice
#>  * <chr>     <chr>      <chr> 
#>  1 1         1          B     
#>  2 2         1          D     
#>  3 3         1          D     
#>  4 4         1          B     
#>  5 5         1          D     
#>  6 6         1          B     
#>  7 7         1          A     
#>  8 8         1          D     
#>  9 9         1          B     
#> 10 10        1          D     
#> # ℹ 90 more rows
```
