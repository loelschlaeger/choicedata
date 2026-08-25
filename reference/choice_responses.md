# Define choice response

The `choice_responses` object defines the observed choice responses.

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
  choice_type = c("unordered", "ordered", "ranked")
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
  object.

- choice_covariates:

  \[`choice_covariates`\]  
  A
  [`choice_covariates`](https://loelschlaeger.de/choicedata/reference/choice_covariates.md)
  object.

- choice_parameters:

  \[`choice_parameters`\]  
  A
  [`choice_parameters`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  object.

- choice_identifiers:

  \[`choice_identifiers`\]  
  A
  [`choice_identifiers`](https://loelschlaeger.de/choicedata/reference/choice_identifiers.md)
  object.

- choice_preferences:

  \[`choice_preferences`\]  
  A
  [`choice_preferences`](https://loelschlaeger.de/choicedata/reference/choice_preferences.md)
  object.

- choice_type:

  \[`character(1)`\]  
  The response type to simulate. Use `"unordered"` (default),
  `"ordered"`, or `"ranked"`.

## Value

A `choice_responses` tibble.

## Examples

``` r
### generate choice responses from choice effects
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | time,
    error_term = "probit"
  ),
  choice_alternatives = choice_alternatives(J = 5)
)
(generate_choice_responses(
  choice_effects = choice_effects,
  choice_type = "ranked"
))
#> # A tibble: 100 × 8
#>    deciderID occasionID choice choice_A choice_B choice_C choice_D choice_E
#>  * <chr>     <chr>      <chr>     <int>    <int>    <int>    <int>    <int>
#>  1 1         1          D             4        5        3        1        2
#>  2 2         1          E             3        5        4        2        1
#>  3 3         1          C             2        5        1        4        3
#>  4 4         1          C             2        4        1        5        3
#>  5 5         1          C             2        3        1        5        4
#>  6 6         1          C             2        4        1        5        3
#>  7 7         1          C             2        3        1        4        5
#>  8 8         1          E             2        5        4        3        1
#>  9 9         1          D             3        4        5        1        2
#> 10 10        1          C             2        4        1        5        3
#> # ℹ 90 more rows
```
