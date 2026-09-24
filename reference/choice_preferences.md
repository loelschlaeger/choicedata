# Define choice preferences

The `choice_preferences` object defines the deciders' preferences in the
choice model.

- `choice_preferences()` constructs a `choice_preferences` object.

- `generate_choice_preferences()` samples choice preferences at random.

## Usage

``` r
choice_preferences(data_frame, column_decider = "deciderID")

generate_choice_preferences(
  choice_effects,
  choice_parameters = generate_choice_parameters(choice_effects),
  choice_identifiers = generate_choice_identifiers(N = 100)
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the deciders' preferences.

- column_decider:

  \[`character(1)` \| `NULL`\]  
  The column name of `data_frame` with the decider identifiers. If
  `NULL`, decider identifiers are generated.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
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

## Value

An object of class `choice_preferences`, which is a `tibble` with the
deciders' preferences. The column names are the names of the effects in
the choice model. The first column contains the decider identifiers, its
name is stored in the attribute `column_decider`.

## Examples

``` r
### generate choice preferences from choice parameters and effects
set.seed(1)
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | income | comfort,
    error_term = "probit",
    random_effects = c(
      "price" = "cn",
      "income" = "cn"
    ),
    latent_class_effects = "price"
  ),
  choice_alternatives = choice_alternatives(J = 3)
)
choice_parameters <- generate_choice_parameters(
  choice_effects = choice_effects, C = 2
)
(choice_preferences <- generate_choice_preferences(
  choice_parameters = choice_parameters,
  choice_effects = choice_effects,
  choice_identifiers = generate_choice_identifiers(N = 4)
))
#> # A tibble: 4 × 9
#>   deciderID  ASC_B ASC_C comfort_A comfort_B comfort_C price income_B income_C
#> * <chr>      <dbl> <dbl>     <dbl>     <dbl>     <dbl> <dbl>    <dbl>    <dbl>
#> 1 1         -0.626 0.184    -0.836      1.60     0.330 -2.19   0.319     0.402
#> 2 2         -0.626 0.184    -0.836      1.60     0.330 -2.43   0.0226    0.861
#> 3 3         -0.626 0.184    -0.836      1.60     0.330 -1.63   1.20      0.872
#> 4 4         -0.626 0.184    -0.836      1.60     0.330 -1.59   0.288     1.47 
```
