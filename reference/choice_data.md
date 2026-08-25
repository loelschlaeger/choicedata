# Define choice data

The `choice_data` object defines the choice data, it is a combination of
`choice_responses` and `choice_covariates`.

## Usage

``` r
choice_data(
  data_frame,
  format = "wide",
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  delimiter = "_",
  cross_section = is.null(column_occasion),
  choice_type = c("unordered", "ordered", "ranked")
)

generate_choice_data(
  choice_effects,
  choice_identifiers = generate_choice_identifiers(N = 100),
  choice_covariates = generate_choice_covariates(choice_effects, choice_identifiers),
  choice_parameters = generate_choice_parameters(choice_effects),
  choice_preferences = generate_choice_preferences(choice_effects, choice_parameters,
    choice_identifiers),
  column_choice = "choice",
  choice_type = c("unordered", "ordered", "ranked")
)

long_to_wide(
  data_frame,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  column_choice = "choice",
  column_alternative = "alternative",
  column_decider = "deciderID",
  column_occasion = NULL,
  alternatives = unique(data_frame[[column_alternative]]),
  delimiter = "_",
  choice_type = c("unordered", "ordered", "ranked")
)

wide_to_long(
  data_frame,
  column_choice = "choice",
  column_alternative = "alternative",
  alternatives = NULL,
  delimiter = "_",
  choice_type = c("unordered", "ordered", "ranked")
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice data.

- format:

  \[`character(1)`\]  
  Format of `data_frame`. Use `"wide"` when each row contains all
  alternatives of an occasion and `"long"` when each row contains a
  single alternative.

- column_choice:

  \[`character(1)` \| `NULL`\]  
  Column name with the observed choices. In wide layout this column
  should contain a single value per observation: for unordered data the
  value is the label of the chosen alternative, for ordered data it is
  the ordered factor or integer score, and for ranked data it is omitted
  in favor of one column per alternative (see `choice_type`).

  In long layout the same column is evaluated once per alternative:
  unordered data must use a binary indicator (1 for the chosen
  alternative, 0 otherwise), ordered data repeats the ordinal value for
  every alternative, and ranked data stores consecutive ranks `1:k` for
  the observed top `k` alternatives and `NA` for unranked alternatives.

  An entirely missing response marks an occasion that is omitted from
  the likelihood. Set to `NULL` for purely covariate tables.

- column_decider:

  \[`character(1)`\]  
  Column name with decider identifiers.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  Column name with occasion identifiers. Set to `NULL` in
  cross-sectional data.

- column_alternative:

  \[`character(1)` \| `NULL`\]  
  Column name with alternative identifiers when `format = "long"`.

- column_ac_covariates:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Column names with alternative-constant covariates.

- column_as_covariates:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Column names of `data_frame` with alternative-specific covariates.

- delimiter:

  \[`character(1)`\]  
  Delimiter separating alternative identifiers from covariate names in
  wide format.

- cross_section:

  \[`logical(1)`\]  
  Treat choice data as cross-sectional?

- choice_type:

  \[`character(1)`\]  
  Requested response type. Use `"unordered"` (default), `"ordered"`, or
  `"ranked"`.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object.

- choice_identifiers:

  \[`choice_identifiers`\]  
  A
  [`choice_identifiers`](https://loelschlaeger.de/choicedata/reference/choice_identifiers.md)
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

- choice_preferences:

  \[`choice_preferences`\]  
  A
  [`choice_preferences`](https://loelschlaeger.de/choicedata/reference/choice_preferences.md)
  object.

- alternatives:

  \[`character(J)`\]  
  Unique labels for the choice alternatives.

## Value

A `choice_data` tibble.

## Examples

``` r
### simulate data from a multinomial probit model
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ A | B,
    error_term = "probit",
    random_effects = c("A" = "cn")
  ),
  choice_alternatives = choice_alternatives(J = 3)
)
generate_choice_data(choice_effects = choice_effects)
#> # A tibble: 100 × 7
#>    deciderID occasionID choice       B     A_A     A_B    A_C
#>  * <chr>     <chr>      <chr>    <dbl>   <dbl>   <dbl>  <dbl>
#>  1 1         1          A       2.76   -0.0500 -0.251   0.445
#>  2 2         1          B      -1.91    0.0465  0.578   0.118
#>  3 3         1          B       0.0192  0.862  -0.243  -0.206
#>  4 4         1          C       2.68    0.0296  0.550  -2.27 
#>  5 5         1          B      -0.665  -0.361   0.213   1.07 
#>  6 6         1          C      -0.976   1.11   -0.246  -1.18 
#>  7 7         1          B      -1.70    1.07    0.132   0.489
#>  8 8         1          A       0.237  -1.47    0.284   1.34 
#>  9 9         1          B      -0.110   1.32    0.524   0.607
#> 10 10        1          A       1.30    0.172  -0.0903  1.92 
#> # ℹ 90 more rows

### transform between long/wide format
long_to_wide(
  data_frame = travel_mode_choice,
  column_alternative = "mode",
  column_decider = "individual"
)
#> # A tibble: 210 × 16
#>    individual income  size wait_plane wait_train wait_bus wait_car cost_plane
#>         <int>  <int> <int>      <int>      <int>    <int>    <int>      <int>
#>  1          1     35     1         69         34       35        0         59
#>  2          2     30     2         64         44       53        0         58
#>  3          3     40     1         69         34       35        0        115
#>  4          4     70     3         64         44       53        0         49
#>  5          5     45     2         64         44       53        0         60
#>  6          6     20     1         69         40       35        0         59
#>  7          7     45     1         45         34       35        0        148
#>  8          8     12     1         69         34       35        0        121
#>  9          9     40     1         69         34       35        0         59
#> 10         10     70     2         69         34       35        0         58
#> # ℹ 200 more rows
#> # ℹ 8 more variables: cost_train <int>, cost_bus <int>, cost_car <int>,
#> #   travel_plane <int>, travel_train <int>, travel_bus <int>, travel_car <int>,
#> #   choice <chr>
wide_to_long(
  data_frame = train_choice
)
#> # A tibble: 5,858 × 8
#>    deciderID occasionID choice alternative price  time change comfort
#>        <int>      <int>  <int> <chr>       <dbl> <dbl>  <int> <fct>  
#>  1         1          1      1 A            52.9  2.5       0 1      
#>  2         1          1      0 B            88.1  2.5       0 1      
#>  3         1          2      1 A            52.9  2.5       0 1      
#>  4         1          2      0 B            70.5  2.17      0 1      
#>  5         1          3      1 A            52.9  1.92      0 1      
#>  6         1          3      0 B            88.1  1.92      0 0      
#>  7         1          4      0 A            88.1  2.17      0 1      
#>  8         1          4      1 B            70.5  2.5       0 0      
#>  9         1          5      0 A            52.9  2.5       0 1      
#> 10         1          5      1 B            70.5  2.5       0 0      
#> # ℹ 5,848 more rows

### individual choice sets and a missing response
partial_data <- data.frame(
  deciderID = c(1, 1, 2),
  alternative = c("A", "B", "B"),
  choice = c(1L, 0L, NA),
  cost = c(1.2, 1.5, 0.8)
)
choice_data(
  data_frame = partial_data,
  format = "long",
  column_decider = "deciderID",
  column_alternative = "alternative",
  column_as_covariates = "cost"
)
#> # A tibble: 3 × 4
#>   deciderID alternative choice  cost
#> *     <dbl> <chr>        <int> <dbl>
#> 1         1 A                1   1.2
#> 2         1 B                0   1.5
#> 3         2 B               NA   0.8
```
