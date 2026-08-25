# Define choice covariates

The `choice_covariates` object defines the choice model covariates.

- `generate_choice_covariates()` samples covariates.

- `covariate_names()` gives the covariate names for given
  `choice_effects`.

- `design_matrices()` builds design matrices.

## Usage

``` r
choice_covariates(
  data_frame,
  format = "wide",
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  delimiter = "_",
  cross_section = is.null(column_occasion)
)

generate_choice_covariates(
  choice_effects = NULL,
  choice_identifiers = generate_choice_identifiers(N = 100),
  labels = covariate_names(choice_effects),
  n = nrow(choice_identifiers),
  marginals = list(),
  correlation = diag(length(labels)),
  verbose = FALSE,
  delimiter = "_"
)

covariate_names(choice_effects)

design_matrices(
  x,
  choice_effects,
  choice_identifiers = extract_choice_identifiers(x)
)
```

## Arguments

- data_frame:

  \[`data.frame`\]  
  Contains the choice covariates.

- format:

  \[`character(1)`\]  
  Format of `data_frame`. Use `"wide"` when covariates for all
  alternatives are stored in a single row per occasion and `"long"` when
  each alternative forms a separate row.

- column_decider:

  \[`character(1)`\]  
  Column name with decider identifiers.

- column_occasion:

  \[`character(1)` \| `NULL`\]  
  Column name with occasion identifiers. Set to `NULL` for
  cross-sectional data.

- column_alternative:

  \[`character(1)` \| `NULL`\]  
  Column name with alternative identifiers when `format = "long"`.

- column_ac_covariates:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Column names with alternative-constant covariates.

- column_as_covariates:

  \[[`character()`](https://rdrr.io/r/base/character.html) \| `NULL`\]  
  Column names with alternative-specific covariates.

- delimiter:

  \[`character(1)`\]  
  Delimiter separating alternative identifiers from covariate names in
  wide format.

- cross_section:

  \[`logical(1)`\]  
  Treat choice data as cross-sectional?

- choice_effects:

  \[`choice_effects` \| `NULL`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object.

- choice_identifiers:

  \[`choice_identifiers`\]  
  A
  [`choice_identifiers`](https://loelschlaeger.de/choicedata/reference/choice_identifiers.md)
  object.

- labels:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Unique labels for the regressors.

- n:

  \[`integer(1)`\]  
  The number of values per regressor.

- marginals:

  \[[`list()`](https://rdrr.io/r/base/list.html)\]  
  Optionally marginal distributions for regressors. If not specified,
  standard normal marginal distributions are used.

  Each list entry must be named according to a regressor label, and the
  following distributions are currently supported:

  discrete distributions

  :   - Poisson: `list(type = "poisson", lambda = ...)`

      - categorical: `list(type = "categorical", p = c(...))`

  continuous distributions

  :   - normal: `list(type = "normal", mean = ..., sd = ...)`

      - uniform: `list(type = "uniform", min = ..., max = ...)`

- correlation:

  \[[`matrix()`](https://rdrr.io/r/base/matrix.html)\]  
  A correlation matrix of dimension `length(labels)`, where the
  `(p, q)`-th entry defines the correlation between regressor
  `labels[p]` and `labels[q]`.

- verbose:

  \[`logical(1)`\]  
  Print information about the simulated regressors?

- x:

  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  or `choice_covariates` object.

## Value

`choice_covariates()` and `generate_choice_covariates()` return a
`choice_covariates` tibble. `covariate_names()` returns a character
vector. `design_matrices()` returns one numeric design matrix per choice
occasion in a list; its `Tp` attribute records the panel lengths.

## Design matrices

A covariate design matrix contains the choice covariates of a decider at
a choice occasion. It is of dimension `J` x `P`, where `J` is the number
of choice alternatives and `P` the number of effects.

## Examples

``` r
### sample covariates from choice effects
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | income | comfort,
    error_term = "probit",
    random_effects = c(
      "price" = "cn",
      "income" = "cn"
    )
  ),
  choice_alternatives = choice_alternatives(J = 3)
)
(choice_covariates <- generate_choice_covariates(
  choice_effects = choice_effects,
  choice_identifiers = generate_choice_identifiers(N = 3, Tp = 2)
))
#> # A tibble: 6 × 9
#>   deciderID occasionID  income price_A price_B price_C comfort_A comfort_B
#> * <chr>     <chr>        <dbl>   <dbl>   <dbl>   <dbl>     <dbl>     <dbl>
#> 1 1         1          -1.82    -1.40    0.255  -2.44   -0.00557    0.622 
#> 2 1         2          -1.63    -0.247  -0.244  -0.283  -0.554      0.629 
#> 3 2         1           0.468    0.512  -1.86   -0.522  -0.0526     0.543 
#> 4 2         2          -0.0160   0.363  -1.30    0.738   1.89      -0.0974
#> 5 3         1           0.112   -0.827  -1.51    0.935   0.176      0.244 
#> 6 3         2          -0.639   -0.134  -1.91   -0.279  -0.313      1.07  
#> # ℹ 1 more variable: comfort_C <dbl>
```
