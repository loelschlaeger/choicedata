# Define choice model parameters

These functions construct, validate, and transform an object of class
`choice_parameters`, which defines the parameters of a choice model.

- `choice_parameters()` constructs a `choice_parameters` object.

- `generate_choice_parameters()` samples parameters at random, see
  details.

- `validate_choice_parameters()` checks model-specific dimensions.

- `switch_parameter_space()` transforms a `choice_parameters` object
  between the interpretation and optimization space, see details.

## Usage

``` r
choice_parameters(
  beta = NULL,
  Omega = NULL,
  Sigma = NULL,
  gamma = NULL,
  weights = NULL
)

generate_choice_parameters(
  choice_effects,
  fixed_parameters = choice_parameters(),
  C = 1L
)

validate_choice_parameters(
  choice_parameters,
  choice_effects,
  allow_missing = FALSE
)

switch_parameter_space(choice_parameters, choice_effects)
```

## Arguments

- beta:

  \[`numeric(P)` \| `list(C)` \| `NULL`\]  
  The coefficient vector for computing the linear-in-parameters
  systematic utility \\V = X\beta\\.

  For a latent class model, a list of one coefficient vector per class.

- Omega:

  \[`matrix(nrow = P_r, ncol = P_r)` \| `list(C)` \| `NULL`\]  
  The covariance matrix of random effects.

  Not used when `P_r = 0`.

  In a latent class model, a list of one covariance matrix per class.

  Covariances involving uncorrelated random effects are fixed to zero.

- Sigma:

  \[`matrix(nrow = J, ncol = J)` \| `numeric(1)` \| `NULL`\]  
  Only relevant in the probit model.

  For unordered alternatives it is the covariance matrix for the
  Gaussian error term \\\epsilon = U - V\\.

  In ordered models it reduces to a single variance term.

- gamma:

  \[`numeric(J - 1)` \| `NULL`\]  
  Vector of strictly increasing threshold parameters required for
  ordered models.

  The first element must equal zero for identification.

- weights:

  \[`numeric(C)` \| `NULL`\]  
  Positive latent class weights.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object.

- fixed_parameters:

  \[`choice_parameters`\]  
  A `choice_parameters` object. Its supplied components are kept fixed.
  Missing components are completed as described below.

- C:

  \[`integer(1)`\]  
  Number of latent classes.

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `choice_parameters` object. For `switch_parameter_space()`, a
  numeric vector in optimization space is also accepted and converted
  back to a `choice_parameters` object.

- allow_missing:

  \[`logical(1)`\]  
  Allow required parameter components to be omitted?

## Value

`choice_parameters()`, `generate_choice_parameters()`, and
`validate_choice_parameters()` return a `choice_parameters` list with
the elements:

- `beta`:

  The coefficient vector (if any).

- `Omega`:

  The random-effect covariance matrix on the underlying normal scale (if
  any).

- `Sigma`:

  The error term covariance matrix (or variance in ordered models).

- `gamma`:

  Threshold parameters for ordered models (if any).

- `weights`:

  The latent class weights (if any).

`switch_parameter_space()` returns a named numeric vector when given a
`choice_parameters` object and a `choice_parameters` object when given a
numeric optimization vector.

## Sampling missing choice model parameters

`generate_choice_parameters()` completes required components that are
absent from `fixed_parameters`.

Missing components are generated as follows:

- `beta`:

  Drawn independently for each class from a multivariate normal
  distribution with zero mean and covariance matrix `10 * diag(P)`.

- `Omega`:

  Drawn independently for each class from an Inverse-Wishart
  distribution with `P_r + 2` degrees of freedom and identity scale
  matrix. Covariances involving uncorrelated random effects are then set
  to zero.

- `Sigma`:

  For unordered probit models, the lower right block is drawn from an
  Inverse-Wishart distribution with `J + 1` degrees of freedom and
  identity scale matrix. The first row and column are fixed to zero and
  the matrix is scaled so that element \\(2, 2)\\ equals one. For
  ordered probit models, `Sigma` is set to one; logit models do not use
  `Sigma`.

- `gamma`:

  For ordered models with two categories, set to zero. Otherwise,
  positive increments are drawn as `exp(z)`, where the elements of `z`
  are independent standard normal draws, and cumulatively added to the
  first threshold zero. Unordered models do not use `gamma`.

- `weights`:

  Set to equal class probabilities `1 / C`.

## Parameter spaces

The `switch_parameter_space()` function transforms a `choice_parameters`
object between the interpretation and optimization space.

- The interpretation space is a `list` of (not necessarily identified)
  parameters that can be interpreted.

- The optimization space is a `numeric` vector of identified parameters
  that can be optimized:

  - `beta` is not transformed

  - `Omega` is represented by its vectorized unique Cholesky factor;
    elements involving uncorrelated random effects are omitted

  - for unordered probit models, `Sigma` is represented through utility
    differences relative to the first alternative, with the first
    variance fixed to one, and transformed to a vectorized unique
    Cholesky factor

  - for ordered probit models, the positive scalar `Sigma` is
    log-transformed

  - the first ordered threshold is fixed to zero and omitted; logarithms
    of the remaining positive threshold increments are used

  - latent class parameters are concatenated in class order, and `C - 1`
    log weight ratios use the first class as reference

## Examples

``` r
### generate choice parameters at random
J <- 3
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ x | y, error_term = "probit",
    random_effects = c("x" = "cn")
  ),
  choice_alternatives = choice_alternatives(J = J)
)
(parameters <- generate_choice_parameters(
  choice_effects = choice_effects,
  fixed_parameters = choice_parameters(
    Sigma = diag(c(0, rep(1, J - 1))) # scale and level normalization
  )
))
#> $beta
#>        y_B        y_C      ASC_B      ASC_C          x 
#>  0.4069475 -4.8479786  0.6399206 -2.2690565  1.1437793 
#> 
#> $Omega
#>           x
#> x 0.1731118
#> 
#> $Sigma
#>   A B C
#> A 0 0 0
#> B 0 1 0
#> C 0 0 1
#> 
#> attr(,"class")
#> [1] "choice_parameters" "list"             

### switch between interpretation and optimization spaces
(optimization_parameters <- switch_parameter_space(
  choice_parameters = parameters,
  choice_effects = choice_effects
))
#>     beta_1     beta_2     beta_3     beta_4     beta_5        o_1        l_2 
#>  0.4069475 -4.8479786  0.6399206 -2.2690565  1.1437793  0.4160670  0.0000000 
#>        l_3 
#>  1.0000000 
#> attr(,"class")
#> [1] "choice_parameters" "numeric"          
switch_parameter_space(
  choice_parameters = optimization_parameters,
  choice_effects = choice_effects
)
#> $beta
#>        y_B        y_C      ASC_B      ASC_C          x 
#>  0.4069475 -4.8479786  0.6399206 -2.2690565  1.1437793 
#> 
#> $Omega
#>           x
#> x 0.1731118
#> 
#> $Sigma
#>   A B C
#> A 0 0 0
#> B 0 1 0
#> C 0 0 1
#> 
#> attr(,"class")
#> [1] "choice_parameters" "list"             
```
