# Define choice model parameters

These functions construct, validate, and transform an object of class
`choice_parameters`, which defines the parameters of a choice model.

- `choice_parameters()` constructs a `choice_parameters` object.

- `generate_choice_parameters()` samples parameters at random, see
  details.

- `validate_choice_parameters()` validates a `choice_parameters` object.

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
  n_classes = 1L
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
  The coefficient vector of length `P` (number of effects) for computing
  the linear-in-parameters systematic utility \\V = X\beta\\. For a
  latent class model, a list of one coefficient vector per class.

- Omega:

  \[`matrix(nrow = P_r, ncol = P_r)` \| `list(C)` \| `NULL`\]  
  The covariance matrix of random effects of dimension `P_r` times
  `P_r`, where `P_r` \\\leq\\ `P` is the number of random effects.

  In a latent class model, a list of one covariance matrix per class.
  Can be `NULL` in the case of `P_r = 0`.

- Sigma:

  \[`matrix(nrow = J, ncol = J)` \| `numeric(1)` \| `NULL`\]  
  Only relevant in the probit model. For unordered alternatives it is
  the covariance matrix of dimension `J` times `J` (number of
  alternatives) for the Gaussian error term \\\epsilon = U - V\\. In
  ordered models it reduces to a single variance term.

- gamma:

  \[`numeric(J - 1)` \| `NULL`\]  
  Optional vector of strictly increasing threshold parameters for
  ordered models. The first element must equal zero for identification.
  Ignored for unordered alternatives.

- weights:

  \[`numeric(C)` \| `NULL`\]  
  Positive latent class weights. They are shared across choice occasions
  and normalized to sum to one during validation.

  Class-specific `beta`, `Omega`, and `weights` entries use their
  supplied order. As usual for finite mixtures, permuting all
  class-specific entries leaves the model unchanged, so class labels are
  not identified.

- choice_effects:

  \[`choice_effects`\]  
  A
  [`choice_effects`](https://loelschlaeger.de/choicedata/reference/choice_effects.md)
  object describing the utility specification.

- fixed_parameters:

  \[`choice_parameters`\]  
  Optionally a `choice_parameters` object of parameters to keep fixed
  when sampling other parameters.

- n_classes:

  \[`integer(1)`\]  
  Number of latent classes. The default `1` generates a regular model.

- choice_parameters:

  \[`choice_parameters` \|
  [`numeric()`](https://rdrr.io/r/base/numeric.html)\]  
  A `choice_parameters` object. For `switch_parameter_space()`, a
  numeric vector in optimization space is also accepted and converted
  back to a `choice_parameters` object.

- allow_missing:

  \[`logical(1)`\]  
  Should parameters be allowed to be missing (`TRUE`) or must all
  required elements be present (`FALSE`)?

## Value

`choice_parameters()`, `generate_choice_parameters()`, and
`validate_choice_parameters()` return a `choice_parameters` list with
the elements:

- `beta`:

  The coefficient vector (if any).

- `Omega`:

  The covariance matrix of random effects (if any).

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

Unspecified choice model parameters (if required) are drawn
independently from the following distributions:

- `beta`:

  Drawn from a multivariate normal distribution with zero mean and a
  diagonal covariance matrix with value 10 on the diagonal.

- `Omega`:

  Drawn from an Inverse-Wishart distribution with degrees of freedom
  equal to `P_r` + 2 and scale matrix equal to the identity.

- `Sigma`:

  The first row and column are fixed to 0 for level normalization. The
  \\(2, 2)\\-value is fixed to 1 for scale normalization. The lower
  right block is drawn from an Inverse-Wishart distribution with degrees
  of freedom equal to `J` + 1 and scale matrix equal to the identity.

## Parameter spaces

The `switch_parameter_space()` function transforms a `choice_parameters`
object between the interpretation and optimization space.

- The interpretation space is a `list` of (not necessarily identified)
  parameters that can be interpreted.

- The optimization space is a `numeric` vector of identified parameters
  that can be optimized:

  - `beta` is not transformed

  - the first row and column of `Sigma` are fixed to 0 for level
    normalization and the second diagonal element is fixed to 1 for
    scale normalization

  - the covariance matrices (`Omega` and `Sigma`) are transformed to
    their vectorized Cholesky factor (diagonal fixed to be positive for
    uniqueness)

  - latent class parameters are concatenated in class order, and `C - 1`
    log weight ratios use the first class as reference

## Examples

``` r
### generate choice parameters at random
J <- 3
choice_effects <- choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ A | B, error_term = "probit",
    random_effects = c("A" = "cn")
  ),
  choice_alternatives = choice_alternatives(J = J)
)
choice_parameters <- generate_choice_parameters(
  choice_effects = choice_effects,
  fixed_parameters = choice_parameters(
    Sigma = diag(c(0, rep(1, J - 1))) # scale and level normalization
  )
)

### generate a two-class model with class-specific random effects
latent_parameters <- generate_choice_parameters(
  choice_effects = choice_effects,
  n_classes = 2L
)
latent_parameters$weights
#> [1] 0.5 0.5
```
