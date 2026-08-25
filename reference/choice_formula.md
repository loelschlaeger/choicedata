# Define choice model formula

The `choice_formula` object defines the choice model equation.

## Usage

``` r
choice_formula(formula, error_term = "probit", random_effects = character())

# S3 method for class 'choice_formula'
print(x, ...)
```

## Arguments

- formula:

  \[`formula`\]  
  A symbolic description of the choice model, see details.

- error_term:

  \[`character(1)`\]  
  Defines the model's error term. Current options are:

  - `"probit"` (default): errors are multivariate normally distributed

  - `"logit"`: errors follow a type-I extreme value distribution

- random_effects:

  \[[`character()`](https://rdrr.io/r/base/character.html)\]  
  Named vector defining random effects, see details.

- x:

  \[`choice_formula`\]  
  A `choice_formula` object.

- ...:

  Currently not used.

## Value

An object of class `choice_formula`, which is a `list` of the elements:

- `formula`:

  The model formula.

- `error_term`:

  The name of the model's error term specification.

- `choice`:

  The name of the response variable.

- `covariate_types`:

  The (up to) three different types of covariates.

- `ASC`:

  Does the model have ASCs?

- `random_effects`:

  The names of covariates with random effects.

## Specifying the model formula

The structure of `formula` is `choice ~ A | B | C`, i.e., a standard
[`formula`](https://rdrr.io/r/stats/formula.html) object but with three
parts on the right-hand side, separated by `|`, where

- `choice` is the name of the discrete response variable,

- `A` are names of **alternative-specific covariates** with **a
  coefficient that is constant across alternatives**,

- `B` are names of **covariates that are constant across alternatives**,

- and `C` are names of **alternative-specific covariates** with
  **alternative-specific coefficients**.

The following rules apply:

1.  By default, intercepts (referred to as alternative-specific
    constants, ASCs) are added to the model. They can be removed by
    adding `+ 0` in the second part, e.g., `choice ~ A | B + 0 | C`. To
    not include any covariates of the second type but to estimate ASCs,
    add `1` in the second part, e.g., `choice ~ A | 1 | C`. The
    expression `choice ~ A | 0 | C` is interpreted as no covariates of
    the second type and no ASCs.

2.  To not include covariates of any type, add `0` in the respective
    part, e.g., `choice ~ 0 | B | C`.

3.  Some parts of the formula can be omitted when there is no ambiguity.
    For example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.

4.  Multiple covariates in one part are separated by a `+` sign, e.g.,
    `choice ~ A1 + A2`.

5.  Arithmetic transformations of covariates in all three parts of the
    right-hand side are possible via the function
    [`I()`](https://rdrr.io/r/base/AsIs.html), e.g.,
    `choice ~ I(A1^2 + A2 * 2)`. In this case, a random effect can be
    defined for the transformed covariate, e.g.,
    `random_effects = c("I(A1^2 + A2 * 2)" = "cn")`.

## Specifying random effects

Specify random effects as `"<covariate>" = "<distribution>"`. Each
covariate must appear explicitly on the right-hand side of `formula`;
use `"ASC"` for alternative-specific constants.

Available distributions are:

- `"cn"`: correlated normal

- `"n"`: uncorrelated normal

- `"cln"`: positively signed correlated log-normal

- `"ln"`: positively signed uncorrelated log-normal

- `"cln-"`: negatively signed correlated log-normal

- `"ln-"`: negatively signed uncorrelated log-normal

## Examples

``` r
### specify a choice formula
choice_formula(
  formula = choice ~ I(A^2 + 1) | B | I(log(C)),
  error_term = "probit",
  random_effects = c("I(A^2+1)" = "cn", "B" = "cn")
)
#> 
#> ── Choice formula 
#> • choice ~ I(A^2 + 1) | B | I(log(C))
#> • error term: probit
#> • random effects:
#>   • I(A^2+1): cn
#>   • B: cn
```
