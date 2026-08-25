# Define choice model effects

This function constructs an object of class `choice_effects`, which
defines the effects of a choice model.

## Usage

``` r
choice_effects(
  choice_formula,
  choice_alternatives,
  choice_data = NULL,
  delimiter = "_"
)

# S3 method for class 'choice_effects'
print(x, ...)
```

## Arguments

- choice_formula:

  \[`choice_formula`\]  
  A
  [`choice_formula`](https://loelschlaeger.de/choicedata/reference/choice_formula.md)
  object.

- choice_alternatives:

  \[`choice_alternatives`\]  
  A
  [`choice_alternatives`](https://loelschlaeger.de/choicedata/reference/choice_alternatives.md)
  object.

- choice_data:

  \[`NULL` \| `choice_data`\]  
  A
  [`choice_data`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  object.

  Required to resolve data-dependent elements in `choice_formula` (if
  any).

- delimiter:

  \[`character(1)`\]  
  The delimiter between covariate and alternative name.

- x:

  \[`choice_effects`\]  
  A `choice_effects` object.

- ...:

  Currently not used.

## Value

A `choice_effects` object, which is a `data.frame`, where each row is a
model effect, and columns are

1.  `"effect_name"`, the name for the effect which is composed of
    covariate and alternative name,

2.  `"generic_name"`, the generic effect name `"beta_<effect number>"`,

3.  `"covariate"`, the (transformed) covariate name connected to the
    effect,

4.  `"alternative"`, the alternative name connected to the effect (only
    if the effect is alternative-specific),

5.  `"as_covariate"`, indicator whether the covariate is
    alternative-specific,

6.  `"as_effect"`, indicator whether the effect is alternative-specific,

7.  `"mixing"`, a factor with levels in the order

    1.  `"cn"` (correlated normal distribution),

    2.  `"cln"` (positively signed correlated log-normal distribution),

    3.  `"cln-"` (negatively signed correlated log-normal distribution),

    4.  `"n"` (uncorrelated normal distribution),

    5.  `"ln"` (positively signed uncorrelated log-normal distribution),

    6.  `"ln-"` (negatively signed uncorrelated log-normal
        distribution),

    indicating the type of random effect.

For identification, the choice effects are ordered according to the
following rules:

1.  Non-random effects come before random effects.

2.  Otherwise, the order is determined by occurrence in `formula`.

It contains the arguments `choice_formula`, `choice_alternatives`, and
`delimiter` as attributes.

## Examples

``` r
### construct choice effects
choice_effects(
  choice_formula = choice_formula(
    formula = choice ~ price | income | I(comfort == 1),
    error_term = "probit",
    random_effects = c(
      "price" = "cn",
      "income" = "cn"
     )
  ),
  choice_alternatives = choice_alternatives(J = 3)
)
#> 
#> ── Choice effects 
#>       effect_name generic_name     covariate alternative as_covariate as_effect
#> 1           ASC_B       beta_1          <NA>           B        FALSE      TRUE
#> 2           ASC_C       beta_2          <NA>           C        FALSE      TRUE
#> 3 I(comfort==1)_A       beta_3 I(comfort==1)           A         TRUE      TRUE
#> 4 I(comfort==1)_B       beta_4 I(comfort==1)           B         TRUE      TRUE
#> 5 I(comfort==1)_C       beta_5 I(comfort==1)           C         TRUE      TRUE
#> 6           price       beta_6         price        <NA>         TRUE     FALSE
#> 7        income_B       beta_7        income           B        FALSE      TRUE
#> 8        income_C       beta_8        income           C        FALSE      TRUE
#>   mixing
#> 1   <NA>
#> 2   <NA>
#> 3   <NA>
#> 4   <NA>
#> 5   <NA>
#> 6     cn
#> 7     cn
#> 8     cn
```
