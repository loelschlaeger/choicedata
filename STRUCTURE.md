---
title: Structure of the `{choicedata}` package
---

# Models supported

- [ ] MNP
- [ ] normally mixed MNP

# Objects

## `choice_alternatives`

Goal is to define the set of choice alternatives. 

```r
choice_alternatives(
  J = 2, 
  alternatives = LETTERS[1:J], 
  base = alternatives[1]
) 
```

`choice_alternatives` object is a `character()` with attributes

- `J`
- `base`

Related functions:

```r
is.choice_alternatives(
  x, 
  error = FALSE, 
  var_name = oeli::variable_name(x)
)

print.choice_alternatives(
  x, 
  ...
) 
```

Documentation on:

- `J`
- `alternatives`
- `base`
- "Base alternative"

## `choice_formula`

Goal is to define the choice model formula.

```r
choice_formula(
  formula, 
  error_term, 
  random_effects = character()
)
```

`choice_formula` object is a `list` with elements

- `formula`
- `error_term`
- `choice`
- `var_types`
- `ASC`
- `mixing_types`
- `ordered_valid`

Related functions:

```r
is.choice_formula <- function(
  x,
  error = FALSE,
  var_name = oeli::variable_name(x)
) 

print.choice_formula <- function(
  x,
  ...
) 
```

Documentation on: 

- `formula`
- `error_term`
- `random_effects`
- "Choice models"
- "Specifying the model formula"

## `choice_effects`

Goal is to define the choice effects based on `choice_alternatives` and `choice_formula`.

```r
choice_effects(
  choice_formula,
  choice_alternatives,
  delimiter = "_"
)
```

`choice_effects` object is a `data.frame` (`choice_formula`, `choice_alternatives`, `delimiter` as attributes) with effects in rows and columns are

- `effect_name`
- `generic_name`
- `covariate`
- `alternative`
- `as_covariate`
- `as_effect`
- `mixing`

Related functions:

```r
is.choice_effects(
  x,
  error = TRUE,
  var_name = oeli::variable_name(x)
)

print.choice_effects(
  x,
  ...
)

compute_P(choice_effects)

compute_P_f(choice_effects)

compute_P_r(choice_effects)
```

Documentation on:

- `delimiter`

## `choice_parameters`

Goal is to define the choice model parameters based on `choice_effects`.

```r
choice_parameters(
  beta = NULL,
  Omega = NULL,
  Sigma = NULL
)
```

`choice_parameters` object is a `list` with elements

- `beta`
- `Omega`
- `Sigma`

Related functions:

```r 
is.choice_parameters(
  x,
  error = TRUE,
  var_name = oeli::variable_name(x)
)

print.choice_parameters(
  x,
  ...,
  rowdots = 4,
  coldots = 4,
  digits = 2,
  simplify = FALSE,
  details = !simplify
)

generate_choice_parameters(
  choice_effects,
  fixed_parameters = choice_parameters()
)

validate_choice_parameters(
  choice_parameters,
  choice_effects,
  allow_missing = FALSE
)

identify_choice_parameters(
  choice_parameters,
  scale = 1,
  level = 1
)

change_format.choice_parameters(
  choice_parameters,
  new_format,
  choice_effects,
  names = "effect"
)
```

Documentation on:

- `beta`
- `Omega`
- `Sigma`
- "Sampling missing choice model parameters"

## `choice_identifiers`

Goal is to define identifiers for deciders and choice occasions.

## `choice_preferences`

Goal is to define choice preferences based on `choice_parameters`, `choice_effects`, and `choice_identifiers`.

# Helpers

## `choicedata-checks`

## `choicedata-helpers`


