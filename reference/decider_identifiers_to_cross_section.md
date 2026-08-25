# Unique decider identifiers in cross-sectional case

This helper function makes unique decider identifiers for a given
combination of decider and occasion identifiers.

## Usage

``` r
decider_identifiers_to_cross_section(
  decider_identifiers,
  occasion_identifiers,
  delimiter = "."
)
```

## Arguments

- decider_identifiers, occasion_identifiers:

  \[`atomic()`\]  
  An `atomic` `vector` of identifiers.

## Value

An `atomic` `vector` of unique identifiers.
