# Get identifier position

This helper function gets a position based on a decider and a choice
occasion number.

## Usage

``` r
get_position_from_identifier(
  N = length(Tp),
  Tp = 1,
  decider_number,
  occasion_number
)
```

## Arguments

- N:

  \[`integer(1)`\]  
  The number of deciders.

- Tp:

  \[`integer(1)` \| `integer(N)`\]  
  The number of choice occasions per decider.

  Can also be of length `N` for a variable number of choice occasions
  per decider.

- decider_number:

  \[`integer(1)`\]  
  A decider number, which is a number between 1 and `N`.

- occasion_number:

  \[`integer(1)`\]  
  An occasion number of decider `n`, which is a number between 1 and
  `Tp[n]`.

## Value

An `integer` position.
