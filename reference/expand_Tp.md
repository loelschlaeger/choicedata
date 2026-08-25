# Expand `Tp`

This helper function expands the number of choice occasions `Tp` to a
`vector` of length `N`.

## Usage

``` r
expand_Tp(N = length(Tp), Tp = 1)
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

## Value

An `integer` `vector` of length `N`.
