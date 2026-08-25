# Revealed Preferences for Travel Mode Choice

Data set of revealed choices by 210 travelers between Sydney and
Melbourne who report their choice between the four travel modes plane,
train, bus, or car. The data were collected as part of a 1987 intercity
mode choice study.

## Usage

``` r
travel_mode_choice
```

## Format

A `tibble` with 840 rows and 8 columns:

- individual \[`integer`\]:

  The identifier for the decider.

- mode \[`character`\]:

  The travel mode.

- choice \[`integer`\]:

  Whether the mode was chosen.

- wait \[`integer`\]:

  The terminal waiting time, 0 for car.

- cost \[`integer`\]:

  The travel cost in dollars.

- travel \[`integer`\]:

  The travel time in minutes.

- income \[`integer`\]:

  The household income in thousand dollars.

- size \[`integer`\]:

  The traveling group size.

## References

Ben-Akiva M, Bolduc D, Bradley M (1993). “Estimation of travel choice
models with randomly distributed values of time.” *Transportation
Research Record*, 88–97. <https://trid.trb.org/View/385096>.
