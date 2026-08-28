# Revealed Preferences for Travel Mode Choice

Data set of revealed choices by 210 travelers between Sydney and
Melbourne who report their choice between the four travel modes plane,
train, bus, or car. The data were collected as part of a 1987 intercity
mode choice study. Monetary values were converted to Euro at an exchange
rate of 1.6196 Australian dollars = 1 Euro.

This data can be used to model the choice between plane, train, bus, and
car based on waiting time, travel cost, travel time, household income,
and traveling group size.

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

  The terminal waiting time in minutes, 0 for car.

- cost \[`numeric`\]:

  The in-vehicle cost for all trip stages, in euros.

- travel \[`integer`\]:

  The travel time in minutes.

- income \[`numeric`\]:

  The household income in thousands of euros.

- size \[`integer`\]:

  The traveling group size.

## Source

Adapted from `TravelMode` in the [AER
package](https://CRAN.R-project.org/package=AER).

## References

Greene WH (2003). *Econometric analysis*, 5 edition. Prentice Hall,
Upper Saddle River, NJ.
