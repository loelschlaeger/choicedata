# Stated Preferences for Train Traveling

Data set of 2929 stated choices by 235 Dutch individuals deciding
between two hypothetical train trip options `"A"` and `"B"` based on the
price, the travel time, the number of rail-to-rail transfers (changes),
and the level of comfort.

The data were obtained in 1987 by Hague Consulting Group for the
National Dutch Railways. Prices were recorded in cents of Dutch guilders
and were converted to Euro at an exchange rate of 2.20371 guilders = 1
Euro.

## Usage

``` r
train_choice
```

## Format

A `tibble` with 2929 rows and 11 columns:

- deciderID \[`integer`\]:

  The identifier for the decider.

- occasionID \[`integer`\]:

  The choice occasion within a decider.

- choice \[`character`\]:

  The chosen alternative, either `"A"` or `"B"`.

- price_A \[`numeric`\]:

  The price for alternative `"A"` in Euro.

- time_A \[`numeric`\]:

  The travel time for alternative `"A"` in hours.

- change_A \[`integer`\]:

  The number of changes for alternative `"A"`.

- comfort_A \[`factor`\]:

  The comfort level for alternative `"A"`, where `0` is the best comfort
  and `2` the worst.

- price_B \[`numeric`\]:

  The price for alternative `"B"` in Euro.

- time_B \[`numeric`\]:

  The travel time for alternative `"B"` in hours.

- change_B \[`integer`\]:

  The number of changes for alternative `"B"`.

- comfort_B \[`factor`\]:

  The comfort level for alternative `"B"`, where `0` is the best comfort
  and `2` the worst.

## Source

Adapted from `Train` in the [mlogit
package](https://CRAN.R-project.org/package=mlogit).

## References

Ben-Akiva M, Bolduc D, Bradley M (1993). “Estimation of travel choice
models with randomly distributed values of time.” *Transportation
Research Record*, 88–97. <https://trid.trb.org/View/385096>.
