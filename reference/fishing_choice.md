# Recreational Fishing Mode Choice

Choices by 1182 US respondents among fishing options.

This data can be used to model the choice between beach, pier,
private-boat, and charter-boat fishing based on trip cost, expected
catch rate, and monthly income.

## Usage

``` r
fishing_choice
```

## Format

A `tibble` with 1182 rows and 11 columns:

- respondent \[`integer`\]:

  The respondent identifier.

- choice \[`character`\]:

  The chosen fishing mode.

- price_beach, price_pier, price_boat, price_charter \[`numeric`\]:

  The cost of a fishing trip for each mode, in US dollars.

- catch_beach, catch_pier, catch_boat, catch_charter \[`numeric`\]:

  The expected catch rate for each fishing mode.

- income \[`numeric`\]:

  The respondent's monthly income in US dollars.

## Source

Adapted from `Fishing` in the [mlogit
package](https://CRAN.R-project.org/package=mlogit). The original study
is described by Herriges and Kling (1999).

## References

Herriges JA, Kling CL (1999). “Nonlinear income effects in random
utility models.” *The Review of Economics and Statistics*, **81**(1),
62–72.
[doi:10.1162/003465399767923827](https://doi.org/10.1162/003465399767923827)
.
