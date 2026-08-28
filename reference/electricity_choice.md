# Stated Preferences for Electricity Suppliers

Stated choices by 361 US households among four hypothetical electricity
suppliers. Each household completed 8 to 12 choice tasks.

This data can be used to model the choice between electricity suppliers
based on price, contract length, whether the supplier is local or
well-known, and whether time-of-day or seasonal rates are offered.

## Usage

``` r
electricity_choice
```

## Format

A `tibble` with 4308 rows and 27 columns:

- household \[`integer`\]:

  The household identifier.

- occasion \[`integer`\]:

  The choice occasion within a household.

- choice \[`character`\]:

  The chosen supplier, from `"1"` to `"4"`.

- price_1–price_4 \[`numeric`\]:

  The fixed electricity price in US-dollar cents per kWh for each
  supplier.

- contract_1–contract_4 \[`numeric`\]:

  The contract length in years for each supplier.

- local_1–local_4 \[`logical`\]:

  Whether each supplier is local.

- known_1–known_4 \[`logical`\]:

  Whether each supplier is well-known.

- timeOfDay_1–timeOfDay_4 \[`logical`\]:

  Whether each supplier offers a time-of-day rate.

- seasonal_1–seasonal_4 \[`logical`\]:

  Whether each supplier offers a seasonal rate.

## Source

Adapted from `Electricity` in the [mlogit
package](https://CRAN.R-project.org/package=mlogit). The study is
described by Huber and Train (2001).

## References

Huber J, Train K (2001). “On the similarity of classical and Bayesian
estimates of individual mean partworths.” *Marketing Letters*,
**12**(3), 259–269.
[doi:10.1023/A:1011120928698](https://doi.org/10.1023/A%3A1011120928698)
.
