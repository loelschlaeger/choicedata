# Travel Mode Choice in Canada

Travel mode choices for trips in the Montreal–Toronto corridor. Choice
sets vary from two to four available modes among air, bus, car, and
train. Monetary values were converted to Euro at an exchange rate of
1.6151 Canadian dollars = 1 Euro.

This data can be used to model the choice between the available air,
bus, car, and train modes based on distance, cost, in-vehicle time,
out-of-vehicle time, service frequency, household income, and the number
of urban trip endpoints.

## Usage

``` r
mode_canada_choice
```

## Format

A `tibble` with 15520 rows and 10 columns:

- case \[`integer`\]:

  The choice occasion identifier.

- mode \[`character`\]:

  The available travel mode.

- choice \[`integer`\]:

  Whether the mode was chosen.

- distance \[`numeric`\]:

  The trip distance in kilometers.

- cost \[`numeric`\]:

  The monetary cost of the mode in euros.

- inVehicleTime \[`numeric`\]:

  The time spent in the vehicle, in minutes.

- outVehicleTime \[`numeric`\]:

  The time spent outside the vehicle, in minutes.

- frequency \[`numeric`\]:

  The number of scheduled services; car has value 0.

- income \[`numeric`\]:

  The household-income value in thousands of euros.

- urban \[`numeric`\]:

  A trip-level count of how many of the trip's two endpoints (the origin
  and the destination) were classified as large cities in the original
  study: `0` means that neither endpoint is a large city, `1` means that
  exactly one endpoint is a large city, and `2` means that both
  endpoints are large cities.

## Source

Adapted from `ModeCanada` in the [mlogit
package](https://CRAN.R-project.org/package=mlogit). The source data
were provided by Frank Koppelman and used by Bhat (1995).

## References

Bhat CR (1995). “A heteroscedastic extreme value model of intercity
travel mode choice.” *Transportation Research Part B: Methodological*,
**29**(6), 471–483.
[doi:10.1016/0191-2615(95)00015-6](https://doi.org/10.1016/0191-2615%2895%2900015-6)
.
