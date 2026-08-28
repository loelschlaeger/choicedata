# Choices for a Proposed Wind-Power Project

Stated panel choices by 308 residents for a proposed wind-power project
near Setskog, Norway. Each respondent completed six choice tasks. The
alternatives are the status quo and two project plans. Compensation
values were converted to Euro at an exchange rate of 10.8770 Norwegian
kroner = 1 Euro.

This data can be used to model the choice between two wind-power project
plans and the status quo based on the number and height of turbines,
power-line placement, compensation, and collective psychological
ownership.

## Usage

``` r
wind_power_choice
```

## Format

A `tibble` with 1848 rows and 16 columns:

- respondent \[`integer`\]:

  The respondent identifier.

- occasion \[`integer`\]:

  The choice occasion from 1 to 6.

- choice \[`character`\]:

  The chosen alternative from 1 to 3.

- turbines_1–turbines_3 \[`integer`\]:

  The number of turbines for each alternative.

- height_1–height_3 \[`integer`\]:

  The turbine height in meters for each alternative.

- powerline_1–powerline_3 \[`factor`\]:

  The power-line route and placement: none, overhead or underground
  throughout, or mixed between forests and residential areas.

- compensation_1–compensation_3 \[`numeric`\]:

  The annual reduction in municipal taxes offered as compensation, in
  euros.

- psychological_ownership \[`numeric`\]:

  A respondent-specific, model-estimated latent score for *collective
  psychological ownership* of the natural area affected by the proposed
  wind farm. It was constructed from three seven-point Likert items
  asking whether the area is "ours" and belongs collectively to
  residents. The construct was normalized to mean zero and standard
  deviation one in the study: positive values indicate stronger and
  negative values weaker feelings of shared ownership relative to the
  sample average.

## Source

Adapted from the Mendeley Data source
[doi:10.17632/3pdx4p3s9g.1](https://doi.org/10.17632/3pdx4p3s9g.1) . The
source data are licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/).

## References

Dugstad A, Grimsrud K, Kipperberg G, Lindhjem H, Navrud S (2023). “Place
attachment and preferences for wind energy – A value-based approach.”
*Energy Research & Social Science*, **100**, 103094.
[doi:10.1016/j.erss.2023.103094](https://doi.org/10.1016/j.erss.2023.103094)
.

Dugstad A, Brouwer R, Grimsrud K, Kipperberg G, Lindhjem H, Navrud S
(2024). “Nature is ours! – Psychological ownership and preferences for
wind energy.” *Energy Economics*, **129**, 107239.
[doi:10.1016/j.eneco.2023.107239](https://doi.org/10.1016/j.eneco.2023.107239)
.
