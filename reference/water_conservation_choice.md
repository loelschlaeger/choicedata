# Dairy-Farm Water Conservation Choices

Stated panel choices by 98 dairy farmers in Mejia, Ecuador. Each farmer
completed four choice tasks with two conservation plans and a status
quo. The data have 1176 rows in long format.

This data can be used to model the choice between two water-conservation
plans and the status quo based on irrigation, manure and waste
management, training, cost-share payment, farm size, milk production,
and cattle density.

## Usage

``` r
water_conservation_choice
```

## Format

A `tibble` with 1176 rows and 12 columns:

- farmer \[`integer`\]:

  The farmer identifier.

- occasion \[`integer`\]:

  The choice occasion from 1 to 4.

- alternative \[`character`\]:

  Plan 1, plan 2, or the status quo.

- choice \[`integer`\]:

  Whether the alternative was chosen.

- irrigation \[`factor`\]:

  No new system, micro-sprinklers, or solid rain.

- manure \[`factor`\]:

  No new practice, composting, or dispersion.

- waste \[`factor`\]:

  No new practice, a collection center, or a municipal container.

- training \[`logical`\]:

  Whether training for water conflict resolution and cooperation is
  offered.

- payment \[`numeric`\]:

  The cost-share payment in US dollars per hectare.

- farm_size \[`numeric`\]:

  The farm size in hectares.

- milk_production \[`numeric`\]:

  The daily milk production in liters.

- cattle_density \[`numeric`\]:

  The number of cattle per hectare.

## Source

Adapted from the Mendeley Data source
[doi:10.17632/ncj6ws6hbj.1](https://doi.org/10.17632/ncj6ws6hbj.1) . The
source data are licensed under [CC BY
4.0](https://creativecommons.org/licenses/by/4.0/).

## References

Ortiz CA, Avila-Santamaría JJ, Martinez-Cruz AL (2023). “Dairy farmers'
willingness to adopt cleaner production practices for water
conservation: A discrete choice experiment in Mejia, Ecuador.”
*Agricultural Water Management*, **278**, 108168.
[doi:10.1016/j.agwat.2023.108168](https://doi.org/10.1016/j.agwat.2023.108168)
.
