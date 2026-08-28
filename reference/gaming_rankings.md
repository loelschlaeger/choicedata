# Rankings of Gaming Platforms

Complete rankings of six gaming platforms by 91 Dutch respondents. Rank
1 is most preferred and rank 6 is least preferred.

This data can be used to model the choice between gaming platforms based
on ownership, the respondent's age, and weekly gaming hours.

## Usage

``` r
gaming_rankings
```

## Format

A `tibble` with 91 rows and 15 columns:

- respondent \[`integer`\]:

  The respondent identifier.

- rank_Xbox, rank_PlayStation, rank_PSPortable, rank_GameCube,
  rank_GameBoy, rank_PC \[`integer`\]:

  The rank assigned to each platform.

- owned_Xbox, owned_PlayStation, owned_PSPortable, owned_GameCube,
  owned_GameBoy, owned_PC \[`logical`\]:

  Whether the respondent owns each platform.

- age \[`integer`\]:

  The respondent's age in years.

- hours \[`numeric`\]:

  The hours spent gaming per week.

## Source

Adapted from `Game` in the [mlogit
package](https://CRAN.R-project.org/package=mlogit). The original data
are from the data archive for Fok et al. (2012).

## References

Fok D, Paap R, van Dijk B (2012). “A rank-ordered logit model with
unobserved heterogeneity in ranking capabilities.” *Journal of Applied
Econometrics*, **27**(5), 831–846.
[doi:10.1002/jae.1223](https://doi.org/10.1002/jae.1223) .
