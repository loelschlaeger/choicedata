# Changelog

## choicedata 0.2.0

- Added support for uncorrelated normal and log-normal random effects.

- Probit probabilities now come from
  [`oeli::pmvnorm()`](http://loelschlaeger.de/oeli/reference/dmvnorm.md).

- Specialized `choiceprob_*()` functions are now internal; use
  [`compute_choice_probabilities()`](https://loelschlaeger.de/choicedata/reference/choice_probabilities.md)
  as the public interface.

- Improved input validations. Internal validation failures now use
  descriptive `cli` messages.

- Added partial rankings, individual choice sets, joint panel outcomes,
  log-normal random effects, and latent class estimation.

- Probability calculations now use Rcpp.

- Added additional choice data sets for demonstrations.

- [`long_to_wide()`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  now accepts a `factor` column of alternatives.

- `J` in
  [`choice_alternatives()`](https://loelschlaeger.de/choicedata/reference/choice_alternatives.md)
  defaults to the number of supplied choice alternatives.

- Removed unused package dependencies.

## choicedata 0.1.0

CRAN release: 2025-10-09

- Initial CRAN submission.
