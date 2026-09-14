# Changelog

## choicedata 0.2.0

- Added support for uncorrelated normal and log-normal random effects.

- Added `latent_class_effects` to
  [`choice_formula()`](https://loelschlaeger.de/choicedata/reference/choice_formula.md),
  which names the effects that differ between latent classes, and the
  column `latent_class` to
  [`choice_effects()`](https://loelschlaeger.de/choicedata/reference/choice_effects.md).

- A named `beta` is matched to the effects by name instead of by
  position, and
  [`generate_choice_parameters()`](https://loelschlaeger.de/choicedata/reference/choice_parameters.md)
  completes a named `beta` that fixes only some of the effects.

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

- Added the choice data sets `water_conservation_choice`,
  `wind_power_choice`, and `lichess_berserk_choice`.

- [`long_to_wide()`](https://loelschlaeger.de/choicedata/reference/choice_data.md)
  now accepts a `factor` column of alternatives.

- `J` in
  [`choice_alternatives()`](https://loelschlaeger.de/choicedata/reference/choice_alternatives.md)
  defaults to the number of supplied choice alternatives.

- New function
  [`train_test()`](https://loelschlaeger.de/choicedata/reference/train_test.md),
  which splits choice data by deciders or by choice occasions into a
  train and a test subset.

- Removed unused package dependencies.

## choicedata 0.1.0

CRAN release: 2025-10-09

- Initial CRAN submission.
