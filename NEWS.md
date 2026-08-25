# choicedata 0.2.0

* Specialized `choiceprob_*()` functions are now internal; use
  `compute_choice_probabilities()` as the public interface.

* Improved input validations. Internal validation failures now use descriptive
  `cli` messages.

* Added partial rankings, individual choice sets, joint panel outcomes,
  log-normal random effects, and latent class estimation.

* Probability calculations now use Rcpp.

# choicedata 0.1.0

* Initial CRAN submission.
