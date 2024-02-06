#' @rdname choice_parameters

choice_preferences <- function(preferences, choice_identifiers = NULL) {
  structure(
    preferences,
    class = c("choice_preferences", "data.frame")
  )
}

validate_choice_preferences <- function() {

}

sample_choice_preferences <- function(choice_parameters, choice_effects, N) {

  ### input checks
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  check_consistency(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects
  )
  N <- check_N(N)
  latent_classes <- attr(choice_parameters, "latent_classes")
  P_f <- sum(choice_effects$mixing == "none")
  P_r <- sum(choice_effects$mixing != "none")

  ### get coefficient vectors
  preferences <- as.data.frame(matrix(NA_real_, nrow = N, ncol = P_f + P_r))
  colnames(preferences) <- choice_effects$name
  if (latent_classes != "none") {
    z_n <- sample.int(
      choice_parameters$C, size = N, replace = TRUE, prob = choice_parameters$s
    )
  }
  for (n in seq_len(N)) {
    coef_n <- numeric()
    if (P_f > 0) {
      if (latent_classes %in% c("fe", "both")) {
        coef_n <- c(coef_n, choice_parameters$alpha[, z_n])
      } else {
        coef_n <- c(coef_n, choice_parameters$alpha)
      }
    }
    if (P_r > 0) {
      if (latent_classes %in% c("re", "both")) {
        beta_n <- oeli::rmvnorm(
          mean = choice_parameters$b[, c],
          Sigma = matrix(choice_parameters$Omega[, c], nrow = P_r, ncol = P_r)
        )
      } else {
        beta_n <- oeli::rmvnorm(
          mean = choice_parameters$b,
          Sigma = choice_parameters$Omega
        )
      }
      coef_n <- c(coef_n, beta_n)
    }
    preferences[n, ] <- coef_n
  }

  ### build 'choice_preferences' object
  choice_preferences(preferences)
}




