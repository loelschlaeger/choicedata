
choice_preferences <- function(
    choice_preferences, ...
  ) {

}

is.choice_preferences <- function() {

}

generate_choice_preferences <- function(
    choice_parameters,
    choice_effects,
    choice_identifiers = generate_choice_identifiers(N = 100)
  ) {

  ### input checks
  check_not_missing(choice_parameters)
  check_not_missing(choice_effects)
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)

  # TODO
  # validate_choice_parameters <- function(
  #   choice_parameters,
  #   choice_effects,
  #   C = 1,
  #   allow_missing = FALSE
  # )

  is.choice_identifiers(choice_identifiers, error = TRUE)
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)
  choice_formula <- attr(choice_effects, "choice_formula")
  latent_classes <- choice_formula$latent_classes
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)

  ### get coefficient vectors
  preferences <- as.data.frame(matrix(NA_real_, nrow = N, ncol = P_f + P_r))
  colnames(preferences) <- choice_effects[, "effect_name"]
  if (length(latent_classes) > 0) {
    z_n <- sample.int(
      choice_parameters$C, size = N, replace = TRUE, prob = choice_parameters$s
    )
  }
  for (n in seq_len(N)) {
    coef_n <- numeric()
    if (P_f > 0) {

      coef_n <- c(coef_n, choice_parameters$alpha)

      # TODO
      # if (latent_classes %in% c("fe", "both")) {
      #   coef_n <- c(coef_n, choice_parameters$alpha[, z_n])
      # } else {
      #   coef_n <- c(coef_n, choice_parameters$alpha)
      # }

    }

    if (P_r > 0) {

        beta_n <- oeli::rmvnorm(
          mean = choice_parameters$b,
          Sigma = choice_parameters$Omega
        )


      # TODO
      # if (latent_classes %in% c("re", "both")) {
      #   beta_n <- oeli::rmvnorm(
      #     mean = choice_parameters$b[, c],
      #     Sigma = matrix(choice_parameters$Omega[, c], nrow = P_r, ncol = P_r)
      #   )
      # } else {
      #   beta_n <- oeli::rmvnorm(
      #     mean = choice_parameters$b,
      #     Sigma = choice_parameters$Omega
      #   )
      # }
      coef_n <- c(coef_n, beta_n)
    }

    preferences[n, ] <- coef_n
  }

  ### build 'choice_preferences' object
  structure(
    cbind(choice_identifiers, preferences),
    class = c("choice_preferences", "data.frame")
  )
}

# print.choice_preferences <- function() {
#
# }

