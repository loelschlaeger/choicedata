#' Define choice model parameters
#'
#' @description
#' These functions construct, validate, and transform an object of class
#' `choice_parameters`, which defines the parameters of a choice model.
#'
#' - `choice_parameters()` constructs a `choice_parameters` object.
#' - `generate_choice_parameters()` samples parameters at random, see details.
#' - `validate_choice_parameters()` validates a `choice_parameters` object.
#' - `identify_choice_parameters()` applies scale and level normalization, see
#'   details.
#' - `change_format.choice_parameters()` transforms between the interpretation
#'   and optimization parameter space, see details.
#'
#' @inheritSection choice_formula Choice models
#'
#' @param alpha \[`numeric(P_f)`\]\cr
#' The non-random coefficients. Only relevant if non-random coefficients are
#' specified.
#'
#' @param b \[`numeric(P_r)`\]\cr
#' The mean of random effects. Only relevant if random coefficients are
#' specified.
#'
#' @param Omega \[`matrix(P_r)``\]\cr
#' The covariance matrix of random effects. Only relevant if random coefficients
#' are specified.
#'
#' @param Sigma \[`matrix(nrow = J, ncol = J)`\]\cr
#' The error term covariance matrix. Only relevant in the probit model.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @return
#' An object of class `choice_parameters`, which is a `list` with the elements:
#' \describe{
#'   \item{`alpha`}{The non-random coefficients (if any).}
#'   \item{`b`}{The mean of random effects (if any).}
#'   \item{`Omega`}{The covariance of random effects (if any).}
#'   \item{`Sigma`}{The error term covariance (if any).}
#' }
#'
#' @export

choice_parameters <- function(
    alpha = NULL,
    b = NULL,
    Omega = NULL,
    Sigma = NULL
  ) {

  ### generate list for parameters
  parameters <- list(
    "alpha" = alpha,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma
  )

  ### remove missing parameters from the list
  parameters[sapply(parameters, is.null)] <- NULL

  ### ensure that parameters are numerics without missing values
  for (i in seq_along(parameters)) {
    oeli::input_check_response(
      check = checkmate::check_numeric(parameters[[i]], any.missing = FALSE),
      var_name = names(parameters)[i]
    )
  }

  ### build object
  structure(
    parameters,
    class = c("choice_parameters", "list")
  )

}

#' @noRd

is.choice_parameters <- function(
    x,
    error = TRUE,
    var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_parameters")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_parameters}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_parameters
#'
#' @param x \[`choice_parameters`\]\cr
#' A `choice_parameters` object.
#'
#' @param ...
#' Currently not used.
#'
#' @inheritParams oeli::print_matrix
#'
#' @exportS3Method

print.choice_parameters <- function(
    x,
    ...,
    rowdots = 4,
    coldots = 4,
    digits = 2,
    simplify = FALSE,
    details = !simplify
) {
  is.choice_parameters(x, error = TRUE)
  cli::cli_h3("Choice parameters")
  if (length(x) == 0) {
    cli::cat_line(cli::style_italic("none specified yet"))
  } else {
    for (i in seq_along(x)) {
      cat(cli::symbol[["bullet"]], " ", sep = "")
      oeli::print_matrix(
        x[[i]], rowdots = rowdots, coldots = coldots, digits = digits,
        label = names(x)[i], simplify = simplify, details = details
      )
      if (i < length(x)) cat("\n\n")
    }
  }
  invisible(x)
}

#' @rdname choice_parameters
#'
#' @param fixed_parameters \[`choice_parameters`\]\cr
#' Optionally a `choice_parameters` object of parameters to keep
#' fixed when sampling other parameters.
#'
#' @section Sampling missing choice model parameters:
#'
#' Unspecified choice model parameters (if required for the model) are drawn
#' independently from the following distributions:
#' \describe{
#'   \item{`alpha`}{Drawn from a multivariate normal distribution with
#'   zero mean and a diagonal covariance matrix with value 10 on the diagonal.}
#'   \item{`b`}{Drawn from a multivariate normal distribution with zero
#'   mean and a diagonal covariance matrix with value 10 on the diagonal.}
#'   \item{`Omega`}{Drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to `P_r` + 2 and scale matrix equal to the identity.}
#'   \item{`Sigma`}{Drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to `J` + 2 and scale matrix equal to the identity.}
#' }
#'
#' @examples
#' ### generate choice parameters at random
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ A | B, error_term = "probit", random_effects = "A"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#' choice_parameters <- generate_choice_parameters(
#'   choice_effects = choice_effects,
#'   fixed_parameters = choice_parameters("b" = 1)
#' )
#'
#' @export

generate_choice_parameters <- function(
    choice_effects,
    fixed_parameters = choice_parameters()
  ) {

  ### input checks
  check_not_missing(choice_effects)
  is.choice_parameters(fixed_parameters, error = TRUE)
  x <- fixed_parameters
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula[["error_term"]]
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")

  ### validate fixed parameters
  x <- validate_choice_parameters(
    choice_parameters = x, choice_effects = choice_effects,
    allow_missing = TRUE
  )

  ### generate missing parameters

  # alpha
  if (P_f > 0 && is.null(x$alpha)) {
    x$alpha <- oeli::rmvnorm(mean = numeric(P_f), Sigma = 10 * diag(P_f))
  }

  # b
  if (P_r > 0 && is.null(x$b)) {
    x$b <- oeli::rmvnorm(mean = numeric(P_r), Sigma = 10 * diag(P_r))
  }

  # Omega
  if (P_r > 0 && is.null(x$Omega)) {
    x$Omega <- oeli::rwishart(df = P_r + 2, scale = diag(P_r), inv = TRUE)
  }

  # Sigma
  if (error_term == "probit" && is.null(x$Sigma)) {
    x$Sigma <- oeli::rwishart(df = J + 2, scale = diag(J), inv = TRUE)
  }

  ### validate parameters and return
  validate_choice_parameters(
    choice_parameters = x,
    choice_effects = choice_effects,
    allow_missing = FALSE
  )
}

#' @rdname choice_parameters
#' @export

validate_choice_parameters <- function(
    choice_parameters,
    choice_effects,
    allow_missing = FALSE
  ) {

  ### input checks
  check_not_missing(choice_parameters)
  check_not_missing(choice_effects)
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  x <- choice_parameters
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  allow_missing <- check_allow_missing(allow_missing)

  ### check parameters

  # alpha
  if (P_f > 0) {
    if ("alpha" %in% names(x)) {
      oeli::input_check_response(
        check = oeli::check_numeric_vector(x$alpha, len = P_f),
        var_name = "alpha"
      )
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var alpha} is required", call = NULL)
    }
  } else {
    x$alpha <- NULL
  }

  # b
  if (P_r > 0) {
    if ("b" %in% names(x)) {
      oeli::input_check_response(
        check = oeli::check_numeric_vector(x$b, len = P_r),
        var_name = "b"
      )
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var b} is required", call = NULL)
    }
  } else {
    x$b <- NULL
  }

  # Omega
  if (P_r > 0) {
    if ("Omega" %in% names(x)) {
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(x$Omega, dim = P_r),
        var_name = "Omega"
      )
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var Omega} is required", call = NULL)
    }
  } else {
    x$Omega <- NULL
  }

  # Sigma
  if (error_term == "probit") {
    if ("Sigma" %in% names(x)) {
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(x$Sigma, dim = J),
        var_name = "Sigma"
      )
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var Sigma} is required", call = NULL)
    }
  } else {
    x$Sigma <- NULL
  }

  ### return object
  return(x)
}

#' @rdname choice_parameters
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' A `choice_parameters` object.
#'
#' @param scale
#' TODO
#'
#' @section Level and scale normalization:
#' Choice models are invariant towards the level and scale of utility, hence
#' a transformation is required for identifiability.
#'
#' For level normalization, we replace the first row and the first column of
#' `Sigma` with zeros, which corresponds to taking utility differences with
#' respect to alternative `1`.
#'
#' For scale normalization, we fix the \eqn{(2, 2)}-value of `Sigma` to a
#' positive value `scale`.
#'
#' @export

identify_choice_parameters <- function(
    choice_parameters,
    scale = 1
  ) {

  ### input checks
  check_not_missing(choice_parameters)
  is.choice_parameters(choice_parameters, error = TRUE)
  oeli::input_check_response(
    check = checkmate::check_number(scale, lower = 0, finite = TRUE)
  )

  ### level normalization
  if (!is.null(choice_parameters$Sigma)) {
    choice_parameters$Sigma[1, ] <- 0
    choice_parameters$Sigma[, 1] <- 0
  }

  ### scale normalization
  if (!is.null(choice_parameters$Sigma)) {
    choice_parameters$Sigma[2, 2] <- 1
  }


  ### return
  choice_parameters
}

#' TODO: inherit choice_effects and error_term
#'
#' @param x \[`choice_parameters` | `numeric()`\]\cr
#' The choice_parameters.
#'
#' @param names \[`character(1)`\]\cr
#' - `"effect"`
#' - `"generic"`
#' - `"unnamed"`
#'
#' TODO: order of numeric vector

change_format.choice_parameters <- function(
    x,
    new_format = NULL,
    choice_effects,
    names = "effect"
  ) {


  par <- optimizeR::ParameterSpaces$
    new()$
    switch()


}



