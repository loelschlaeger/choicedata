#' Define choice model parameters
#'
#' @description
#' These functions construct, validate, and transform an object of class
#' `choice_parameters`, which defines the parameters of a choice model.
#'
#' - `choice_parameters()` constructs a `choice_parameters` object.
#' - `generate_choice_parameters()` samples parameters at random, see details.
#' - `validate_choice_parameters()` validates a `choice_parameters` object.
#' - `identify_choice_parameters()` applies scale and level normalization.
#' - `change_format.choice_parameters()` transforms between parameter spaces.
#'
#' @inheritSection choice_formula Choice models
#'
#' @param C \[`integer(1)`\]\cr
#' The number of latent classes (if any specified in `choice_formula`).
#'
#' By default, `C = 1`, which effectively means no latent classes.
#'
#' @param s \[`numeric(C)`\]\cr
#' The latent class weights. Only relevant if latent classes are specified.
#'
#' Must be non-negative and sum-up to 1.
#'
#' @param alpha \[`numeric(P_f)` | `matrix(nrow = P_f, ncol = C)`\]\cr
#' The non-random coefficients. Only relevant if non-random coefficients are
#' specified.
#'
#' In the case of latent classes, different values are stored column-wise.
#'
#' @param b \[`numeric(P_r)` | `matrix(nrow = P_r, ncol = C)`\]\cr
#' The mean of random effects. Only relevant if random coefficients are
#' specified.
#'
#' In the case of latent classes, different values are stored column-wise.
#'
#' @param Omega \[`matrix(P_r)` | `matrix(nrow = P_r^2, ncol = C)`\]\cr
#' The covariance matrix of random effects. Only relevant if random coefficients
#' are specified.
#'
#' In the case of latent classes, different covariance matrices are stored
#' column-wise in vector form.
#'
#' @param Sigma \[`matrix(nrow = J, ncol = J)` | `numeric(1)`\]\cr
#' The error term covariance matrix. Only relevant in the probit model.
#'
#' In the ordered probit model, `Sigma` is `numeric(1)`.
#'
#' @param gamma \[`numeric(J - 1)`\]\cr
#' The utility thresholds. Only relevant in the ordered model case.
#' Must be strictly ascending.
#'
#' `gamma` corresponds to \eqn{\gamma_1, \dots, \gamma_{J-1}}, while the
#' lower and upper bounds are \eqn{\gamma_0 = -\infty} and
#' \eqn{\gamma_J = +\infty}.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @return
#' An object of class `choice_parameters`, which is a `list` with the elements:
#' \describe{
#'   \item{`s`}{The latent class weights (if any).}
#'   \item{`alpha`}{The non-random coefficients (if any).}
#'   \item{`b`}{The mean of random effects (if any).}
#'   \item{`Omega`}{The covariance of random effects (if any).}
#'   \item{`Sigma`}{The error term covariance (if any).}
#'   \item{`gamma`}{The utility thresholds (if any).}
#' }
#'
#' @export

choice_parameters <- function(
    # TODO: no alpha, b becomes beta, contains every (mean) preferences
    s = NULL,
    alpha = NULL,
    b = NULL,
    Omega = NULL,
    Sigma = NULL,
    gamma = NULL
  ) {

  ### generate list for parameters
  parameters <- list(
    "s" = s,
    "alpha" = alpha,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "gamma" = gamma
  )

  ### remove missing parameters from the list
  parameters[sapply(parameters, is.null)] <- NULL

  ### ensure that parameters are numerics without missing values
  for (i in seq_along(parameters)) {
    check <- checkmate::check_numeric(parameters[[i]], any.missing = FALSE)
    if (!isTRUE(check)) {
      cli::cli_abort(
        "Input {.var {names(parameters)[i]}} is bad: {check}", call = NULL
      )
    }
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
#' The `choice_parameters` object to be printed.
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
#'   \item{`s`}{drawn from a Dirichlet distribution with concentration 1}
#'   \item{`alpha`}{drawn from a multivariate normal distribution with
#'   zero mean and a diagonal covariance matrix with value 10 on the diagonal}
#'   \item{`b`}{drawn from a multivariate normal distribution with zero
#'   mean and a diagonal covariance matrix with value 10 on the diagonal}
#'   \item{`Omega`}{drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to `P_r` + 2 and scale matrix equal to the identity}
#'   \item{`Sigma`}{drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to `J` + 2 and scale matrix equal to the identity,
#'   in the ordered probit case drawn from a standard normal distribution}
#'   \item{`gamma`}{derived from the logarithmic increases of the utility
#'   thresholds `d`, which are drawn from a multivariate normal
#'   distribution with zero mean and covariance matrix equal to the identity}
#' }
#'
#' @examples
#' choice_formula <- choice_formula(
#'   formula = choice ~ A | B, error_term = "probit", random_effects = "A"
#' )
#' choice_alternatives <- choice_alternatives(J = 3)
#' generate_choice_parameters(
#'   choice_effects = choice_effects(choice_formula, choice_alternatives)
#' )
#'
#' @export

generate_choice_parameters <- function(
    choice_effects,
    C = 1,
    fixed_parameters = choice_parameters()
  ) {

  ### input checks
  check_not_missing(choice_effects)
  is.choice_parameters(fixed_parameters, error = TRUE)
  x <- fixed_parameters
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula[["error_term"]]

  # TODO: which effect get latent classes?
  latent_classes <- choice_formula[["latent_classes"]]

  C <- check_C(C, latent_classes)
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  ordered <- attr(choice_alternatives, "ordered")

  ### validate fixed parameters
  x <- validate_choice_parameters(
    choice_parameters = x, choice_effects = choice_effects, C = C,
    allow_missing = TRUE
  )

  ### generate missing parameters

  # s
  if (C > 1 && is.null(x$s)) {
    x$s <- sort(oeli::rdirichlet(concentration = rep(1, C)), decreasing = TRUE)
  }

  # alpha
  if (P_f > 0 && is.null(x$alpha)) {
    x$alpha <- if (C == 1) {
      oeli::rmvnorm(mean = numeric(P_f), Sigma = 10 * diag(P_f))
    } else {
      t(oeli::rmvnorm(n = C, mean = numeric(P_f), Sigma = 10 * diag(P_f)))
    }
  }

  # b
  if (P_r > 0 && is.null(x$b)) {
    x$b <- if (C == 1) {
      oeli::rmvnorm(mean = numeric(P_r), Sigma = 10 * diag(P_r))
    } else {
      t(oeli::rmvnorm(n = C, mean = numeric(P_r), Sigma = 10 * diag(P_r)))
    }
  }

  # Omega
  if (P_r > 0 && is.null(x$Omega)) {
    x$Omega <- if (C == 1) {
      oeli::rwishart(df = P_r + 2, scale = diag(P_r), inv = TRUE)
    } else {
      do.call(
        cbind,
        lapply(
          replicate(
            C,
            oeli::rwishart(df = P_r + 2, scale = diag(P_r), inv = TRUE),
            simplify = FALSE
          ),
          as.vector
        )
      )
    }
  }

  # Sigma
  if (error_term == "probit" && is.null(x$Sigma)) {
    x$Sigma <- if (ordered) {
      oeli::rwishart(df = 3, scale = diag(1), inv = TRUE)
    } else {
      oeli::rwishart(df = J + 2, scale = diag(J), inv = TRUE)
    }
  }

  # gamma
  if (ordered && is.null(x$gamma)) {
    d <- oeli::rmvnorm(n = 1, mean = numeric(J - 2), Sigma = diag(J - 2))
    x$gamma <- c(0, cumsum(exp(d)))
  }

  ### validate parameters and return
  validate_choice_parameters(
    choice_parameters = x, choice_effects = choice_effects, C = C,
    allow_missing = FALSE
  )
}

#' @rdname choice_parameters
#' @export

validate_choice_parameters <- function(
    choice_parameters,
    choice_effects,
    C = 1,
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
  latent_classes <- choice_formula$latent_classes
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  ordered <- attr(choice_alternatives, "ordered")
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  C <- check_C(C, latent_classes)
  allow_missing <- check_allow_missing(allow_missing)

  ### check parameters

  # s
  if (C > 1) {
    if ("s" %in% names(x)) {
      check <- oeli::check_probability_vector(x$s, len = C)
      if (!isTRUE(check)) {
        cli::cli_abort("Parameter {.var s} is bad: {check}", call = NULL)
      }
      if (is.unsorted(rev(x$s))) {
        cli::cli_abort("Parameter {.var s} is bad: Must be descending", call = NULL)
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var s} is required", call = NULL)
    }
  } else {
    x$s <- NULL
  }

  # alpha
  if (P_f > 0) {
    if ("alpha" %in% names(x)) {
      if (C > 1) {
        check <- checkmate::check_matrix(
          x$alpha, mode = "numeric", any.missing = FALSE, nrows = P_f, ncols = C
        )
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var alpha} is bad: {check}", call = NULL)
        }
      } else {
        check <- oeli::check_numeric_vector(x$alpha, len = P_f)
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var alpha} is bad: {check}", call = NULL)
        }
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var alpha} is required", call = NULL)
    }
  } else {
    x$alpha <- NULL
  }

  # b
  if (P_r > 0) {
    if ("b" %in% names(x)) {
      if (C > 1) {
        check <- checkmate::check_matrix(
          x$b, mode = "numeric", any.missing = FALSE, nrows = P_r, ncols = C
        )
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var b} is bad: {check}", call = NULL)
        }
      } else {
        check <- oeli::check_numeric_vector(x$b, len = P_r)
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var b} is bad: {check}", call = NULL)
        }
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var b} is required", call = NULL)
    }
  } else {
    x$b <- NULL
  }

  # Omega
  if (P_r > 0) {
    if ("Omega" %in% names(x)) {
      if (C > 1) {
        check <- checkmate::check_matrix(
          x$Omega, mode = "numeric", any.missing = FALSE, nrows = P_r^2, ncols = C
        )
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var Omega} is bad: {check}", call = NULL)
        }
        for (c in 1:x$C) {
          check <- oeli::check_covariance_matrix(x$Omega[, c], dim = P_r)
          if (!isTRUE(check)) {
            cli::cli_abort("Parameter {.var Omega[, {c}]} is bad: {check}", call = NULL)
          }
        }
      } else {
        check <- oeli::check_covariance_matrix(x$Omega, dim = P_r)
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var Omega} is bad: {check}", call = NULL)
        }
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var Omega} is required", call = NULL)
    }
  } else {
    x$Omega <- NULL
  }

  # Sigma
  if (error_term == "probit") {
    if ("Sigma" %in% names(x)) {
      if (ordered) {
        if (checkmate::test_number(x$Sigma)) {
          x$Sigma <- matrix(x$Sigma)
        }
        check <- oeli::check_covariance_matrix(x$Sigma, dim = 1)
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var Sigma} is bad: {check}", call = NULL)
        }
      } else {
        check <- oeli::check_covariance_matrix(x$Sigma, dim = J)
        if (!isTRUE(check)) {
          cli::cli_abort("Parameter {.var Sigma} is bad: {check}", call = NULL)
        }
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var Sigma} is required", call = NULL)
    }
  } else {
    x$Sigma <- NULL
  }

  # gamma
  if (ordered) {
    if ("gamma" %in% names(x)) {
      check <- oeli::check_numeric_vector(x$gamma, len = J - 1)
      if (!isTRUE(check)) {
        cli::cli_abort("Parameter {.var gamma} is bad: {check}", call = NULL)
      }
      if (is.unsorted(x$gamma)) {
        cli::cli_abort("Parameter {.var gamma} is bad: Must be ascending", call = NULL)
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var gamma} is required", call = NULL)
    }
  } else {
    x$gamma <- NULL
  }

  ### return object
  return(x)
}

#' @rdname choice_parameters
#'
#' @param choice_parameters
#' TODO
#'
#' @param scale
#' TODO
#'
#' @param level
#' TODO
#'
#' @section Level and scale normalization:
#' Choice models are invariant towards the level and scale of utility, hence
#' a transformation is required for identifiability.
#'
#' For level normalization, we take utility differences:
#' \deqn{\tilde{U}_{ntj} = \tilde{X}_{ntj}' \tilde{\beta}_n +
#' \tilde{\epsilon}_{ntj},}
#' where (choosing some alternative \eqn{k \in \{1,\dots,J\}} as the reference)
#' \eqn{\tilde{U}_{ntj} = U_{ntj} - U_{ntk}},
#' \eqn{\tilde{X}_{ntj} = X_{ntj} - X_{ntk}}, and
#' \eqn{\tilde{\epsilon}_{ntj} = \epsilon_{ntj} - \epsilon_{ntk}} for
#' \eqn{j\neq k}.
#'
#' In the probit model case, the error term differences
#' \eqn{(\tilde{\epsilon}_{nt:})} again are
#' multivariate normally distributed with mean \eqn{0} but transformed
#' covariance matrix \eqn{\tilde{\Sigma}}, also denoted by `Sigma_diff`.
#' See \code{\link[oeli]{diff_cov}} for computing `Sigma_diff` from
#' `Sigma`, and \code{\link[oeli]{undiff_cov}} for the other way around.
#'
#' For level normalization in the ordered model case, we fix
#' \eqn{\gamma_1 = 0}.
#'
#' For scale normalization, we fix the top left element of `Sigma_diff` to
#' the value `scale` (or fix `Sigma = scale` in the ordered probit case).
#' In the logit model case, the scale normalization is already implied by the
#' assumption that the errors are independently extreme value distributed with
#' variance \eqn{\pi^2/6}.
#'
#' @export

identify_choice_parameters <- function(
    choice_parameters,
    scale = 1,
    level = 1
  ) {


}

#' TODO: inherit choice_effects and error_term
#'
#' @param x \[`choice_parameters` | `numeric`()\]\cr
#' The choice parameters.
#'
#' @param names \[`character(1)`\]\cr
#' - `"effect"`
#' - `"generic"`
#' - `"unnamed"`
#'
#' TODO: order of numeric vector

change_format.choice_parameters <- function(
    choice_parameters,
    new_format,
    choice_effects,
    names = "effect"
  ) {


  par <- optimizeR::ParameterSpaces$
    new()$
    switch()


}



