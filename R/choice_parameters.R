#' Define choice model parameters
#'
#' @description
#' These functions construct and validate an object of class
#' \code{choice_parameters}, which defines the parameters of a choice model.
#'
#' - \code{choice_parameters()} constructs a \code{choice_parameters} object.
#' - \code{sample_choice_parameters()} samples parameters at random, see
#'   details.
#' - \code{validate_choice_parameters()} validates a \code{choice_parameters}
#'   object.
#' - \code{identify_choice_parameters()} applies scale and level normalization.
#'
#' @inheritSection choice_formula The probit and logit model
#'
#' @param C (`integer(1)`)\cr
#' The number of latent classes (if any specified in `choice_formula`).
#'
#' By default, `C = 1`, which effectively means no latent classes.
#'
#' @param s (`numeric(C)`)\cr
#' The latent class weights. Only relevant if latent classes are specified.
#'
#' @param alpha (`numeric(P_f)` or `matrix(nrow = P_f, ncol = C)`)\cr
#' The non-random coefficients. Only relevant if non-random coefficients are
#' specified.
#'
#' Different values are stored column-wise in the case of latent classes.
#'
#' @param b (`numeric(P_r)` or `matrix(nrow = P_r, ncol = C)`)\cr
#' The mean of random effects. Only relevant if random coefficients are
#' specified.
#'
#' Different values are stored column-wise in the case of latent classes.
#'
#' @param Omega (`matrix(P_r)` or `matrix(nrow = P_r^2, ncol = C)`)\cr
#' The covariance matrix of random effects. Only relevant if random coefficients
#' are specified.
#'
#' Different covariance matrices are stored column-wise in vector form in the
#' case of latent classes.
#'
#' @param Sigma (`matrix(nrow = J, ncol = J)` or `numeric(1)`)\cr
#' The error term covariance matrix. Only relevant in the probit model.
#'
#' In the ordered probit model, \code{Sigma} is a single, non-negative
#' \code{numeric}.
#'
#' @param gamma (`numeric(J - 1)`)\cr
#' The utility thresholds. Only relevant in the ordered model case.
#' Must be strictly ascending.
#'
#' \code{gamma} corresponds to \eqn{\gamma_1, \dots, \gamma_{J-1}}, while the
#' lower and upper bounds are \eqn{\gamma_0 = -\infty} and
#' \eqn{\gamma_J = +\infty}.
#'
#' @param choice_effects (`choice_effects`)\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @return
#' An object of class \code{choice_parameters}, which is a \code{list} with the
#' following elements:
#' \describe{
#'   \item{\code{s}}{The class weights (if any).}
#'   \item{\code{alpha}}{The non-random coefficients (if any).}
#'   \item{\code{b}}{The mean of random effects (if any).}
#'   \item{\code{Omega}}{The covariance of random effects (if any).}
#'   \item{\code{Sigma}}{The error term covariance (if any).}
#'   \item{\code{gamma}}{The utility thresholds (if any).}
#' }
#'
#' @export

choice_parameters <- function(
    s = NULL,
    alpha = NULL,
    b = NULL,
    Omega = NULL,
    Sigma = NULL,
    gamma = NULL
  ) {
  parameters <- list(
    "s" = s,
    "alpha" = alpha,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "gamma" = gamma
  )
  parameters[sapply(parameters, is.null)] <- NULL
  for (i in seq_along(parameters)) {
    check <- checkmate::check_numeric(parameters[[i]], any.missing = FALSE)
    if (!isTRUE(check)) {
      cli::cli_abort(
        "Input {.var {names(parameters)[i]}} is bad: {check}", call = NULL
      )
    }
  }
  structure(
    parameters,
    class = c("choice_parameters", "list")
  )
}

#' @noRd

is.choice_parameters <- function(
    x, error = TRUE, var_name = oeli::variable_name(x)
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
#' @inheritParams oeli::print_matrix
#' @exportS3Method

print.choice_parameters <- function(
    x, ..., rowdots = 4, coldots = 4, digits = 2, simplify = FALSE,
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
#' @param fixed_parameters (`choice_parameters`)\cr
#' Optionally a \code{\link{choice_parameters}} object of parameters to keep
#' fixed when sampling other parameters.
#'
#' @section Sampling missing choice model parameters:
#'
#' Unspecified choice model parameters (if required for the model) are drawn
#' independently from the following distributions:
#' \describe{
#'   \item{\code{s}}{drawn from a Dirichlet distribution with concentration 1}
#'   \item{\code{alpha}}{drawn from a multivariate normal distribution with
#'   zero mean and a diagonal covariance matrix with value 10 on the diagonal}
#'   \item{\code{b}}{drawn from a multivariate normal distribution with zero
#'   mean and a diagonal covariance matrix with value 10 on the diagonal}
#'   \item{\code{Omega}}{drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to \code{P_r} + 2 and scale matrix equal to the identity}
#'   \item{\code{Sigma}}{drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to \code{J} + 2 and scale matrix equal to the identity,
#'   in the ordered probit case drawn from a standard normal distribution}
#'   \item{\code{gamma}}{derived from the logarithmic increases of the utility
#'   thresholds \code{d}, which are drawn from a multivariate normal
#'   distribution with zero mean and covariance matrix equal to the identity}
#' }
#'
#' @examples
#' choice_formula <- choice_formula(
#'   formula = choice ~ A | B, error_term = "probit", random_effects = "A"
#' )
#' choice_alternatives <- choice_alternatives(J = 3)
#' sample_choice_parameters(
#'   choice_effects = choice_effects(choice_formula, choice_alternatives)
#' )
#'
#' @export

sample_choice_parameters <- function(
    choice_effects, fixed_parameters = choice_parameters()
  ) {

  ### input checks
  is.choice_parameters(fixed_parameters, error = TRUE)
  x <- fixed_parameters
  model_type <- attr(x, "model_type")
  latent_classes <- attr(x, "latent_classes")
  C <- attr(x, "C")
  is.choice_formula(choice_formula, error = TRUE)
  formula <- choice_formula$formula
  re <- choice_formula$re
  ordered <- choice_formula$ordered
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
  J <- check_J(J)

  ### validate fixed parameters
  x <- validate_choice_parameters(
    choice_parameters = x, choice_formula = choice_formula, J = J,
    allow_missing = TRUE
  )

  ### sample missing parameters

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
  if (model_type == "probit" && is.null(x$Sigma)) {
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
    choice_parameters = x, choice_formula = choice_formula, J = J,
    allow_missing = FALSE
  )
}

#' @rdname choice_parameters
#' @export

validate_choice_parameters <- function(
    choice_parameters, choice_effects, allow_missing = FALSE
  ) {

  ### input checks
  check_not_missing(choice_parameters)
  is.choice_parameters(choice_parameters, error = TRUE)
  x <- choice_parameters
  model_type <- attr(x, "model_type")
  latent_classes <- attr(x, "latent_classes")
  C <- attr(x, "C")
  check_not_missing(choice_formula)
  is.choice_formula(choice_formula, error = TRUE)
  formula <- choice_formula$formula
  re <- choice_formula$re
  ordered <- choice_formula$ordered
  J <- check_J(J)
  P_f <- compute_P_f(formula = formula, re = re, J = J, ordered = ordered)
  P_r <- compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
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
  if (model_type == "probit") {
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
#' @param x
#' Either a \code{\link{choice_parameters}} object or a \code{numeric}
#' \code{vector}.
#'
#' @param mode
#' \itemize{
#'   \item \code{"vector_identified"}: return numeric and named vector of identified parameters
#'   \item \code{"list_identified"}: return list of identified parameters
#'   \item \code{"choice_parameters"}: return choice_parameters object with new normalization
#' }
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
#' covariance matrix \eqn{\tilde{\Sigma}}, also denoted by \code{Sigma_diff}.
#' See \code{\link[oeli]{diff_cov}} for computing \code{Sigma_diff} from
#' \code{Sigma}, and \code{\link[oeli]{undiff_cov}} for the other way around.
#'
#' For level normalization in the ordered model case, we fix
#' \eqn{\gamma_1 = 0}.
#'
#' For scale normalization, we fix the top left element of \code{Sigma_diff} to
#' \eqn{1} (or fix \code{Sigma = 1} in the ordered probit case). In the logit
#' model case, the scale normalization is already implied by the assumption that
#' the errors are independently distributed extreme value with variance
#' \eqn{\pi^2/6}.
#'
#' @export

identify_choice_parameters <- function(
    x, mode, choice_effects, scale = 1, level = 1
  ) {


}






