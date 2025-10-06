#' Define choice model parameters
#'
#' @description
#' These functions construct, validate, and transform an object of class
#' `choice_parameters`, which defines the parameters of a choice model.
#'
#' - `choice_parameters()` constructs a `choice_parameters` object.
#' - `generate_choice_parameters()` samples parameters at random, see details.
#' - `validate_choice_parameters()` validates a `choice_parameters` object.
#' - `switch_parameter_space()` transforms a `choice_parameters` object between
#'    the interpretation and optimization space, see details.
#'
#' @param beta \[`numeric(P)`\]\cr
#' The coefficient vector of length `P` (number of effects) for computing the
#' linear-in-parameters systematic utility \eqn{V = X\beta}.
#'
#' @param Omega \[`matrix(nrow = P_r, ncol = P_r)` | `NULL`\]\cr
#' The covariance matrix of random effects of dimension `P_r` times `P_r`,
#' where `P_r` \eqn{\leq} `P` is the number of random effects.
#'
#' Can be `NULL` in the case of `P_r = 0`.
#'
#' @param Sigma \[`matrix(nrow = J, ncol = J)` | `numeric(1)`\]\cr
#' Only relevant in the probit model. For unordered alternatives it is the
#' covariance matrix of dimension `J` times `J` (number of alternatives) for
#' the Gaussian error term \eqn{\epsilon = U - V}. In ordered models it reduces
#' to a single variance term.
#'
#' @param gamma \[`numeric(J - 1)` | `NULL`\]\cr
#' Optional vector of strictly increasing threshold parameters for ordered
#' models. The first element must equal zero for identification. Ignored for
#' unordered alternatives.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @return
#' An object of class `choice_parameters`, which is a `list` with the elements:
#' \describe{
#'   \item{`beta`}{The coefficient vector (if any).}
#'   \item{`Omega`}{The covariance matrix of random effects (if any).}
#'   \item{`Sigma`}{The error term covariance matrix (or variance in ordered
#'     models).}
#'   \item{`gamma`}{Threshold parameters for ordered models (if any).}
#' }
#'
#' @export
#'
#' @keywords model
#'
#' @examples
#' ### generate choice parameters at random
#' J <- 3
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ A | B, error_term = "probit",
#'     random_effects = c("A" = "cn")
#'   ),
#'   choice_alternatives = choice_alternatives(J = J)
#' )
#' choice_parameters <- generate_choice_parameters(
#'   choice_effects = choice_effects,
#'   fixed_parameters = choice_parameters(
#'     Sigma = diag(c(0, rep(1, J - 1))) # scale and level normalization
#'   )
#' )

choice_parameters <- function(
    beta = NULL,
    Omega = NULL,
    Sigma = NULL,
    gamma = NULL
  ) {

  ### generate list for parameters
  parameters <- list(
    "beta" = beta,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "gamma" = gamma
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
  validate_choice_object(
    x = x,
    class_name = "choice_parameters",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_parameters
#'
#' @param fixed_parameters \[`choice_parameters`\]\cr
#' Optionally a `choice_parameters` object of parameters to keep
#' fixed when sampling other parameters.
#'
#' @section Sampling missing choice model parameters:
#'
#' Unspecified choice model parameters (if required) are drawn
#' independently from the following distributions:
#' \describe{
#'   \item{`beta`}{Drawn from a multivariate normal distribution with zero
#'   mean and a diagonal covariance matrix with value 10 on the diagonal.}
#'   \item{`Omega`}{Drawn from an Inverse-Wishart distribution with degrees
#'   of freedom equal to `P_r` + 2 and scale matrix equal to the identity.}
#'   \item{`Sigma`}{The first row and column are fixed to 0 for level
#'   normalization. The \eqn{(2, 2)}-value is fixed to 1 for scale
#'   normalization. The lower right block is drawn from an Inverse-Wishart
#'   distribution with degrees of freedom equal to `J` + 1 and scale matrix
#'   equal to the identity.}
#' }
#'
#' @export

generate_choice_parameters <- function(
    choice_effects,
    fixed_parameters = choice_parameters()
  ) {

  ### input checks
  check_not_missing(choice_effects)
  is.choice_parameters(fixed_parameters, error = TRUE)
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula[["error_term"]]
  P <- compute_P(choice_effects)
  P_r <- compute_P_r(choice_effects)
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))

  ### validate fixed parameters
  x <- validate_choice_parameters(
    choice_parameters = fixed_parameters, choice_effects = choice_effects,
    allow_missing = TRUE
  )

  ### generate missing parameters

  # beta
  if (P > 0 && is.null(x$beta)) {
    x$beta <- oeli::rmvnorm(mean = numeric(P), Sigma = 10 * diag(P))
  }

  # Omega
  if (P_r > 0 && is.null(x$Omega)) {
    x$Omega <- oeli::rwishart(df = P_r + 2, scale = diag(P_r), inv = TRUE)
  }

  # Sigma
  if (error_term == "probit" && is.null(x$Sigma)) {
    if (ordered_alternatives) {
      x$Sigma <- 1
    } else {
      Sigma <- matrix(0, J, J)
      Sigma[-1, -1] <- oeli::rwishart(
        df = J + 1, scale = diag(J - 1), inv = TRUE
      )
      Sigma <- Sigma / Sigma[2, 2]
      x$Sigma <- Sigma
    }
  }

  if (ordered_alternatives && is.null(x$gamma)) {
    if (J < 2) {
      cli::cli_abort(
        "Ordered choice models must have at least two categories.",
        call = NULL
      )
    }
    if (J == 2) {
      x$gamma <- 0
    } else {
      increments <- exp(stats::rnorm(J - 2))
      x$gamma <- c(0, cumsum(increments))
    }
  }

  ### validate parameters and return
  validate_choice_parameters(
    choice_parameters = choice_parameters(
      beta = x$beta, Omega = x$Omega, Sigma = x$Sigma, gamma = x$gamma
    ),
    choice_effects = choice_effects,
    allow_missing = FALSE
  )
}

#' @rdname choice_parameters
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' A `choice_parameters` object.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object describing the utility
#' specification.
#'
#' @param allow_missing \[`logical(1)`\]\cr
#' Should parameters be allowed to be missing (`TRUE`) or must all required
#' elements be present (`FALSE`)?
#'
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
  ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))
  P <- compute_P(choice_effects)
  P_r <- compute_P_r(choice_effects)
  allow_missing <- check_allow_missing(allow_missing)

  ### check parameters

  # beta
  if (P > 0) {
    if ("beta" %in% names(x)) {
      oeli::input_check_response(
        check = oeli::check_numeric_vector(x$beta, len = P),
        var_name = "beta"
      )
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var beta} is required", call = NULL)
    }
  } else {
    x$beta <- NULL
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
      if (ordered_alternatives) {
        oeli::input_check_response(
          check = checkmate::check_number(x$Sigma, lower = .Machine$double.eps),
          var_name = "Sigma"
        )
      } else {
        oeli::input_check_response(
          check = oeli::check_covariance_matrix(x$Sigma, dim = J),
          var_name = "Sigma"
        )
      }
    } else if (!allow_missing) {
      cli::cli_abort("Parameter {.var Sigma} is required", call = NULL)
    }
  } else {
    x$Sigma <- NULL
  }

  if (ordered_alternatives) {
      if ("gamma" %in% names(x)) {
        required_len <- max(J - 1, 0L)
        oeli::input_check_response(
          check = checkmate::check_numeric(
            x$gamma,
            any.missing = FALSE,
            min.len = required_len
          ),
          var_name = "gamma"
        )
        gamma_vec <- x$gamma
        if (length(gamma_vec) > 1 && any(diff(gamma_vec) <= 0)) {
          cli::cli_abort(
            "Threshold parameters {.field gamma} must be strictly increasing.",
            call = NULL
          )
        }
        if (required_len > 0 && length(gamma_vec) > 0 &&
            !isTRUE(all.equal(gamma_vec[1], 0))) {
          cli::cli_abort(
            "The first ordered threshold must be fixed at zero for
            identification.",
            call = NULL
          )
        }
        if (length(gamma_vec) != required_len) {
          cli::cli_abort(
            "Ordered models require exactly {J - 1} threshold parameters.",
            call = NULL
          )
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
#' @section Parameter spaces:
#'
#' The `switch_parameter_space()` function transforms a `choice_parameters`
#' object between the interpretation and optimization space.
#'
#' - The interpretation space is a `list` of (not necessarily identified)
#'   parameters that can be interpreted.
#'
#' - The optimization space is a `numeric` vector of identified parameters that
#'   can be optimized:
#'
#'   - `beta` is not transformed
#'   - the first row and column of `Sigma` are fixed to 0 for level
#'     normalization and the second diagonal element is fixed to 1 for scale
#'     normalization
#'   - the covariance matrices (`Omega` and `Sigma`) are transformed to their
#'     vectorized Cholesky factor (diagonal fixed to be positive for uniqueness)
#'
#' @export

switch_parameter_space <- function(choice_parameters, choice_effects) {

  ### input checks
  check_not_missing(choice_parameters)
  is.choice_effects(choice_effects, error = TRUE)

  ### extract information
  P <- compute_P(choice_effects)
  P_r <- compute_P_r(choice_effects)
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  J <- attr(choice_alternatives, "J")
  ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))

  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term

  sigma_length <- if (identical(error_term, "probit")) {
    if (ordered_alternatives) 1L else J * (J - 1) / 2 - 1L
  } else {
    0L
  }
  gamma_length <- if (ordered_alternatives) max(J - 2L, 0L) else 0L

  sigma_o2i <- if (identical(error_term, "probit")) {
    if (ordered_alternatives) {
      function(x) {
        if (!length(x)) NULL else unname(exp(x))
      }
    } else {
      function(x) {
        oeli::undiff_cov(oeli::chol_to_cov(c(1, x)), ref = 1)
      }
    }
  } else {
    function(x) NULL
  }
  sigma_i2o <- if (identical(error_term, "probit")) {
    if (ordered_alternatives) {
      function(x) {
        if (is.null(x)) return(numeric())
        structure(log(x), names = "sigma")
      }
    } else {
      function(x) {
        l <- oeli::cov_to_chol(oeli::diff_cov(x), unique = TRUE)[-1]
        structure(
          l,
          names = paste0("l_", seq_along(l) + 1, recycle0 = TRUE)
        )
      }
    }
  } else {
    function(x) numeric()
  }
    gamma_o2i <- if (ordered_alternatives) {
      function(x) {
        if (!length(x)) {
          if (J <= 1L) {
            numeric()
          } else {
            unname(c(0))
          }
        } else {
          unname(c(0, cumsum(exp(unname(x)))))
        }
      }
  } else {
    function(x) NULL
  }
  gamma_i2o <- if (ordered_alternatives) {
    function(x) {
      if (is.null(x) || length(x) <= 1L) {
        numeric()
      } else {
        diffs <- diff(x)
        structure(
          log(diffs),
          names = paste0("g_", seq_along(diffs) + 1, recycle0 = TRUE)
        )
      }
    }
  } else {
    function(x) numeric()
  }

  ### build ParameterSpaces object
  parameter_names <- c("beta", "Omega", "Sigma", "gamma")
  parameter_lengths_in_o_space <- c(
    P,
    P_r * (P_r + 1) / 2,
    sigma_length,
    gamma_length
  )
  par <- optimizeR::ParameterSpaces$
    new(
      parameter_names = parameter_names,
      parameter_lengths_in_o_space = parameter_lengths_in_o_space
    )$
    o2i(
      "beta" = function(x) unname(x),
      "Omega" = function(x) {
        if (length(x) == 0) return(NULL)
        oeli::chol_to_cov(x)
      },
      "Sigma" = sigma_o2i,
      "gamma" = gamma_o2i
    )$
    i2o(
      "beta" = function(x) {
        structure(
          x,
          names = paste0("beta_", seq_along(x), recycle0 = TRUE)
        )
      },
      "Omega" = function(x) {
        if (is.null(x)) return(numeric())
        o <- oeli::cov_to_chol(x, unique = TRUE)
        structure(
          o,
          names = paste0("o_", seq_along(o), recycle0 = TRUE)
        )
      },
      "Sigma" = sigma_i2o,
      "gamma" = gamma_i2o
    )

  ### transform and return
  choice_parameters_transformed <- par$switch(choice_parameters)
  structure(
    choice_parameters_transformed,
    class = unique(c("choice_parameters", class(choice_parameters_transformed)))
  )
}
