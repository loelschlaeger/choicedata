#' Define choice model parameters
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choice_parameters}}, which defines the parameters of a choice
#' model, see details.
#'
#' \code{sample_choice_parameters()} draws (missing) choice model
#' parameters at random, see details.
#'
#' \code{identify_choice_parameters()} applies scale and level normalization
#' and transforms a \code{list} of parameters to a \code{vector} of identified
#' parameters to be passed to \code{choice_likelihood()}.
#'
#' @param model_type
#' Either \code{"probit"} (default) to select the probit model, or
#' \code{"logit"} to select the logit model.
#' @param latent_classes
#' A \code{character}, which defines a latent class model, either:
#' \itemize{
#'   \item \code{"none"} for no latent classes (which is equivalent to
#'         setting \code{C = 1}),
#'   \item \code{"fe"} for latent classes only for the fixed effects,
#'   \item \code{"re"} for latent classes only for the random effects,
#'   \item \code{"both"} for latent classes both for the fixed and random
#'         effects.
#' }
#' @param C
#' An \code{integer}, the number (greater or equal 1) of latent classes of
#' decision makers.
#'
#' By default, \code{C = 1}, which is equivalent to
#' \code{latent_classes = "none"}.
#' @param s
#' A \code{numeric} vector of length \code{C}, the vector of class weights.
#' Only relevant if \code{C > 1}.
#'
#' For identifiability of the classes, the vector elements must be decreasing.
#' @param alpha
#' A \code{numeric} vector of length \code{P_f}, the vector of fixed effect
#' coefficients (i.e., coefficients not connected to random effects).
#'
#' If \code{C > 1} and \code{latent_classes = "fe"} or
#' \code{latent_classes = "both"}, a \code{matrix} of dimension
#' \code{P_f} x \code{C}, where column \code{c} contains the fixed effect
#' coefficients for class \code{c}.
#' @param b
#' A \code{numeric} vector of length \code{P_r}, the vector of mean random
#' effects.
#'
#' If \code{C > 1} and \code{latent_classes = "re"} or
#' \code{latent_classes = "both"}, a \code{matrix} of dimension
#' \code{P_r} x \code{C}, where column \code{c} contains the mean random effects
#' for class \code{c}.
#' @param Omega
#' A \code{matrix} of dimension \code{P_r} x \code{P_r}, the covariance matrix
#' of random effects.
#'
#' If \code{C > 1} and \code{latent_classes = "re"} or
#' \code{latent_classes = "both"}, a \code{matrix} of dimension
#' \code{P_r^2} x \code{C}, where column \code{c} contains the covariance matrix
#' of random effects for class \code{c} in vector form.
#' @param Sigma
#' A \code{matrix} of dimension \code{J} x \code{J}, the error term covariance
#' matrix for a probit model. It is ignored if \code{model_type = "logit"}.
#'
#' In the ordered probit model (see details), \code{Sigma} is a
#' \code{matrix} of dimension \code{1} x \code{1} (or simply a single
#' \code{numeric}).
#' @param gamma
#' A \code{numeric} of length \code{J - 1}, the vector of utility thresholds
#' in the ordered model case (see details). Must be ascending.
#'
#' \code{gamma} corresponds to \eqn{\gamma_1, \dots, \gamma_{J-1}}, while the
#' lower and upper bounds are \eqn{\gamma_0 = -\infty} and
#' \eqn{\gamma_J = +\infty}.
#' @inheritParams choice_alternatives
#' @inheritParams expand_Tp
#'
#' @param fixed_parameters
#' TODO
#' @param choice_formula
#' TODO
#' @param allow_missing
#' TODO
#' @param preferences
#' TODO
#' @param choice_identifiers
#' TODO
#'
#' @return
#' A \code{\link{choice_parameters}} object. It contains the elements:
#' \describe{
#'   \item{\code{s}}{The class weights (if any).}
#'   \item{\code{alpha}}{The fixed coefficients (if any).}
#'   \item{\code{b}}{The mean random effects (if any).}
#'   \item{\code{Omega}}{The random effect covariances (if any).}
#'   \item{\code{Sigma}}{The error term covariance matrix.}
#'   \item{\code{gamma}}{The utility thresholds (if any).}
#' }
#'
#' It further has the attributes \code{"model_type"}, \code{"latent_classes"},
#' and \code{"C"}.
#'
#' @section The probit and logit model:
#' Assume that we know the choices of \eqn{N} deciders choosing between
#' \eqn{J \geq 2} alternatives at each of \eqn{T} choice occasions.
#' Specific to each decider, alternative and choice occasion, we observe \eqn{P}
#' covariates, a linear combination of which eventually explains the latent
#' random utility:
#' \deqn{U_{ntj} = X_{ntj}' \tilde{\beta}_n + \epsilon_{ntj},}
#' \eqn{n=1,\dots,N}, \eqn{t=1,\dots,T}, and \eqn{j=1,\dots,J}.
#' Here, \eqn{X_{ntj}} is a (column) vector of \eqn{P} characteristics specific
#' to alternative \eqn{j} as faced by decider \eqn{n} at choice occasion
#' \eqn{t}, \eqn{\tilde{\beta}_n \in \mathbb{R}^{P}} is the coefficient vector
#' encoding the preferences of \eqn{n}, and
#' \eqn{(\epsilon_{nt:}) = (\epsilon_{nt1},\dots,\epsilon_{ntJ})'} is the model's
#' error term vector for \eqn{n} at \eqn{t}.
#'
#' In the probit model case, the error vector \eqn{(\epsilon_{nt:})} is normally
#' distributed with covariance matrix \code{Sigma}. In the logit model case,
#' the components \eqn{\epsilon_{ntj}} of the error vector are independently,
#' identically distributed extreme value.
#'
#' The value \eqn{U_{ntj}} can be interpreted as the decider's utility for
#' alternative \code{j}.
#' It is unobserved by the researcher, but we assume that the deciders know
#' their utilities for each alternative and make a choice which is consistent
#' with utility maximization. Therefore,
#' \deqn{y_{nt} = \operatorname*{argmax}_{j = 1,\dots,J} U_{ntj},}
#' where \eqn{y_{nt}=j} denotes the event that decider \eqn{n} chooses \eqn{j}
#' at her \eqn{t}-th choice occasion.
#'
#' Entries of the decider-specific coefficient vector \eqn{\tilde{\beta}_n} can
#' be fixed across deciders, in which case the coefficient vector is of the form
#' \eqn{\tilde{\beta}_n' = (\alpha', \beta_n')'}, where
#' \eqn{\alpha \in \mathbb{R}^{P_f}} are \eqn{P_f} coefficients that are
#' constant across deciders and \eqn{\beta_n} are \eqn{P_r} decider-specific
#' coefficients, \eqn{P_f + P_r = P}.
#'
#' The decider-specific coefficients are assumed to be realizations of an
#' underlying mixing distribution and to be independent of the characteristics
#' \eqn{X_{ntj}} and the errors \eqn{(\epsilon_{nt:})}.
#' This distribution characterizes heterogeneity among the deciders and allows
#' for individual sensitivities. Typically, a normal or log-normal distribution
#' is imposed.
#'
#' @section The latent class model:
#' The mixing distribution can be discrete, which results in a discrete latent
#' class model where the fixed effects \eqn{\alpha} taking a fixed set of
#' \code{C} distinct values \eqn{\alpha_c}.
#'
#' Alternatively, the mixing distribution can be a mixture of
#' \eqn{P_r}-variate Gaussian densities \eqn{\phi_{P_r}} with mean vectors
#' \eqn{b = (b_c)_{c}} and covariance matrices \eqn{\Omega = (\Omega_c)_{c}}
#' using \eqn{C} components:
#' \deqn{\beta_n\mid b,\Omega \sim \sum_{c=1}^{C} s_c \phi_{P_r} (\cdot \mid
#' b_c,\Omega_c).}
#' Here, \eqn{(s_c)_{c}} are weights satisfying \eqn{0 < s_c\leq 1} for
#' \eqn{c=1,\dots,C} and \eqn{\sum_c s_c=1}.
#'
#' One interpretation of the (discrete) latent class model is obtained by
#' introducing variables \eqn{z=(z_n)_n}, allocating each decision maker
#' \eqn{n} to class \eqn{c} with probability \eqn{s_c}, i.e.,
#' \deqn{\text{Prob}(z_n=c)=s_c \land \beta_n \mid z,b,\Omega \sim
#' \phi_{P_r}(\cdot \mid b_{z_n},\Omega_{z_n}).}
#'
#' @section Ordered choice model:
#' When the set of choice alternatives is ordered, the choice model has only a
#' single utility
#' \deqn{U_{nt} = X_{nt}' \tilde{\beta}_n + \epsilon_{nt},}
#' where \eqn{\epsilon_{nt} \sim \text{MVN}_{1} (0,\Sigma)} in the probit model
#' and logistic in the logit model, per decider \eqn{n} and choice
#' occasion \eqn{t}.
#'
#' This utility can be interpreted as the level of association that \eqn{n} has
#' with the choice question. It falls into discrete categories, which in turn
#' are linked to the ordered alternatives \eqn{j=1,\dots,J}. Formally,
#' \deqn{y_{nt} = \sum_{j = 1,\dots,J} j \cdot I(\gamma_{j-1} < U_{nt} \leq
#' \gamma_{j}),}
#' where \eqn{\gamma_0 = -\infty} and \eqn{\gamma_J = +\infty}. This implies
#' that alternative \eqn{j} is chosen, if the utility falls into the interval
#' \eqn{(\gamma_{j-1}, \gamma_j]}.
#' Monotonicity of the thresholds \eqn{(\gamma_j)_{j=1,\dots,J-1}} is ensured
#' by estimating logarithmic increments \eqn{d_j} with
#' \eqn{\gamma_j = \sum_{i\leq j} \exp{(d_i)}}, \eqn{j=1,\dots,J-1}.
#' For level normalization, we fix \eqn{\gamma_1 = 0}.
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

choice_parameters <- function(
    model_type = "probit", latent_classes = "none", C = 1,
    s = NULL, alpha = NULL, b = NULL, Omega = NULL, Sigma = NULL, gamma = NULL
  ) {
  model_type <- check_model_type(model_type)
  latent_classes <- check_latent_classes(latent_classes)
  C <- check_C(C, latent_classes)
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
    model_type = model_type,
    latent_classes = latent_classes,
    C = C,
    class = c("choice_parameters", "list")
  )
}

#' @rdname choice_parameters
#' @export

is.choice_parameters <- function(x, error = TRUE) {
  check_not_missing(x)
  check <- inherits(x, "choice_parameters")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_parameters}",
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
  cli::cli_h3("Choice model parameters")
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
#' @inheritParams choice_formula
#' @inheritParams choice_data
#'
#' @section Drawing missing choice model parameters:
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
#'   of freedom equal to \code{J} + 2 and scale matrix equal to the identity}
#'   \item{\code{gamma}}{derived from the logarithmic increases of the utility
#'   thresholds \code{d}, which are drawn from a multivariate normal
#'   distribution with zero mean and covariance matrix equal to the identity}
#' }
#'
#' @export
#'
#' @examples
#' # choice_formula <- choice_formula(formula = choice ~ A | B, re = "A")
#' # sample_choice_parameters(
#' #   fixed_parameters = choice_parameters(
#' #     model_type = "probit", latent_classes = "both", C = 2
#' #   ),
#' #   choice_formula = choice_formula, J = 3
#' # )

sample_choice_parameters <- function(
  fixed_parameters = choice_parameters(
    model_type = "probit", latent_classes = "none", C = 1
  ), choice_formula, J
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
  J <- check_J(J, ordered)

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
    choice_parameters, choice_formula, J, allow_missing = FALSE
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
  J <- check_J(J, ordered)
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
#' @param x
#' Either a \code{\link{choice_parameters}} object or a \code{numeric}
#' \code{vector}.
#' @param mode
#' \itemize{
#'   \item \code{"vector_identified"}: return numeric and named vector of identified parameters
#'   \item \code{"list_identified"}: return list of identified parameters
#'   \item \code{"choice_parameters"}: return choice_parameters object with new normalization
#' }
#' @param scale
#' TODO
#' @param level
#' TODO
#' @export

identify_choice_parameters <- function(
    x, mode, scale = 1, level = 1, model_type = "probit",
    latent_classes = "none", C = 1, choice_formula, J
  ) {


}






