#' Define choice model formula
#'
#' @description
#' The \code{choice_formula} object defines the utility equation for a choice
#' model.
#'
#' @param formula \[`formula`\]\cr
#' A symbolic description of the choice model, see details.
#'
#' @param error_term \[`character(1)`\]\cr
#' Defines the model's error term. Current options are:
#' - `"logit"`: errors are assumed to be iid standard Gumbel distributed
#' - `"probit"`: errors are assumed to be multivariate normally distributed
#'
#' @param random_effects \[`character()`\]\cr
#' The names of covariates in \code{formula} connected to a random effect, i.e.,
#' their coefficients follow a random (so-called mixing) distribution.
#'
#' Per default, the mixing distribution is normal. In addition, the log-normal
#' distribution (e.g., for sign-restriction) can be specified via appending
#' \code{+} to the corresponding name. To have random effects for the ASCs, add
#' \code{ASC} (or \code{ASC+}) to \code{random_effects}.
#'
#' @param latent_classes \[`character()`\]\cr
#' The names of covariates in \code{formula} connected to latent classes, see
#' details.
#'
#' @return
#' An object of class \code{choice_formula}, which is a \code{list} with the
#' following elements:
#' \describe{
#'   \item{\code{formula}}{The model formula.}
#'   \item{\code{error_term}}{The model's error term.}
#'   \item{\code{choice}}{The name of the discrete response variable.}
#'   \item{\code{var_types}}{The three different types of covariates.}
#'   \item{\code{ASC}}{Does the model have alternative-specific constants?}
#'   \item{\code{mixing_types}}{The types of random effects (if any).}
#'   \item{\code{latent_classes}}{Specification of latent classes (if any).}
#'   \item{\code{ordered_valid}}{Formula valid for ordered case (see details)?}
#' }
#'
#' @section The probit and logit model:
#' Assume that we know the choices of \eqn{N} deciders choosing between
#' \eqn{J \geq 2} alternatives at each of \eqn{T} choice occasions.
#' Specific to each decider, alternative and choice occasion, we observe \eqn{P}
#' covariates, a linear combination of which explains the latent random utility:
#' \deqn{U_{ntj} = X_{ntj}' \tilde{\beta}_n + \epsilon_{ntj},}
#' \eqn{n=1,\dots,N}, \eqn{t=1,\dots,T}, and \eqn{j=1,\dots,J}.
#' Here, \eqn{X_{ntj}} is a (column) vector of \eqn{P} characteristics specific
#' to alternative \eqn{j} as faced by decider \eqn{n} at choice occasion
#' \eqn{t}, \eqn{\tilde{\beta}_n \in \mathbb{R}^{P}} is the coefficient vector
#' encoding the preferences of \eqn{n}, and
#' \eqn{(\epsilon_{nt:}) = (\epsilon_{nt1},\dots,\epsilon_{ntJ})'} is the
#' model's error term vector for \eqn{n} at \eqn{t}.
#'
#' In the probit model case, the error vector \eqn{(\epsilon_{nt:})} is normally
#' distributed with covariance matrix \code{Sigma}. In the logit model case,
#' the components \eqn{\epsilon_{ntj}} of the error vector are independently,
#' identically distributed extreme value.
#'
#' The value \eqn{U_{ntj}} can be interpreted as the decider's utility for
#' alternative \code{j}. It is unobserved by the researcher, but we assume that
#' the deciders know their utilities for each alternative and make a choice
#' which is consistent with utility maximization. Therefore,
#' \deqn{y_{nt} = \operatorname*{argmax}_{j = 1,\dots,J} U_{ntj},}
#' where \eqn{y_{nt}=j} denotes the event that decider \eqn{n} chooses \eqn{j}
#' at their \eqn{t}-th choice occasion.
#'
#' Entries of the decider-specific coefficient vector \eqn{\tilde{\beta}_n} can
#' be fixed across deciders, in which case the coefficient vector is of the form
#' \eqn{\tilde{\beta}_n' = (\alpha', \beta_n')'}, where
#' \eqn{\alpha \in \mathbb{R}^{P_f}} are \eqn{P_f} coefficients that are
#' constant across deciders and \eqn{\beta_n} are \eqn{P_r} decider-specific
#' coefficients, \eqn{P_f + P_r = P}.
#'
#' The decider-specific coefficients are assumed to be random effects, i.e.,
#' realizations of an underlying mixing distribution and to be independent of
#' the characteristics \eqn{X_{ntj}} and the errors \eqn{(\epsilon_{nt:})}.
#' This distribution characterizes heterogeneity among the deciders and allows
#' for individual sensitivities. Typically, a normal or log-normal distribution
#' is imposed.
#'
#' @section The latent class model:
#' The mixing distribution can be discrete, which results in a discrete latent
#' class model where the non-random effects \eqn{\alpha} take a fixed set of
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
#' One interpretation of the latent class model is obtained by
#' introducing variables \eqn{z=(z_n)_n}, allocating each decision maker
#' \eqn{n} to class \eqn{c} with probability \eqn{s_c}, i.e.,
#' \deqn{\text{Prob}(z_n=c)=s_c \land \beta_n \mid z,b,\Omega \sim
#' \phi_{P_r}(\cdot \mid b_{z_n},\Omega_{z_n}).}
#'
#' @section Specifying the model formula:
#' The structure of \code{formula} is
#' \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the discrete response variable,
#'   \item \code{A} are names of \strong{alternative-specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item \code{B} are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and \code{C} are names of \strong{alternative-specific covariates}
#'   with \strong{alternative-specific coefficients}.
#' }
#'
#' Multiple covariates of one type are separated by a \code{+} sign, e.g.,
#' \code{choice ~ A1 + A2}.
#'
#' By default, alternative-specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+ 0} in the second spot, e.g.,
#' \code{choice ~ A | B + 0 | C}. To not include any covariates of
#' the second type but to estimate ASCs, add \code{1} in the second
#' spot, e.g., \code{choice ~ A | 1 | C}. The expression
#' \code{choice ~ A | 0 | C} is interpreted as no covariates of the second
#' type and no ASCs.
#'
#' Some parts of the formula can be omitted when there is no ambiguity. For
#' example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.
#'
#' In the ordered case, since only a single utility is modeled, no ASCs and no
#' alternative-specific covariates can be included. Hence, in the ordered case,
#' \code{formula} must be of the form, e.g.,  \code{choice ~ 0 | A + B + 0}.
#'
#' @examples
#' choice_formula(
#'   formula = choice ~ A | B | C,
#'   error_term = "probit",
#'   random_effects = c("A", "B+"),
#'   latent_classes = "C"
#' )
#'
#' @export

choice_formula <- function(
    formula, error_term, random_effects = NULL, latent_classes = NULL
  ) {

  ### input checks
  formula <- check_formula(formula)
  error_term <- check_error_term(error_term)
  random_effects <- check_random_effects(random_effects)
  latent_classes <- check_latent_classes(latent_classes)

  ### read formula
  formula_parts <- as.character(formula)
  if (length(formula_parts) != 3) {
    cli::cli_abort(
      "Input {.var formula} must be of the form {.val <choice> ~ <covariates>}",
      call = NULL
    )
  }
  var_types <- trimws(
    strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]]
  )
  if (length(var_types) > 3) {
    cli::cli_abort(
      "Input {.var formula} should have no more than two '|' separators",
      call = NULL
    )
  }
  while (length(var_types) < 3) {
    var_types <- c(var_types, NA_character_)
  }
  var_types <- lapply(strsplit(var_types, split = "+", fixed = TRUE), trimws)
  ASC <- ifelse(0 %in% var_types[[2]], FALSE, TRUE)
  var_types <- lapply(var_types, function(x) x[!x %in% c(0, 1, NA)])
  if ("ASC" %in% unlist(var_types)) {
    cli::cli_abort(
      "Covariates named {.val ASC} in {.var formula} are not allowed",
      call = NULL
    )
  }
  if (any(duplicated(unlist(var_types)))) {
    dup_ind <- which(duplicated(unlist(var_types)))[1]
    cov_dup <- unlist(var_types)[dup_ind]
    cli::cli_abort(
      "Input {.var formula} contains covariate {.val {cov_dup}} multiple times",
      call = NULL
    )
  }
  choice <- formula_parts[2]
  if (choice %in% unlist(var_types)) {
    cli::cli_abort(
      "Variable {.val {choice}} cannot occur on both sides of {.var formula}",
      call = NULL
    )
  }

  ### determine mixing types
  mixing_types <- character()
  for (random_effect in random_effects) {
    if (endsWith(random_effect, "+")) {
      random_effect <- sub(".{1}$", "", random_effect)
      mixing_type <- "log-normal"
    } else {
      mixing_type <- "normal"
    }
    if (random_effect %in% names(mixing_types)) {
      cli::cli_abort(
        "Multiple random effects specifications for {.val {random_effect}}
        detected",
        call = NULL
      )
    } else if (!random_effect %in% c(unlist(var_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var random_effects} contains {.val {random_effect}}, but it is
        not on the right hand side of {.var formula}",
        call = NULL
      )
    } else {
      mixing_types[random_effect] <- mixing_type
    }
  }

  ### determine latent classes
  for (latent_class in latent_classes) {
    if (!latent_class %in% c(unlist(var_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var latent_classes} contains {.val {latent_class}}, but it is
        not on the right hand side of {.var formula}",
        call = NULL
      )
    }
  }

  ### build object
  structure(
    list(
      formula = formula,
      error_term = error_term,
      choice = choice,
      var_types = var_types,
      ASC = ASC,
      mixing_types = mixing_types,
      latent_classes = latent_classes,
      ordered_valid = all(sapply(var_types[c(1, 3)], length) == 0) & !ASC
    ),
    class = c("choice_formula", "list")
  )
}

#' @noRd

is.choice_formula <- function(
    x, error = FALSE, var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_formula")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_formula}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_formula
#'
#' @param x \[`choice_formula`\]\cr
#' The `choice_formula` object to be printed.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_formula <- function(x, ...) {
  is.choice_formula(x, error = TRUE)
  cli::cli_h3("Choice formula")
  ul <- cli::cli_ul()
  cli::cli_li(deparse1(x$formula))
  cli::cli_li(paste("error term:", x$error_term))
  if (length(x$mixing_types) > 0) {
    cli::cli_li("random effects:")
    ul2 <- cli::cli_ul()
    cli::cli_li(paste0(names(x$mixing_types), ": ", x$mixing_types))
    cli::cli_end(ul2)
  }
  if (length(x$latent_classes) > 0) {
    cli::cli_li("latent classes:")
    ul2 <- cli::cli_ul()
    cli::cli_li(x$latent_classes)
    cli::cli_end(ul2)
  }
  cli::cli_end(ul)
  invisible(x)
}

