#' Define choice model formula
#'
#' @description
#' The `choice_formula` object defines the utility equation for a choice model.
#'
#' @param formula \[`formula`\]\cr
#' A symbolic description of the choice model, see details.
#'
#' @param error_term \[`character(1)`\]\cr
#' Defines the model's error term. Current options are:
#' - `"probit"`: errors are assumed to be multivariate normally distributed
#'
#' @param random_effects \[`character()`\]\cr
#' The names of covariates in `formula` connected to a random effect, i.e.,
#' their coefficients follow a random (so-called mixing) distribution.
#'
#' To have random effects for the ASCs, `ASC` to `random_effects`.
#'
#' Per default, the mixing distribution is normal.
#'
#' @return
#' An object of class `choice_formula`, which is a `list` of the elements:
#' \describe{
#'   \item{`formula`}{The model formula.}
#'   \item{`error_term`}{The model's error term.}
#'   \item{`choice`}{The name of the discrete response variable.}
#'   \item{`var_types`}{The three different types of covariates.}
#'   \item{`ASC`}{Does the model have alternative-specific constants?}
#'   \item{`mixing_types`}{The types of random effects (if any).}
#' }
#'
#' @section Choice models:
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
#' distributed with covariance matrix `Sigma`.
#'
#' The value \eqn{U_{ntj}} can be interpreted as the decider's utility for
#' alternative \eqn{j}. It is unobserved by the researcher, but we assume that
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
#' for individual sensitivities.
#'
#' @section Specifying the model formula:
#' The structure of `formula` is `choice ~ A | B | C`, where
#' \itemize{
#'   \item `choice` is the name of the discrete response variable,
#'   \item `A` are names of \strong{alternative-specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item `B` are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and `C` are names of \strong{alternative-specific covariates}
#'   with \strong{alternative-specific coefficients}.
#' }
#'
#' Multiple covariates of one type are separated by a `+` sign, e.g.,
#' `choice ~ A1 + A2`.
#'
#' By default, alternative-specific constants (ASCs) are added to the model.
#' They can be removed by adding `+ 0` in the second spot, e.g.,
#' `choice ~ A | B + 0 | C`. To not include any covariates of
#' the second type but to estimate ASCs, add `1` in the second
#' spot, e.g., `choice ~ A | 1 | C`. The expression
#' `choice ~ A | 0 | C` is interpreted as no covariates of the second
#' type and no ASCs.
#'
#' Some parts of the formula can be omitted when there is no ambiguity. For
#' example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.
#'
#' In the ordered case, since only a single utility is modeled, no ASCs and no
#' alternative-specific covariates can be included. Hence, in the ordered case,
#' `formula` must be of the form, e.g., `choice ~ 0 | A + B + 0`.
#'
#' @examples
#' choice_formula(
#'   formula = choice ~ A | B | C,
#'   error_term = "probit",
#'   random_effects = c("A", "B")
#' )
#'
#' @export

choice_formula <- function(
    formula,
    error_term,
    random_effects = character()
  ) {

  ### input checks
  formula <- check_formula(formula)
  error_term <- check_error_term(error_term)
  random_effects <- check_random_effects(random_effects)

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
    mixing_type <- "normal"
    if (!random_effect %in% c(unlist(var_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var random_effects} contains {.val {random_effect}}, but it is
        not on the right hand side of {.var formula}",
        call = NULL
      )
    } else {
      mixing_types[random_effect] <- mixing_type
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
      ordered_valid = all(sapply(var_types[c(1, 3)], length) == 0) & !ASC
    ),
    class = c("choice_formula", "list")
  )
}

#' @noRd

is.choice_formula <- function(
    x,
    error = FALSE,
    var_name = oeli::variable_name(x)
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

print.choice_formula <- function(
    x,
    ...
  ) {
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
  cli::cli_end(ul)
  invisible(x)
}

