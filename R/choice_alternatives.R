#' Define choice alternatives
#'
#' @description
#' The `choice_alternatives` object defines the set of choice alternatives.
#'
#' @param J \[`integer(1)`\]\cr
#' The number of choice alternatives.
#'
#' Must be at least two.
#'
#' @param alternatives \[`character(J)`\]\cr
#' Labels for the choice alternatives.
#'
#' @param base \[`character(1)`\]\cr
#' The name of the base alternative for covariates that are not
#' alternative-specific, see details.
#'
#' `base` must be contained in `alternatives`.
#'
#' By default, `base` is the first element of `alternatives`.
#'
#' @param ordered \[`logical(1)`\]\cr
#' Are the choice alternatives ordered?
#'
#' @return
#' An object of class `choice_alternatives`, which is a `character`
#' vector of the choice alternatives that has the following attributes:
#' \describe{
#'   \item{`J`}{The number of choice alternatives.}
#'   \item{`base`}{The name of the base alternative.}
#'   \item{`ordered`}{Are the choice alternatives ordered?}
#' }
#'
#' @section Base alternative:
#' The full set of coefficients for covariates that are constant across
#' alternatives (including alternative-specific constants) is not identified.
#' To achieve identifiability, the coefficient of alternative `base`
#' is fixed to zero. The other coefficients then have to be interpreted with
#' respect to `base`. The base alternative is marked with a `*` when
#' printing a `choice_alternatives` object.
#'
#' @section Ordered choice alternatives:
#' When the set of choice alternatives is ordered, the choice model has only a
#' single utility equation
#' \deqn{U_{nt} = X_{nt}' \tilde{\beta}_n + \epsilon_{nt},}
#' per decider \eqn{n} and choice occasion \eqn{t}, where
#' \eqn{\epsilon_{nt} \sim \text{MVN}_{1} (0,\Sigma)} in the probit model
#' and logistic in the logit model.
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
#' @examples
#' choice_alternatives(
#'   J = 3,
#'   alternatives = c("gas", "electricity", "oil"),
#'   base = "gas",
#'   ordered = FALSE
#' )
#'
#' @export

choice_alternatives <- function(
    J = 2,
    alternatives = LETTERS[1:J],
    base = alternatives[1],
    ordered = FALSE
) {

  ### input checks
  check_base(base = base, alternatives = alternatives, J = J)
  check_ordered(ordered)
  if (ordered) {
    base <- NA_character_
  } else {
    alternatives <- sort(alternatives)
  }

  ### build object
  structure(
    alternatives,
    "J" = as.integer(J),
    "base" = base,
    "ordered" = ordered,
    class = c("choice_alternatives", "character")
  )
}

#' @noRd

is.choice_alternatives <- function(
    x,
    error = FALSE,
    var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_alternatives")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_alternatives}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_alternatives
#'
#' @param x \[`choice_alternatives`\]\cr
#' The `choice_alternatives` object to be printed.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_alternatives <- function(
    x,
    ...
  ) {
  is.choice_alternatives(x, error = TRUE)
  base <- attr(x, "base")
  ordered <- attr(x, "ordered")
  cli::cli_h3(paste("Choice alternatives", if (ordered) "(ordered)"))
  alt <- as.character(x)
  if (!ordered) {
    alt[alt == base] <- paste0(alt[alt == base], "*")
  }
  cli::cat_bullet(alt)
  invisible(x)
}

