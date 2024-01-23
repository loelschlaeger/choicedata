#' Define choice alternatives
#'
#' @description
#' This function constructs an object of class
#' \code{\link{choice_alternatives}}, which defines the choice alternatives.
#'
#' @param J
#' An \code{integer}, the number of choice alternatives.
#' Must be at least \code{2}.
#' If \code{ordered = TRUE}, must be at least \code{3}.
#' @param alternatives
#' A \code{character} vector, labels for the choice alternatives.
#' Its length must be \code{J}.
#' By default, \code{alternatives = LETTERS[1:J]}.
#' @param base
#' A \code{character}, the name of the base alternative for covariates that are
#' not alternative specific, see details.
#' \code{base} must be contained in \code{alternatives}.
#' Ignored if the model has no alternative specific covariates (e.g., in the
#' ordered case).
#' By default, \code{base} is the first element of \code{alternatives}.
#' @param ordered
#' A \code{logical}, \code{TRUE} if the choice alternatives are ordered and
#' \code{FALSE} (default) else.
#' @inheritParams doc-helper
#'
#' @return
#' A \code{\link{choice_alternatives}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{J}}{The number of choice alternatives.}
#'   \item{\code{alternatives}}{The labels for the choice alternatives.}
#'   \item{\code{base}}{The name of the base alternative.}
#'   \item{\code{ordered}}{Are the choice alternatives ordered?}
#' }
#'
#' @section Base alternative:
#' The full collection of coefficients for covariates that are constant across
#' alternatives (including alternative specific constants) is not identified.
#' To achieve identifiability, the coefficient of one alternative \code{base}
#' is fixed to \code{0}.
#' The other coefficients then have to be interpreted with respect to
#' \code{base}.
#' The base alternative is marked with a \code{*} when printing a
#' \code{\link{choice_alternatives}} object.
#'
#' @export

choice_alternatives <- function(
    J = 2, alternatives = LETTERS[1:J], base = alternatives[1], ordered = FALSE
) {
  check_base(
    base = base, alternatives = alternatives, J = J, ordered = ordered,
    error = TRUE
  )
  if (ordered) {
    base <- NA_character_
  } else {
    alternatives <- sort(alternatives)
  }
  structure(
    list(
      J = as.integer(J),
      alternatives = alternatives,
      base = base,
      ordered = ordered
    ),
    class = c("choice_alternatives", "list")
  )
}

#' @rdname choice_alternatives
#' @export

is.choice_alternatives <- function(x, error = TRUE) {
  check <- inherits(x, "choice_alternatives")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_alternatives}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_alternatives
#' @exportS3Method

print.choice_alternatives <- function(x, ...) {
  is.choice_alternatives(x, error = TRUE)
  cli::cli_h3(paste("Choice alternatives", if (x$ordered) "(ordered)"))
  alt <- x$alternatives
  if (!x$ordered) {
    alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  }
  cli::cat_bullet(alt)
}

