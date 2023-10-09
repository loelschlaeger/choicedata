#' Define probit choice alternatives
#'
#' @description
#' This function constructs an object of class
#' \code{\link{probit_alternatives}}, which defines the choice alternatives.
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
#' ordered probit case).
#' By default, \code{base} is the first element of \code{alternatives}.
#' @param ordered
#' A \code{logical}, \code{TRUE} if the choice alternatives are ordered and
#' \code{FALSE} (default) else.
#'
#' @return
#' A \code{\link{probit_alternatives}} object.
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
#' is typically fixed to \code{0}.
#' The other coefficients then have to be interpreted with respect to
#' \code{base}.
#' The base alternative is marked with a \code{*} when printing a
#' \code{\link{probit_alternatives}} object.

probit_alternatives <- function(
    J = 2, alternatives = LETTERS[1:J], base = alternatives[1], ordered = FALSE
) {
  checkmate::assert_flag(ordered)
  checkmate::assert_int(J, lower = 2 + ordered)
  J <- as.integer(J)
  checkmate::assert_character(
    alternatives, any.missing = FALSE, len = J, unique = TRUE
  )
  checkmate::assert_string(base)
  stopifnot(
    "Base alternative must be in alternative set." = base %in% alternatives
  )
  if (ordered) {
    base <- NA_character_
  } else {
    alternatives <- sort(alternatives)
  }
  structure(
    list(
      J = J,
      alternatives = alternatives,
      base = base,
      ordered = ordered
    ),
    class = "probit_alternatives"
  )
}

#' @rdname probit_alternatives
#' @param x
#' A \code{\link{probit_alternatives}} object.

is.probit_alternatives <- function(x) {
  inherits(x, "probit_alternatives")
}

#' @rdname probit_alternatives
#' @exportS3Method
#' @param ...
#' Currently not used.

print.probit_alternatives <- function(x, ...) {
  checkmate::assert_class(x, "probit_alternatives")
  alt <- x$alternatives
  if (!x$ordered) {
    alt[alt == x$base] <- paste0(alt[alt == x$base], "*")
  }
  cat("Alternatives:", alt, if (x$ordered) "(ordered)")
}

