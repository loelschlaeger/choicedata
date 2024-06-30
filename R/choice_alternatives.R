#' Define choice alternatives
#'
#' @description
#' This function constructs an object of class
#' \code{\link{choice_alternatives}}, which defines the set of choice
#' alternatives.
#'
#' @param J (`integer(1)`)\cr
#' The number of choice alternatives.
#'
#' Must be at least \code{2}.
#'
#' @param alternatives (`character(J)`)\cr
#' Labels for the choice alternatives.
#'
#' @param base (`character(1)`)\cr
#' The name of the base alternative for covariates that are
#' not alternative specific, see details.
#'
#' \code{base} must be contained in \code{alternatives}.
#'
#' Ignored if the model has no alternative specific covariates (in particular if
#' \code{ordered = TRUE}).
#'
#' By default, \code{base} is the first element of \code{alternatives}.
#'
#' @param ordered (`logical(1)`)\cr
#' Are the choice alternatives ordered?
#'
#' @return
#' An object of class \code{\link{choice_alternatives}}, which is a
#' \code{character} vector of the choice alternatives and has the following
#' attributes:
#' \describe{
#'   \item{\code{J}}{The number of choice alternatives.}
#'   \item{\code{base}}{The name of the base alternative.}
#'   \item{\code{ordered}}{Are the choice alternatives ordered?}
#' }
#'
#' @section Base alternative:
#' The full set of coefficients for covariates that are constant across
#' alternatives (including alternative specific constants) is not identified.
#' To achieve identifiability, the coefficient of alternative \code{base}
#' is fixed to \code{0}. The other coefficients then have to be interpreted with
#' respect to \code{base}. The base alternative is marked with a \code{*} when
#' printing a \code{\link{choice_alternatives}} object.
#'
#' @export

choice_alternatives <- function(
    J = 2, alternatives = LETTERS[1:J], base = alternatives[1], ordered = FALSE
) {
  check_base(base = base, alternatives = alternatives, J = J)
  check_ordered(ordered)
  if (ordered) {
    base <- NA_character_
  } else {
    alternatives <- sort(alternatives)
  }
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
    x, error = FALSE, var_name = oeli::variable_name(x)
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
#' @inheritParams doc-helper
#' @exportS3Method

print.choice_alternatives <- function(x, ...) {
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

