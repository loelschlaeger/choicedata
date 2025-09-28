#' Define choice alternatives
#'
#' @description
#' The `choice_alternatives` object defines the set of choice alternatives.
#'
#' @param J \[`integer(1)`\]\cr
#' The number \eqn{\geq 2} of choice alternatives.
#'
#' @param alternatives \[`character(J)`\]\cr
#' Unique labels for the choice alternatives.
#'
#' @param base \[`character(1)`\]\cr
#' The name of the base alternative for alternative-constant covariates, see
#' details.
#'
#' If `NULL` (default), it is set to the first element of the sorted
#' `alternatives`.
#'
#' @param ordered \[`logical(1)`\]\cr
#' Should the supplied order of `alternatives` be preserved and treated as an
#' intrinsic ranking (for ordered response models)?
#'
#' When `TRUE`, the alternatives are kept in the given order.
#'
#' Otherwise, they are sorted alphabetically.
#'
#' @return
#' An object of class `choice_alternatives`, i.e. a `character` vector of the
#' choice alternatives with attributes:
#' \describe{
#'   \item{`J`}{The number of choice alternatives.}
#'   \item{`base`}{The name of the base alternative.}
#'   \item{`ordered`}{Do the alternatives encode an inherent ordering?}
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
#' @export
#'
#' @keywords model
#'
#' @examples
#' choice_alternatives(
#'   J = 3,
#'   alternatives = c("gas", "electricity", "oil"),
#'   base = "gas"
#' )

choice_alternatives <- function(
  J = 2,
  alternatives = LETTERS[1:J],
  base = NULL,
  ordered = FALSE
) {

  ### input checks
  ordered <- check_ordered(ordered)
  alternatives <- check_alternatives(alternatives = alternatives, J = J)
  if (!isTRUE(ordered)) alternatives <- sort(alternatives)
  if (is.null(base)) base <- alternatives[1]
  check_base(base = base, alternatives = alternatives, J = J)

  ### build object
  structure(
    alternatives,
    J = as.integer(J),
    base = base,
    ordered = ordered,
    class = c("choice_alternatives", "character")
  )
}

#' @noRd

is.choice_alternatives <- function(
  x,
  error = FALSE,
  var_name = oeli::variable_name(x)
) {
  validate_choice_object(
    x = x,
    class_name = "choice_alternatives",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_alternatives
#'
#' @param x \[`choice_alternatives`\]\cr
#' A `choice_alternatives` object.
#'
#' @param ... Currently not used.
#'
#' @exportS3Method

print.choice_alternatives <- function(x, ...) {
  is.choice_alternatives(x, error = TRUE)
  base <- attr(x, "base")
  cli::cli_h3("Choice alternatives")
  alt <- as.character(x)
  alt[alt == base] <- paste0(alt[alt == base], "*")
  cli::cat_bullet(alt)
  invisible(x)
}
