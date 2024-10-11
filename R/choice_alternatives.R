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
#' @return
#' An object of class `choice_alternatives`, which is a `character`
#' vector of the choice alternatives that has the following attributes:
#' \describe{
#'   \item{`J`}{The number of choice alternatives.}
#'   \item{`base`}{The name of the base alternative.}
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
#' @examples
#' choice_alternatives(
#'   J = 3,
#'   alternatives = c("gas", "electricity", "oil"),
#'   base = "gas"
#' )
#'
#' @export

choice_alternatives <- function(
    J = 2,
    alternatives = LETTERS[1:J],
    base = alternatives[1]
) {

  ### input checks
  check_base(base = base, alternatives = alternatives, J = J)
  alternatives <- sort(alternatives)

  ### build object
  structure(
    alternatives,
    "J" = as.integer(J),
    "base" = base,
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
  cli::cli_h3(paste("Choice alternatives"))
  alt <- as.character(x)
  alt[alt == base] <- paste0(alt[alt == base], "*")
  cli::cat_bullet(alt)
  invisible(x)
}

