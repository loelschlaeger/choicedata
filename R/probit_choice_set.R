#' Define probit choice set
#'
#' @description
#' This function constructs an object of class
#' \code{\link{probit_choice_set}}, which defines the choice set.
#'
#' @param probit_alternatives
#' A \code{\link{probit_alternatives}} object.
#' @param ranked
#' Either \code{TRUE} for ranked choices or \code{FALSE} (default), else.
#'
#' @return
#' A \code{\link{probit_choice_set}} object. It is a \code{character}
#' \code{vector} with the labels for the elements in the choice set.
#'
#' @section Ranked choices:
#' TODO
#'
#' @export

probit_choice_set <- function(
    probit_alternatives, ranked = FALSE, delimiter = "_"
  ) {
  checkmate::assert_class(probit_alternatives, "probit_alternatives")
  checkmate::assert_flag(ranked)
  checkmate::assert_string(delimiter, n.chars = 1)
  choice_set <- if (ranked) {

  } else {
    probit_alternatives$alternatives
  }
  structure(
    choice_set,
    class = c("probit_choice_set", "list")
  )
}

#' @rdname probit_choice_set
#' @param x
#' A \code{\link{probit_choice_set}} object.
#' @export

is.probit_choice_set <- function(x) {
  inherits(x, "probit_choice_set")
}

#' @rdname probit_choice_set
#' @exportS3Method
#' @param ...
#' Currently not used.

print.probit_probit_choice_set <- function(x, ...) {
  checkmate::assert_class(x, "probit_probit_choice_set")
  # TODO
}

