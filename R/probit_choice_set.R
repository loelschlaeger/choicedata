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
#' @param delimiter
#' A \code{character}, the delimiter between alternative names in the ranked
#' case. By default, \code{delimiter = "_"}.
#'
#' @return
#' A \code{\link{probit_choice_set}} object. It is a \code{character}
#' \code{vector} with the labels for the elements in the choice set.
#'
#' @section Ranked choices:
#' Ranked choices are yet another model variation: rather than recording only
#' the single most preferred alternative, some surveys ask for a full ranking of
#' all the alternatives, which reveals far more about the underlying
#' preferences. Ranked choices can by analyzed by setting \code{ranked = TRUE}.
#' The choice column of the data set must provide the full ranking for each
#' choice occasion (from most preferred to least preferred), where the
#' alternatives are separated by the \code{delimiter} string.
#'
#' The ranked probit model follows directly from the general unordered case
#' noting that the ranking implies that the highest ranked alternative is chosen
#' in any case, while the second highest ranked alternative is chosen, if the
#' highest ranked alternative is not available and so forth. The only difference
#' is that we take flexible utility differences such that the differenced
#' utility vector is always negative, in contrast to the general case where we
#' difference with respect to a fixed reference alternative. Thereby, we
#' incorporate information of the full ranking.

probit_choice_set <- function(
    probit_alternatives, ranked = FALSE, delimiter = "_"
  ) {
  checkmate::assert_class(probit_alternatives, "probit_alternatives")
  checkmate::assert_flag(ranked)
  checkmate::assert_string(delimiter, n.chars = 1)
  choice_set <- if (ranked) {
    sapply(
      oeli::permutations(probit_alternatives$alternatives),
      paste,
      collapse = delimiter
    )
  } else {
    probit_alternatives$alternatives
  }
  structure(
    choice_set,
    alternatives = probit_alternatives,
    ranked = ranked,
    class = c("probit_choice_set", "list")
  )
}

#' @rdname probit_choice_set
#' @param x
#' A \code{\link{probit_choice_set}} object.

is.probit_choice_set <- function(x) {
  inherits(x, "probit_choice_set")
}

#' @rdname probit_choice_set
#' @exportS3Method
#' @param ...
#' Currently not used.

print.probit_choice_set <- function(x, ...) {
  checkmate::assert_class(x, "probit_choice_set")
  cat("Choice set:", x, "\n")
}

