#' Documentation helper
#'
#' @name doc-helper
#'
#' @param choice_alternatives
#' A \code{\link{choice_alternatives}} object.
#' @param choice_formula
#' A \code{\link{choice_formula}} object.
#' @param error
#' Either \code{TRUE} to throw an error if the check failed, or \code{FALSE}
#' to return the check result as \code{TRUE} or \code{FALSE}, respectively.
#' @param x
#' An object of the corresponding class to be printed.
#' @param ...
#' Currently not used.
#'
#' @keywords internal

NULL

check_missing <- function(x, error = TRUE) {
  check <- missing(x)
  if (isTRUE(error) && isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort("Please specify the input {.var {var_name}}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_delimiter <- function(delimiter, error = TRUE) {
  check <- checkmate::check_string(delimiter, n.chars = 1)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var delimiter} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_formula <- function(formula, error = TRUE) {
  check <- checkmate::check_formula(formula)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var formula} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_re <- function(re, error = TRUE) {
  check <- checkmate::check_character(
    re, any.missing = FALSE, unique = TRUE, null.ok = TRUE
  )
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var re} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_ordered <- function(ordered, error = TRUE) {
  check <- checkmate::check_flag(ordered)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var ordered} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_J <- function(J, ordered, error = TRUE) {
  check_ordered(ordered, error = error)
  check <- checkmate::check_int(J, lower = 2 + ordered)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var J} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_alternatives <- function(alternatives, J, ordered, error = TRUE) {
  check_J(J = J, ordered = ordered, error = error)
  check <- checkmate::check_character(
    alternatives, any.missing = FALSE, len = J, unique = TRUE
  )
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var alternatives} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_base <- function(base, alternatives, J, ordered, error = TRUE) {
  check_alternatives(
    alternatives = alternatives, J = J, ordered = ordered, error = error
  )
  check <- checkmate::check_choice(base, choices = alternatives)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var base} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}


check_model_consistent <- function(

) {

}

check_model_ordered <- function() {

}

check_model_ranked <- function() {

}

