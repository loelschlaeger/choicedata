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

get_coefficient_vector <- function(choice_parameters, decider_id) {
  checkmate::assert_class(choice_parameters, "choice_parameters")
  checkmate::assert_int(
    decider_id, lower = 1, upper = length(choice_parameters$z)
  )
  z_n <- choice_parameters$z[decider_id]
  checkmate::assert_int(z_n, lower = 1)
  coef <- numeric()
  if (checkmate::test_numeric(choice_parameters$alpha, any.missing = FALSE)) {
    coef <- c(coef, choice_parameters$alpha[, z_n])
  }
  if (checkmate::test_numeric(choice_parameters$beta, any.missing = FALSE)) {
    coef <- c(coef, choice_parameters$beta[, z_n])
  }
  return(coef)
}


























