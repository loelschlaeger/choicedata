#' Define probit choices
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{probit_choices}}, which defines the observed choices.
#'
#' @param choices
#' The observed choices in one of two possible formats:
#' - Can be a \code{data.frame} with three columns, named according to
#'   \code{column_choice}, \code{column_decider}, and \code{column_occasion},
#'   where the \code{column_choice} column contains the chosen alternative
#'   of the decider in the \code{column_decider} column at the choice occasion
#'   in \code{column_occasion}.
#' - Alternatively, it can be a \code{list}, where the \code{n}th element is a
#'   \code{list} that has the choice of decider \code{n} at choice occasion
#'   \code{t} as the \code{t}th element.
#'
#' @return
#' A \code{\link{probit_formula}} object.

probit_choices <- function(
  choices, column_choice = "choice", column_decider = "id",
  column_occasion = "idc", ranked = FALSE, ordered = FALSE
) {

}

#' @rdname probit_choices

simulate_probit_choices <- function(
  probit_parameters, probit_covariates, probit_choice_set, seed = NULL,
  column_choice = "choice", column_decider = "id", column_occasion = "idc"
) {

  ### input checks
  is.probit_parameters(probit_parameters)
  is.probit_covariates(probit_covariates)
  is.probit_choice_set(probit_choice_set)

  ### extract information from objects
  Tp <- attr(probit_covariates, "Tp")
  N <- length(Tp)
  ranked <- attr(probit_choice_set, "ranked")

  ### simulate choices
  if (!is.null(seed)) {
    set.seed(seed)
  }
  choices <- lapply(seq_len(N), function(n) {
    coef <- get_coefficient_vector(
      probit_parameters = probit_parameters, decider_id = n
    )
    lapply(seq_len(Tp[n]), function(t) {
      X_nt <- probit_covariates[[n]][[t]]
      U_nt <- oeli::rmvnorm(
        mean = as.vector(X_nt %*% coef),
        Sigma = probit_parameters$Sigma
      )
      if (ranked) {

      } else {
        probit_choice_set[which.max(U_nt)]
      }
    })
  })

  ### create and return 'probit_choices' object
  probit_choices(
    choices = choices,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    ranked = FALSE, # TODO
    ordered = FALSE # TODO
  )
}

#' @rdname probit_choices
#' @exportS3Method

as.data.frame.probit_choices <- function() {

}

#' @rdname probit_choices
#' @exportS3Method

as.list.probit_choices <- function() {

}
