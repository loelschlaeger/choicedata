#' Define choices
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choices}}, which defines the observed choices.
#'
#' @return
#' A \code{\link{choices}} object.

choices <- function(

) {

}

#' @rdname choices

simulate_choices <- function(
    choice_parameters, choice_covariates, choice_set, seed = NULL,
    column_choice = "choice", column_decider = "id", column_occasion = "idc"
) {

  ### input checks
  is.choice_parameters(choice_parameters)
  is.choice_covariates(choice_covariates)
  is.choice_set(choice_set)

  ### extract information from objects
  Tp <- attr(choice_covariates, "Tp")
  N <- length(Tp)
  ranked <- attr(choice_set, "ranked")

  ### simulate choices
  if (!is.null(seed)) {
    set.seed(seed)
  }
  choices <- lapply(seq_len(N), function(n) {
    coef <- get_coefficient_vector(
      choice_parameters = choice_parameters, decider_id = n
    )
    lapply(seq_len(Tp[n]), function(t) {
      X_nt <- choice_covariates[[n]][[t]]
      U_nt <- oeli::rmvnorm(
        mean = as.vector(X_nt %*% coef),
        Sigma = choice_parameters$Sigma
      )
      if (ranked) {

      } else {
        choice_set[which.max(U_nt)]
      }
    })
  })

  ### create and return 'choices' object
  choices(
    choices = choices,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    ranked = FALSE, # TODO
    ordered = FALSE # TODO
  )
}

#' @rdname choices
#' @exportS3Method

as.data.frame.choices <- function() {

}

#' @rdname choices
#' @exportS3Method

as.list.choices <- function() {

}
