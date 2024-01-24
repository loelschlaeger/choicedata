#' Define choices
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choices}}, which defines the observed choices.
#'
#' @param choices
#' A \code{list}, where the \code{n}-th element is a \code{list} of choices for
#' the \code{n}-th decider, where the \code{t}-th element is an element from
#' \code{choice_set} that defines the choice at their \code{t}-th choice
#' occasion.
#' @param choice_parameters
#' A \code{\link{choice_parameters}} object.
#' @param choice_covariates
#' A \code{\link{choice_covariates}} object.
#' @param choice_set
#' A \code{\link{choice_set}} object.
#' @param ranked
#' Either \code{TRUE} for ranked choices or \code{FALSE} (default), else.
#' @inheritParams choice_data
#'
#' @return
#' A \code{\link{choices}} object. It can be in \code{data.frame} or \code{list}
#' and can be transformed between these two formats via the functions
#' \code{as.data.frame.choices()} or \code{as.list.choices()}.
#' \itemize{
#'   \item \code{data.frame} format: a \code{data.frame} with three columns
#'         named \code{column_choice} (the choices), \code{column_decider}
#'         (the decider identifiers), and \code{column_occasion} (the
#'         identifiers for the choice occasions)
#'   \item \code{list} format: a \code{list}, where the \code{n}-th element is
#'         a \code{list} of choices for the \code{n}-th decider, where the
#'         \code{t}-th element is an element from \code{choice_set} that defines
#'         the choice at their \code{t}-th choice occasion
#' }
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

choices <- function(
    choices = list(), column_choice = column_choice,
    column_decider = "deciderID", column_occasion = "occasionID",
    ranked = FALSE, ordered = FALSE, delimiter = "_"
) {
  oeli::assert_list_of_lists(choices)
  structure(
    choices,
    "Tp" = vapply(choices, length, integer(1)),
    "choice_formula" = choice_formula,
    "column_choice" = column_choice,
    "column_decider" = column_decider,
    "column_occasion" = column_occasion,
    "ranked" = ranked,
    "ordered" = ordered,
    "delimiter" = delimiter,
    class = "choices"
  )
}

#' @rdname choices

simulate_choices <- function(
  choice_parameters, choice_covariates, choice_set,
  column_choice = "choice", column_decider = "deciderID",
  column_occasion = "occasionID"
) {

  ### input checks
  is.choice_parameters(choice_parameters)
  is.choice_covariates(choice_covariates)
  is.choice_set(choice_set)

  ### transform choice covariates to list form
  choice_covariates <- as.list(choice_covariates)

  ### extract information from objects
  Tp <- attr(choice_covariates, "Tp")
  N <- length(Tp)
  ordered <- attr(choice_set, "ordered")
  ranked <- attr(choice_set, "ranked")

  ### simulate choices
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
        # TODO
      } else if (ordered) {
        # TODO
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
    ranked = FALSE,
    ordered = FALSE
  )
}

#' @rdname choices
#' @param x
#' An \code{\link{choices}} object.
#' @export

is.choices <- function(x) {
  inherits(x, "choices")
}

#' @rdname choices
#' @export

validate_choices <- function() {

}

#' @rdname choices
#' @exportS3Method

as.data.frame.choices <- function(x, ...) {
  stopifnot(is.choices(x))
  if (is.data.frame(x)) {
    return(x)
  }
}

#' @rdname choices
#' @exportS3Method

as.list.choices <- function(x, ...) {
  stopifnot(is.choices(x))
  if (is.list(x)) {
    return(x)
  }
}
