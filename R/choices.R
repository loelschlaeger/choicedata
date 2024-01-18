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
#' @param seed
#' An \code{integer}, passed to \code{set.seed()} to make the simulation of
#' choices reproducible.
#' By default, \code{seed = NULL}, i.e., no seed is set.
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

choices <- function(
    choices = list(), column_choice = column_choice,
    column_decider = "id", column_occasion = "idc",
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
  choice_parameters, choice_covariates, choice_set, seed = NULL,
  column_choice = "choice", column_decider = "id", column_occasion = "idc"
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

is.choice_formula <- function(x) {
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
