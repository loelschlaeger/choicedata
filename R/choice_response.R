#' Define choice response
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choice_response}}, which defines the observed choices.
#'
#' @param choices
#' A \code{list}, where the \code{n}-th element is a \code{vector} of choices
#' for the \code{n}-th decider, where the \code{t}-th element is a
#' \code{character} that defines the choice at their \code{t}-th occasion.
#'
#' @inheritParams choice_data
#'
#' @param choice_identifiers
#' TODO
#' @param ranked
#' TODO
#' @param ordered
#' TODO
#' @param delimiter
#' TODO
#' @param choice_formula
#' TODO
#' @param choice_preferences
#' TODO
#' @param x
#' TODO
#' @param error
#' TODO
#' @param ...
#' TODO
#'
#' @return
#' A \code{\link{choice_response}} object. It can be a \code{data.frame} or
#' \code{list} object and can be transformed between these two formats via
#' \code{as.data.frame.choices()} or \code{as.list.choices()}:
#' \itemize{
#'   \item \code{data.frame} format: a \code{data.frame} with three columns
#'         named, \code{column_decider} (the decider identifiers),
#'         \code{column_occasion} (the identifiers for the choice occasions),
#'         and \code{column_choice} (the choices)
#'   \item \code{list} format: a \code{list}, where the \code{n}-th element is
#'         a \code{vector} of choices for the \code{n}-th decider, where the
#'         \code{t}-th element is the choice at their \code{t}-th occasion
#' }
#' The \code{\link{choice_response}} object has the following attributes:
#' \itemize{
#'   \item TODO
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

choice_response <- function(
    choices,
    choice_identifiers,
    ranked = FALSE,
    ordered = FALSE,
    column_choice = "choice",
    delimiter = "_"
  ) {

  ### input checks
  checkmate::assert_list(choices)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  ranked <- check_ranked(ranked)
  ordered <- check_ordered(ordered)
  column_choice <- check_column_choice(column_choice)
  delimiter <- check_delimiter(delimiter)

  ### build 'choices' object
  structure(
    choices,
    "choice_identifiers" = choice_identifiers,
    "ranked" = ranked,
    "ordered" = ordered,
    "column_choice" = column_choice,
    "delimiter" = delimiter,
    class = c("choice_response", "list")
  )
}

#' @description
#' \code{\link{simulate_choices}} simulates choices based on
#' \code{\link{choice_parameters}} and \code{\link{choice_covariates}}:
#' \itemize{
#'   \item If \code{\link{choice_formula}} is not specified, it is taken from
#'         \code{\link{choice_covariates}} (if available).
#'   \item If \code{\link{choice_preferences}} is not specified, it is sampled
#'         from \code{\link{choice_parameters}} and the implied
#'         \code{\link{choice_effects}}.
#' }
#'
#'
#' @rdname choice_response

simulate_choice_response <- function(
  choice_effects,
  choice_covariates,
  choice_parameters,
  choice_preferences,
  ranked = FALSE,
  column_choice = "choice",
  delimiter = "_"
) {

  ### input checks
  is.choice_parameters(choice_parameters)
  is.choice_covariates(choice_covariates)
  design_matrices <- design_matrices(
    choice_covariates, choice_effects
  )
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  ordered <- attr(choice_alternatives, "ordered")
  alt <- as.character(choice_alternatives)
  choice_identifiers <- attr(design_matrices, "choice_identifiers")
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)
  choice_formula <- attr(choice_effects, "choice_formula")

  ### simulate choices
  choices <- lapply(seq_len(N), function(n) {
    # TODO: -(1:2)
    coef_n <- as.numeric(choice_preferences[n, -(1:2)])
    vapply(
      seq_len(Tp[n]),
      function(t) {
        X_nt <- design_matrices[[n]][[t]]
        U_nt <- oeli::rmvnorm(
          mean = as.vector(X_nt %*% coef_n),
          Sigma = choice_parameters$Sigma
        )
        if (ranked) {
          # TODO
        } else if (ordered) {
          # TODO
        } else {
          alt[which.max(U_nt)]
        }
      },
      character(1)
    )
  })

  ### create and return 'choices' object
  choice_response(
    choices = choices,
    choice_identifiers = choice_identifiers,
    ranked = ranked,
    ordered = ordered,
    column_choice = column_choice,
    delimiter = delimiter
  )
}

#' @rdname choice_response
#' @export

is.choice_response <- function(x, error = TRUE) {
  check_not_missing(x)
  check <- inherits(x, "choice_response")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_response}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_response
#' @export

validate_choice_response <- function() {
  # TODO
}

#' @rdname choice_response
#' @exportS3Method

as.data.frame.choice_response <- function(x, ...) {
  is.choice_response(x, error = TRUE)
  if (checkmate::test_data_frame(x)) {
    return(x)
  }
  attributes <- attributes(x)
  structure(
    structure(
      cbind(attributes$choice_identifiers, unlist(x)),
      "names" = c(names(attributes$choice_identifiers), attributes$column_choice),
      "class" = "data.frame"
    ),
    ranked = attributes$ranked,
    ordered = attributes$ordered,
    delimiter = attributes$delimiter,
    class = c("choices", "data.frame")
  )
}

#' @rdname choice_response
#' @exportS3Method

as.list.choice_response <- function(x, ...) {
  is.choice_response(x, error = TRUE)
  if (checkmate::test_list(x)) {
    return(x)
  }
}
