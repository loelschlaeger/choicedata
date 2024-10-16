#' Define choice response
#'
#' @description
#' These functions construct, simulate, and transform an object of class
#' `choice_response`, which defines the observed discrete choices.
#'
#' - `choice_response()` constructs a `choice_response` object.
#' - `simulate_choice_response()` simulates choices.
#' - `change_format.choice_parameters()` transforms between `list` and
#'   `data.frame` format, see details.
#'
#' @param choices
#' TODO
#'
#' @inheritParams choice_data
#'
#' @param choice_identifiers
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

choice_response <- function(
    choices,
    choice_identifiers,
    column_choice = "choice"
  ) {

  ### input checks
  checkmate::assert_list(choices)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  column_choice <- check_column_choice(column_choice)

  ### change format to 'data.frame'
  choices <- change_format.choice_response(
    x = choices,
    new_format = "data_frame",
    choice_identifiers = choice_identifiers,
    column_choice = column_choice
  )

  ### build 'choices' object
  structure(
    choices,
    "choice_identifiers" = choice_identifiers,
    "column_choice" = column_choice,
    class = c("choice_response", "list")
  )
}

#' @rdname choice_response
#'
#' @export

simulate_choice_response <- function(
  choice_effects,
  choice_covariates,
  choice_parameters,
  choice_preferences,
  column_choice = "choice"
) {

  ### input checks
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_covariates(choice_covariates, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  check_column_choice(column_choice = "choice", null.ok = FALSE)

  ### extract objects
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  alt <- as.character(choice_alternatives)
  choice_formula <- attr(choice_effects, "choice_formula")

  ### extract choice identifiers
  choice_identifiers <- choice_identifiers(
    data_frame = choice_covariates[, 1:2],
    column_decider = "deciderID",
    column_occasion = "occasionID"
  )
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)

  ### transform choice covariates to design matrices TODO
  design_matrices <- df_to_design_matrices(
    choice_covariates = choice_covariates,
    choice_effects = choice_effects
  )

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
        alt[which.max(U_nt)]
      },
      character(1)
    )
  })

  ### create and return 'choices' object
  choice_response(
    choices = choices,
    choice_identifiers = choice_identifiers,
    column_choice = column_choice
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

#' @export

change_format.choice_response <- function(
    x,
    new_format = NULL,
    choice_identifiers = NULL,
    column_choice = NULL,
    ...
) {

  x

}

