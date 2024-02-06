#' Define choice data
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choice_data}}, which defines the choice model data.
#'
#' @param data
#' A \code{data.frame}.
#' @param column_choice
#' A \code{character}, the column name of the \code{data.frame} with the
#' choices.
#' @param column_decider
#' A \code{character}, the column name of the \code{data.frame} with the
#' decider identifiers.
#' @param column_occasion
#' A \code{character}, the column name of the \code{data.frame} with the
#' identifiers for the choice occasions.
#' @inheritParams choice_alternatives
#' @inheritParams choices
#'
#' @return
#' TODO

choice_data <- function(
  data, column_choice = "choice", column_decider = "id",
  column_occasion = "idc", ranked = FALSE, ordered = FALSE
) {

  ### merge choices and covariates




  ### validate list format

  ### transform to data.frame format

  structure(
    "simulated" = FALSE
  )

}

#' @rdname choice_data
#' @export

is.choice_data <- function(x, error = TRUE) {
  check_not_missing(x)
  check <- inherits(x, "choice_data")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_data}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_data

validate_choice_data <- function() {

}

#' Simulate choice data
#'
#' @description
#' This function simulates choice data and creates a
#' \code{\link{choice_data}} object.
#'
#' @param choice_covariates
#' A \code{\link{choice_covariates}} object.
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
#' @param choice_covariates
#' An \code{\link{choice_covariates}} object, which contains the covariate
#' matrices used for the choice data simulation.
#' @param choice_parameters
#' An \code{\link{choice_parameters}} object, which contains the model
#' parameters used for the choice data simulation.
#' By default, \code{choice_parameters = choice_parameters()}, i.e. default
#' parameters are used.
#'
#' @return
#' An \code{\link{choice_data}} object.
#'
#' @inheritSection choice_formula Model formula
#' @inheritSection choice_formula Random effects
#'
#' @examples
#' # ### simulate data from a binary probit model with two latent classes
#' # data <- simulate_choice_data(
#' #   choice_covariates = sample_choice_covariates(
#' #     choice_formula = choice_formula(
#' #       formula = choice ~ cost | income | time, re = c("cost", "time")
#' #     ),
#' #     N = 10, Tp = 1:10,
#' #     choice_alternatives = choice_alternatives(
#' #       J = 2, alternatives = c("car", "bus")
#' #     )
#' #   )
#' # )
#'
#' ### simulate data from an ordered probit model
#' # data <- simulate_choice_data(
#' #   formula = opinion ~ age + gender, N = 50, J = 5,
#' #   alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
#' #   ordered = TRUE
#' # )
#'
#' ### simulate data from a ranked probit model
#' # data <- simulate_choice_data(
#' #   formula = product ~ price, N = 10, J = 3, T = 1:10, ranked = TRUE
#' # )
#'
#' @export
#'
#' @seealso
#' \itemize{
#'   \item TODO
#' }

simulate_choice_data <- function(
    choice_covariates = choice_covariates(),
    choice_parameters = choice_parameters(),
    choice_formula = NULL,
    ranked = FALSE,
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = "occasionID"
  ) {

  ### input checks
  is.choice_covariates(choice_covariates, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  if (is.null(choice_formula)) {
    choice_formula <- attr(choice_covariates, "choice_formula")
  }
  is.choice_formula(choice_formula, error = TRUE)
  Tp <- attr(choice_covariates, "Tp")
  N <- length(Tp)
  choice_alternatives <- attr(choice_covariates, "choice_alternatives")
  is.choice_parameters(choice_parameters, error = TRUE)
  choice_parameters <- validate_choice_parameters(
    choice_parameters = choice_parameters,
    choice_formula = choice_formula,
    J = choice_alternatives$J,
    allow_missing = FALSE
  )
  check_column_choice(column_choice)

  ### simulate choices
  choices <- simulate_choices(
    choice_parameters = choice_parameters,
    choice_covariates = choice_covariates,
    choice_preferences = choice_preferences
  )

  ### create and return 'choice_data' object
  choice_data(
    choice_covariates = choice_covariates,
    choices = choices,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    ranked = ranked,
    ordered = FALSE
  )
}

read_choice_data <- function() {

}


as.list.choice_data <- function() {

}

as.data.frame.choice_data <- function() {

}

plot.choice_data <- function() {


}




