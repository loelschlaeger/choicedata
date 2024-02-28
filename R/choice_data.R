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
#' A \code{\link{choice_data}} object.

choice_data <- function(
  data = Train,
  format = "wide",
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = "occasionID",
  column_covariates = NULL,
  alternatives = NULL,
  ordered = FALSE,
  ranked = FALSE,
  delimiter = "_"
) {

  ### input checks
  check_data(data)
  check_format(format)
  check_column_choice(column_choice)
  check_column_decider(column_decider)
  check_column_occasion(column_occasion)
  check_column_covariates(column_covariates)
  check_alternatives(alternatives)
  check_ordered(ordered)
  check_ranked(ranked)
  check_delimiter(delimiter)

  ### build and return object
  structure(
    data,
    class = c(
      "choice_data",
      switch(format, wide = "choice_data_wide", long = "choice_data_long"),
      "data.frame"
    ),
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_covariates = column_covariates,
    alternatives = alternatives,
    ordered = ordered,
    ranked = ranked,
    delimiter = delimiter,
    validated = FALSE
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

validate_choice_data_long <- function() {

}

validate_choice_data_wide <- function() {

}

#' @rdname choice_data
#'
#' @description
#' \code{\link{simulate_choice_data}} simulates choice data.
#'
#' @param choice_covariates
#' A \code{\link{choice_covariates}} object.
#' @param choice_covariates
#' An \code{\link{choice_covariates}} object, which contains the covariate
#' matrices used for the choice data simulation.
#' @param choice_parameters
#' An \code{\link{choice_parameters}} object, which contains the model
#' parameters used for the choice data simulation.
#' By default, \code{choice_parameters = choice_parameters()}, i.e. default
#' parameters are used.
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
    choice_covariates,
    choice_parameters,
    choice_formula = NULL,
    format = "wide",
    ranked = FALSE,
    column_choice = "choice"
  ) {

  ### input checks
  is.choice_covariates(choice_covariates, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  if (is.null(choice_formula)) {
    choice_formula <- attr(choice_covariates, "choice_formula")
    if (getOption("verbose", default = TRUE)) {
      cli::cli_alert_info(
        "Retrieving {.cls choice_formula} from {.cls choice_covariates}"
      )
    }
  }
  is.choice_formula(choice_formula, error = TRUE)
  Tp <- attr(choice_covariates, "Tp")
  N <- length(Tp)
  choice_alternatives <- attr(choice_covariates, "choice_alternatives")
  is.choice_parameters(choice_parameters, error = TRUE)
  choice_parameters <- validate_choice_parameters(
    choice_parameters = choice_parameters,
    choice_formula = choice_formula,
    J = attr(choice_alternatives, "J"),
    allow_missing = FALSE
  )
  column_choice <- check_column_choice(column_choice)
  column_decider <- attr(choice_covariates, "column_decider")
  column_occasion <- attr(choice_covariates, "column_occasion")

  ### simulate choices
  choices <- simulate_choices(
    choice_parameters = choice_parameters,
    choice_covariates = choice_covariates,
    choice_preferences = NULL
  )

  ### merge choices and covariates
  data <- merge(
    as.data.frame(choices), choice_covariates,
    by = c(column_decider, column_occasion),
    sort = FALSE
  )

  return(data)
  ### create and return 'choice_data' object
  choice_data(
    data = data,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    ranked = ranked,
    ordered = ordered
  )
}



as.list.choice_data <- function() {

}

as.list_choice_data_long <- function() {

}

as.list_choice_data_wide <- function() {

}

as.data.frame.choice_data <- function(format) {

}

as.data.frame.choice_data_long <- function(format) {

}

as.data.frame.choice_data_wide <- function(format) {

}

plot.choice_data <- function() {

}

plot.choice_data_long <- function() {

}

plot.choice_data_wide <- function() {

}



