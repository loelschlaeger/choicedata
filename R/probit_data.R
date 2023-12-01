#' Define probit data
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{probit_data}}, which defines the probit model data.
#'
#' @param data
#' A \code{data.frame}.
#' @param column_choice
#' TODO
#' @param column_decider
#' TODO
#' @param column_occasion
#' TODO
#' @inheritParams probit_alternatives
#' @inheritParams probit_choice_set
#'
#' @return
#' TODO

probit_data <- function(
  probit_covariates, probit_choices,
  column_choice = "choice", column_decider = "id", column_occasion = "idc",
  ranked = FALSE, ordered = FALSE
) {

  ### merge choices and covariates




  ### validate list format

  ### transform to data.frame format

  structure(
    "simulated" = FALSE
  )

}

#' @rdname probit_data

validate_probit_data <- function() {

}

#' Simulate choice data
#'
#' @description
#' This function simulates choice data from a probit model. It helps to create
#' a \code{\link{probit_data}} object.
#'
#' @param probit_covariates
#' An \code{\link{probit_covariates}} object, which contains the covariate
#' matrices used for the choice data simulation.
#' @param probit_parameter
#' An \code{\link{probit_parameter}} object, which contains the model
#' parameters used for the choice data simulation.
#' By default, \code{probit_parameter = probit_parameter()}, i.e. default
#' parameters are used.
#' @inheritParams probit_data
#' @inheritParams probit_covariates
#'
#' @return
#' An \code{\link{probit_data}} object.
#'
#' @inheritSection probit_formula Model formula
#' @inheritSection probit_formula Random effects
#'
#' @examples
#' ### simulate data from a binary probit model with two latent classes
#' data <- simulate_probit_data(
#'   probit_covariates = sample_probit_covariates(
#'     probit_formula = probit_formula(
#'       formula = choice ~ cost | income | time, re = c("cost", "time")
#'     ),
#'     N = 10, Tp = 1:10,
#'     probit_alternatives = probit_alternatives(
#'       J = 2, alternatives = c("car", "bus")
#'     )
#'   )
#' )
#'
#' ### simulate data from an ordered probit model
#' # data <- simulate_probit_data(
#' #   formula = opinion ~ age + gender, N = 50, J = 5,
#' #   alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
#' #   ordered = TRUE
#' # )
#'
#' ### simulate data from a ranked probit model
#' # data <- simulate_probit_data(
#' #   formula = product ~ price, N = 10, J = 3, T = 1:10, ranked = TRUE
#' # )
#'
#' @export
#'
#' @seealso
#' \itemize{
#'   \item TODO
#' }

simulate_probit_data <- function(
    probit_covariates = sample_probit_covariates(
      probit_formula, N, Tp = 1,
      probit_alternatives = probit_alternatives(J = 3)
    ),
    probit_parameter = probit_parameter(), ranked = FALSE, seed = NULL,
    column_choice = "choice"
) {

  ### input checks
  checkmate::assert_class(probit_covariates, "probit_covariates")
  Tp <- attr(probit_covariates, "Tp")
  N <- length(Tp)
  probit_alternatives <- attr(probit_covariates, "probit_alternatives")
  probit_formula <- attr(probit_covariates, "probit_formula")
  checkmate::assert_class(probit_parameter, "probit_parameter")
  probit_parameter <- validate_probit_parameter(
    x = probit_parameter, formula = probit_formula$formula,
    re = probit_formula$re, ordered = probit_alternatives$ordered,
    J = probit_alternatives$J, N = N
  )
  probit_choice_set <- probit_choice_set(
    probit_alternatives = probit_alternatives, ranked = ranked
  )
  checkmate::assert_string(column_choice, min.chars = 1)

  ### simulate choices
  probit_choices <- simulate_probit_choices(
    probit_parameter = probit_parameter,
    probit_covariates = probit_covariates,
    probit_choice_set = probit_choice_set,
    seed = seed
  )

  ### create and return 'probit_data' object
  probit_data(
    probit_covariates = probit_covariates,
    probit_choices = probit_choices,
    column_choice = column_choice,
    column_decider = "id",
    column_occasion = "idc",
    ranked = ranked,
    ordered = FALSE
  )
}

read_probit_data <- function() {

}


as.list.probit_data <- function() {

}

as.data.frame.probit_data <- function() {

}



