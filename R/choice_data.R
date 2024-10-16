#' Define choice data
#'
#' @description
#' These functions construct and validate an object of class
#' \code{\link{choice_data}}, which defines the choice model data.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice data.
#'
#' @param format
#' TODO
#'
#' @param column_choice
#' A \code{character}, the column name of the \code{data.frame} with the
#' choices.
#' @param column_alternative
#' TODO
#' @param column_decider
#' A \code{character}, the column name of the \code{data.frame} with the
#' decider identifiers.
#' @param column_occasion
#' A \code{character}, the column name of the \code{data.frame} with the
#' identifiers for the choice occasions.
#' @param column_covariates_alternative_constant
#' TODO
#' @param column_covariates_alternative_varying
#' TODO
#' @param ranked
#' TODO
#' @param delimiter
#' TODO
#' @param error
#' TODO
#' @param choice_formula
#' TODO
#' @inheritParams choice_alternatives
#' @inheritParams choice_response
#'
#' @return
#' A \code{\link{choice_data}} object.
#'
#' @export

choice_data <- function(
  data,
  format = "wide",
  column_choice = "choice",                             # must be specified
  column_alternative = NA,                              # only in long case, can be NA
  column_decider = "deciderID",                         # must be specified
  column_occasion = NA,                                 # can be NA
  column_covariates_alternative_constant = character(), # only in long case, must be specified
  column_covariates_alternative_varying = character(),  # only in long case, must be specified
  alternatives = character(),                           # must be specified
  ordered = FALSE,
  ranked = FALSE,
  delimiter = "_"
) {

  ### input checks
  data <- check_data(data, force_data_frame = TRUE)
  check_format(format)
  check_column_choice(column_choice, null.ok = FALSE)
  check_column_alternatives(column_alternative, na.ok = TRUE)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, null.ok = FALSE)
  check_column_covariates_alternative_constant(column_covariates_alternative_constant)
  check_column_covariates_alternative_varying(column_covariates_alternative_varying)
  check_alternatives(alternatives, J = length(alternatives))
  check_ranked(ranked)
  check_delimiter(delimiter)

  ### identify columns
  columns <- colnames(data)
  stopifnot(column_choice %in% columns)
  columns <- setdiff(columns, column_choice)
  stopifnot(column_decider %in% columns)
  columns <- setdiff(columns, column_decider)
  if (!checkmate::test_scalar_na(column_alternative)) {
    stopifnot(column_alternative %in% columns)
    columns <- setdiff(columns, column_alternative)
  }
  if (!checkmate::test_scalar_na(column_occasion)) {
    stopifnot(column_occasion %in% columns)
    columns <- setdiff(columns, column_occasion)
  }
  if (length(column_covariates_alternative_constant) > 0) {
    stopifnot(column_covariates_alternative_constant %in% columns)
    columns <- setdiff(columns, column_covariates_alternative_constant)
  }
  if (length(column_covariates_alternative_varying) > 0) {
    if (format == "long") {
      stopifnot(column_covariates_alternative_varying %in% columns)
      columns <- setdiff(columns, column_covariates_alternative_varying)
    } else {
      warning("check not implemented yet")
    }
  }
  if (length(columns) > 0) {
    if (format == "long") {
      cli::cli_alert_info("Dropping column{?s} {cli::cli_vec(columns, list('vec-trunc' = 3))}")
      data <- data[, !names(data) %in% columns, drop = FALSE]
    } else {
      warning("check not implemented yet")
    }
  }

  ### build and return object
  structure(
    data,
    class = unique(c(
      "choice_data",
      switch(format, wide = "choice_data_wide", long = "choice_data_long"),
      "data.frame",
      class(data)
    )),
    column_choice = column_choice,
    column_alternative = column_alternative,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_covariates_alternative_constant = column_covariates_alternative_constant,
    column_covariates_alternative_varying = column_covariates_alternative_varying,
    alternatives = alternatives,
    ordered = ordered,
    ranked = ranked,
    delimiter = delimiter,
    validated = FALSE
  )
}

#' @noRd

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
#'
#' @description
#' \code{\link{simulate_choice_data}} simulates choice data.
#'
#' @param choice_covariates
#' A \code{\link{choice_covariates}} object.
#'
#' @param choice_covariates \[`choice_covariates`\]\cr
#' The choice covariates used for the choice data simulation.
#'
#' @param choice_parameters
#' An \code{\link{choice_parameters}} object, which contains the model
#' parameters used for the choice data simulation.
#'
#' @examples
#' # ### simulate data from a binary probit model with two latent classes
#' # data <- generate_choice_data(
#' #   choice_covariates = generate_choice_covariates(
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

#' @rdname choice_data

change_format_old <- function(
    x, row.names = NULL, optional = FALSE, format = "wide", ...
  ) {

  ### do nothing if already in long format
  if (format == "long") {
    cli::cli_alert_warning("Already in long format")
    return(x)
  }

  ### extract needed information from object
  alternatives <- attr(x, "alternatives")
  delimiter <- attr(x, "delimiter")
  column_alternative <- attr(x, "column_alternative")
  column_choice <- attr(x, "column_choice")
  column_decider <- attr(x, "column_decider")
  column_occasion <- attr(x, "column_occasion")
  column_covariates_alternative_constant <- attr(x, "column_covariates_alternative_constant")
  column_covariates_alternative_varying <- attr(x, "column_covariates_alternative_varying")
  ordered <- attr(x, "ordered")
  ranked <- attr(x, "ranked")

  ### need to strip of 'choice_data' class
  class(x) <- "data.frame"

  ### if no column with alternative available, add auxiliary one
  if (checkmate::test_scalar_na(column_alternative)) {
    column_alternative <- ".tmp.column_alternative"
    x <- x |>
      dplyr::group_by(!!rlang::sym(column_decider), !!rlang::sym(column_occasion)) |>
      dplyr::mutate(!!column_alternative := alternatives)
  }

  ### mutate binary choice to alternative label
  x <- x |> mutate(!!column_choice := as.factor(ifelse(
    !!rlang::sym(column_choice) == 1, !!rlang::sym(column_alternative), NA
  )))

  ### transform to wide format
  x <- x |>
    tidyr::pivot_wider(
      names_from = !!column_alternative,
      id_cols = c(!!column_decider, !!column_occasion),
      values_from = !!column_covariates_alternative_varying,
      unused_fn = structure(
        list(function(y) y[!is.na(y)]),
        names = column_choice
      ),
      names_glue = paste0("{.value}", delimiter, "{", column_alternative, "}")
    )

  ### TODO: sort columns
  # x <- x |> dplyr::select(!!column_decider, !!column_occasion, !!column_choice, sort(colnames(.)))

  ### return new object
  choice_data(
    data = x,
    format = "wide",
    column_choice = column_choice,
    column_alternative = NA,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_covariates_alternative_constant = column_covariates_alternative_constant,
    column_covariates_alternative_varying = column_covariates_alternative_varying,
    alternatives = alternatives,
    ordered = ordered,
    ranked = ranked,
    delimiter = delimiter
  )
}

#' @rdname choice_data
#' @exportS3Method

plot.choice_data <- function(x, ...) {
  plot(1)
}

#' @rdname choice_data
#' @exportS3Method

print.choice_data <- function(x, ...) {
  print(x)
}
