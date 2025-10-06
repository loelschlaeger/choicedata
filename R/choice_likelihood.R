#' Define and compute choice likelihood
#'
#' @description
#' These functions prepare and evaluate the likelihood contribution of observed
#' choices for a given choice model.
#'
#' - `choice_likelihood()` pre-computes the design matrices and choice indices
#'   implied by `choice_data` and `choice_effects`. The returned object stores
#'   these quantities so that repeated likelihood evaluations during maximum
#'   likelihood estimation avoid redundant work.
#' - `compute_choice_likelihood()` evaluates the (log-)likelihood for given
#'   `choice_parameters`. It can either take the original choice objects or a
#'   pre-computed `choice_likelihood` object.
#'
#' @param choice_data \[`choice_data`\]\cr
#' A \code{\link{choice_data}} object with the observed choices.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object that determines the model effects.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object. The default extracts identifiers
#' from `choice_data`.
#'
#' @param choice_parameters \[`choice_parameters` | `list`\]\cr
#' A \code{\link{choice_parameters}} object or a list as returned by
#' \code{\link{switch_parameter_space}}. When a numeric vector in optimization
#' space is supplied, it is converted via `switch_parameter_space()`.
#'
#' @param logarithm \[`logical(1)`\]\cr
#' Return the log-likelihood? If `FALSE`, the likelihood is returned.
#'
#' @param negative \[`logical(1)`\]\cr
#' Return the negative (log-)likelihood? Useful for minimization routines.
#'
#' @param input_checks \[`logical(1)`\]\cr
#' Forwarded to \code{\link{choiceprob_probit}} or
#' \code{\link{choiceprob_logit}} to control additional input validation.
#'
#' @param lower_bound \[`numeric(1)`\]\cr
#' The minimum probability used when computing the log-likelihood. Values below
#' this bound are truncated to avoid taking the logarithm of zero.
#'
#' @param ...
#' Additional arguments passed to \code{\link{choiceprob_probit}} or
#' \code{\link{choiceprob_logit}}.
#'
#' @return
#' `choice_likelihood()` returns an object of class `choice_likelihood`, which
#' is a `list` containing the design matrices, the choice indices, and the
#' identifiers. `compute_choice_likelihood()` returns a single numeric value
#' with the (negative) log-likelihood or likelihood, depending on `logarithm`
#' and `negative`.
#'
#' @export
#'
#' @keywords probability
#'
#' @examples
#' data(train_choice)
#'
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | time,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(
#'     J = 2, alternatives = c("A", "B")
#'   )
#' )
#'
#' choice_data <- choice_data(
#'   data_frame = train_choice,
#'   format = "wide",
#'   column_choice = "choice",
#'   column_decider = "deciderID",
#'   column_occasion = "occasionID"
#' )
#'
#' likelihood <- choice_likelihood(
#'   choice_data = choice_data,
#'   choice_effects = choice_effects
#' )
#'
#' choice_parameters <- generate_choice_parameters(choice_effects)
#'
#' compute_choice_likelihood(
#'   choice_parameters = choice_parameters,
#'   choice_data = likelihood,
#'   choice_effects = choice_effects,
#'   logarithm = TRUE
#' )

choice_likelihood <- function(
    choice_data,
    choice_effects,
    choice_identifiers = extract_choice_identifiers(choice_data),
    input_checks = TRUE,
    lower_bound = 1e-10,
    ...
  ) {

  ### input checks
  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  checkmate::assert_flag(input_checks, na.ok = FALSE)
  oeli::input_check_response(
    check = checkmate::check_number(lower_bound, lower = 0, finite = TRUE),
    var_name = "lower_bound"
  )

  ### prepare repeated-use quantities
  design_list <- design_matrices(
    x = choice_data,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )
  choice_indices <- extract_choice_indices(
    choice_data = choice_data,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )
  Tp <- attr(design_list, "Tp")
  prob_args <- list(...)

  ### evaluation function
  objective <- function(
      choice_parameters,
      logarithm = TRUE,
      negative = FALSE,
      ...
    ) {

    params <- choice_parameters
    if (!is.list(params)) {
      params <- switch_parameter_space(
        choice_parameters = params,
        choice_effects = choice_effects
      )
    }
    is.choice_parameters(params, error = TRUE)

    prob_args_eval <- prob_args
    extra_args <- list(...)
    if (length(extra_args)) {
      prob_args_eval <- utils::modifyList(prob_args_eval, extra_args)
    }

    probabilities <- do.call(
      evaluate_choice_probabilities,
      c(
        list(
          design_list = design_list,
          choice_identifiers = choice_identifiers,
          choice_effects = choice_effects,
          choice_parameters = params,
          choice_only = TRUE,
          choice_indices = choice_indices,
          input_checks = input_checks
        ),
        prob_args_eval
      )
    )
    if (!inherits(probabilities, "choice_probabilities")) {
      cli::cli_abort(
        "Evaluating the likelihood requires valid choice probabilities.",
        call = NULL
      )
    }
    column_probabilities <- attr(probabilities, "column_probabilities")
    prob_column <- if (length(column_probabilities)) {
      column_probabilities[1]
    } else {
      "choice_probability"
    }
    prob_vec <- as.numeric(probabilities[[prob_column]])
    prob_vec <- pmax(prob_vec, lower_bound)

    value <- if (isTRUE(logarithm)) sum(log(prob_vec)) else prod(prob_vec)
    if (isTRUE(negative)) -value else value
  }

  structure(
    list(
      objective = objective,
      design_matrices = design_list,
      choice_indices = choice_indices,
      choice_identifiers = choice_identifiers
    ),
    class = c("choice_likelihood", "list"),
    Tp = Tp,
    choice_effects = choice_effects
  )
}

#' @noRd

is.choice_likelihood <- function(
    x,
    error = FALSE,
    var_name = oeli::variable_name(x)
  ) {
  validate_choice_object(
    x = x,
    class_name = "choice_likelihood",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_likelihood
#' @export

compute_choice_likelihood <- function(
    choice_parameters,
    choice_data,
    choice_effects,
    logarithm = TRUE,
    negative = FALSE,
    lower_bound = 1e-10,
    input_checks = TRUE,
    ...
  ) {

  ### prepare likelihood object
  likelihood <- if (inherits(choice_data, "choice_likelihood")) {
    is.choice_likelihood(choice_data, error = TRUE)
    if (missing(choice_effects)) {
      choice_effects <- attr(choice_data, "choice_effects")
    }
    choice_data
  } else {
    choice_likelihood(
      choice_data = choice_data,
      choice_effects = choice_effects,
      choice_identifiers = extract_choice_identifiers(choice_data),
      input_checks = input_checks,
      lower_bound = lower_bound,
      ...
    )
  }

  ### evaluate (log-)likelihood
  likelihood$objective(
    choice_parameters = choice_parameters,
    logarithm = logarithm,
    negative = negative,
    ...
  )
}

