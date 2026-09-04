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
#'   `choice_parameters` and a pre-computed `choice_likelihood` object.
#'
#' @param choice_data \[`choice_data`\]\cr
#' A \code{\link{choice_data}} object.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object.
#' The default is extracted from `choice_data`.
#'
#' @param choice_parameters \[`choice_parameters` | `numeric()` | `list()`\]\cr
#' A \code{\link{choice_parameters}} object.
#'
#' A numeric vector in optimization space is also accepted and converted with
#' `switch_parameter_space()`.
#'
#' A list of either representation is evaluated as a batch of parameter draws.
#'
#' @param choice_likelihood \[`choice_likelihood`\]\cr
#' A \code{\link{choice_likelihood}} object.
#'
#' @param logarithm \[`logical(1)`\]\cr
#' Return the log-likelihood? If `FALSE`, the likelihood is returned.
#'
#' @param negative \[`logical(1)`\]\cr
#' Return the negative (log-)likelihood? Useful for minimization routines.
#'
#' @param input_checks \[`logical(1)`\]\cr
#' Check the pre-computed design matrices and choice indices against the
#' parameters on the first likelihood evaluation? Later evaluations skip these
#' checks because the pre-computed quantities do not change.
#'
#' @param aggregate \[`character(1)`\]\cr
#' Unit of the returned likelihood:
#'
#' - `"occasion"` returns one contribution per observed choice occasion,
#' - `"decider"` returns one joint contribution per decider,
#' - `"total"` sums the decider contributions.
#'
#' @param ...
#' Additional arguments for the probability computation:
#'
#' - `n_draws` \[`integer(1)`\]: The number of draws for simulated
#'   probabilities, which are required for mixed logit models and for mixed
#'   probit models with non-normal random effects. The default is `200`.
#' - `draws` \[`matrix`\]: A matrix of standard normal draws with one column
#'   per random effect that replaces the generated draws.
#' - `cml` \[`character(1)`\]: Composite marginal likelihood for panel probit
#'   models. Either `"no"` (default, the full likelihood), `"fp"` (all pairs of
#'   choice occasions), or `"ap"` (adjacent pairs of choice occasions).
#' - `gcdf` \[`function`\]: The Gaussian CDF used for probit probabilities,
#'   with arguments `upper` and `corr`. The default uses
#'   \code{\link[mvtnorm]{pmvnorm}}.
#'
#' @return
#' `choice_likelihood()` returns an object of class `choice_likelihood`, which
#' is a `list` containing the design matrices, choice indices, and identifiers.
#'
#' `compute_choice_likelihood()` returns a numeric scalar for
#' `aggregate = "total"` and a named numeric vector otherwise.
#'
#' @export
#'
#' @keywords probability
#'
#' @examples
#' ### compute choice likelihood
#' data(list = "train_choice")
#'
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | time,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(
#'     J = 2,
#'     alternatives = c("A", "B")
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
#' choice_parameters <- generate_choice_parameters(
#'   choice_effects = choice_effects
#' )
#'
#' compute_choice_likelihood(
#'   choice_parameters = choice_parameters,
#'   choice_likelihood = likelihood,
#'   logarithm = TRUE
#' )

choice_likelihood <- function(
    choice_data,
    choice_effects,
    choice_identifiers = extract_choice_identifiers(choice_data),
    input_checks = TRUE,
    ...
  ) {

  ### input checks
  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  oeli::input_check_response(
    check = checkmate::check_flag(input_checks),
    var_name = "input_checks"
  )
  ### prepare repeated-use quantities
  inputs <- prepare_choice_inputs(
    choice_data, choice_effects, choice_identifiers
  )
  design_list <- inputs$design_matrices
  choice_indices <- inputs$choice_indices

  ### remove observations without a response
  observed <- lengths(choice_indices) > 0L
  if (any(!observed)) {
    availability <- attr(design_list, "availability")
    alternatives <- attr(design_list, "alternatives")
    choice_type <- attr(design_list, "choice_type")
    design_class <- class(design_list)
    design_list <- design_list[observed]
    choice_indices <- choice_indices[observed]
    choice_identifiers <- choice_identifiers[observed, , drop = FALSE]
    Tp <- read_Tp(choice_identifiers)
    design_list <- structure(
      design_list,
      class = design_class,
      Tp = Tp,
      alternatives = alternatives,
      availability = availability[observed],
      choice_type = choice_type
    )
    attr(choice_indices, "Tp") <- Tp
  }
  Tp <- attr(design_list, "Tp")
  prob_args <- list(...)

  ### keep simulated likelihoods deterministic across evaluations
  choice_formula <- attr(choice_effects, "choice_formula")
  P_r <- sum(!is.na(choice_effects$mixing))
  simulated <- P_r > 0L && (
    identical(choice_formula$error_term, "logit") ||
      any(random_effect_distribution(
        stats::na.omit(choice_effects$mixing)
      ) != "n")
  )
  if (simulated && is.null(prob_args$draws)) {
    n_draws <- if (is.null(prob_args$n_draws)) 200L else prob_args$n_draws
    check_n_draws(n_draws)
    prob_args$draws <- matrix(
      stats::rnorm(n_draws * P_r),
      nrow = n_draws,
      ncol = P_r
    )
    prob_args$n_draws <- NULL
  }

  ### evaluation function (input checks run on the first evaluation only)
  checks_pending <- isTRUE(input_checks)
  objective <- function(
      choice_parameters,
      logarithm = TRUE,
      negative = FALSE,
      aggregate = c("total", "decider", "occasion"),
      ...
    ) {

    oeli::input_check_response(
      check = checkmate::check_flag(logarithm),
      var_name = "logarithm"
    )
    oeli::input_check_response(
      check = checkmate::check_flag(negative),
      var_name = "negative"
    )
    aggregate <- match.arg(aggregate)
    params <- choice_parameters
    if (!is.list(params)) {
      params <- switch_parameter_space(
        choice_parameters = params,
        choice_effects = choice_effects
      )
    }
    is.choice_parameters(params, error = TRUE)
    params <- validate_choice_parameters(
      params,
      choice_effects,
      allow_missing = FALSE
    )

    prob_args_eval <- prob_args
    extra_args <- list(...)
    if (length(extra_args)) {
      prob_args_eval <- utils::modifyList(prob_args_eval, extra_args)
    }
    prob_args_eval$numeric_only <- TRUE
    prob_args_eval$logarithm <- TRUE
    prob_args_eval$aggregate <- if (identical(aggregate, "occasion")) {
      "occasion"
    } else {
      "decider"
    }

    log_prob <- if (length(choice_indices)) {
      do.call(
        evaluate_choice_probabilities,
        c(
          list(
            design_list = design_list,
            choice_identifiers = choice_identifiers,
            choice_effects = choice_effects,
            choice_parameters = params,
            choice_only = TRUE,
            choice_indices = choice_indices,
            input_checks = checks_pending
          ),
          prob_args_eval
        )
      )
    } else {
      numeric()
    }
    checks_pending <<- FALSE
    if (!is.numeric(log_prob) || anyNA(log_prob)) {
      cli::cli_abort(
        "Evaluating the likelihood requires numeric log contributions.",
        call = NULL
      )
    }
    if (identical(aggregate, "occasion")) {
      identifier_data <- as.data.frame(choice_identifiers)
      contribution_names <- if (ncol(identifier_data) > 1L) {
        do.call(paste, c(identifier_data, sep = ":"))
      } else {
        as.character(identifier_data[[1L]])
      }
      names(log_prob) <- contribution_names
    } else if (identical(aggregate, "decider")) {
      column_decider <- attr(choice_identifiers, "column_decider")
      names(log_prob) <- unique(as.character(
        choice_identifiers[[column_decider]]
      ))
    }
    log_value <- if (identical(aggregate, "total")) sum(log_prob) else log_prob
    value <- if (isTRUE(logarithm)) log_value else exp(log_value)
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
  check_choice_object(
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
    choice_likelihood,
    logarithm = TRUE,
    negative = FALSE,
    aggregate = c("total", "decider", "occasion"),
    ...
  ) {

  is.choice_likelihood(choice_likelihood, error = TRUE)
  aggregate <- match.arg(aggregate)

  parameter_batch <- is.list(choice_parameters) &&
    !inherits(choice_parameters, "choice_parameters") &&
    length(choice_parameters) > 0L &&
    all(vapply(choice_parameters, function(x) {
      inherits(x, "choice_parameters") || is.numeric(x)
    }, logical(1)))

  if (parameter_batch) {
    values <- lapply(choice_parameters, function(parameters) {
      choice_likelihood$objective(
        choice_parameters = parameters,
        logarithm = logarithm,
        negative = negative,
        aggregate = aggregate,
        ...
      )
    })
    draw_names <- names(choice_parameters)
    if (identical(aggregate, "total")) {
      result <- unlist(values, use.names = FALSE)
      if (!is.null(draw_names)) names(result) <- draw_names
      return(result)
    }
    result <- do.call(cbind, values)
    if (!is.null(draw_names)) colnames(result) <- draw_names
    return(result)
  }

  ### evaluate (log-)likelihood
  choice_likelihood$objective(
    choice_parameters = choice_parameters,
    logarithm = logarithm,
    negative = negative,
    aggregate = aggregate,
    ...
  )
}
