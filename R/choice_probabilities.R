#' Define choice probabilities
#'
#' @description
#' The `choice_probabilities` object defines the choice probabilities.
#'
#' - `compute_choice_probabilities()` calculates the choice probabilities based
#'   on the choice parameters and the choice data.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice probabilities.
#' @inheritParams choice_identifiers
#'
#' @param choice_only \[`logical(1)`\]\cr
#' Only the probabilities for the chosen alternatives?
#'
#' @param column_probabilities \[`character()`\]\cr
#' The column name of the `data_frame` with the choice probabilities for all
#' choice alternatives.
#'
#' If `choice_only = TRUE`, it is the name of a single column that contains the
#' probabilities for the chosen alternatives.
#'
#' @param logarithm \[`logical(1)`\]\cr
#' Are the supplied or requested values log-probabilities?
#'
#' @return
#' A `choice_probabilities` tibble with the identifier columns followed by the
#' probability column(s). If `choice_only = TRUE`, there is a single column
#' `choice_probability` (or `log_choice_probability` if `logarithm = TRUE`).
#' Otherwise, there is one column per choice alternative. The attributes
#' `column_decider`, `column_occasion`, `cross_section`,
#' `column_probabilities`, `choice_only`, `logarithm`, and `aggregate` store
#' the column roles and the probability type.
#'
#' If `choice_only = FALSE` and `aggregate = "decider"`, the tibble instead has
#' one row per possible outcome sequence and decider, with the sequence in the
#' list column `outcome`.
#'
#' @export
#'
#' @keywords probability
#'
#' @examples
#' ### multinomial logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(beta = c(0.2, -0.1))
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### multinomial probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Sigma = diag(c(0, 1))
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### ordered logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit"
#'   ),
#'   choice_alternatives = choice_alternatives(
#'     J = 3,
#'     ordered = TRUE
#'   )
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   gamma = c(0, 1)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters,
#'   choice_type = "ordered"
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### ordered probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(
#'     J = 3,
#'     ordered = TRUE
#'   )
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Sigma = 1,
#'   gamma = c(0, 1)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters,
#'   choice_type = "ordered"
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### ranked logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(beta = c(0.2, -0.1))
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters,
#'   choice_type = "ranked"
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### ranked probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Sigma = diag(c(0, 1))
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters,
#'   choice_type = "ranked"
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### mixed logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit",
#'     random_effects = c(
#'       x = "cn",
#'       z = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### mixed probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit",
#'     random_effects = c(
#'       x = "cn",
#'       z = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2),
#'   Sigma = diag(c(0, 1))
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### panel logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit",
#'     random_effects = c(
#'       x = "cn",
#'       z = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(
#'     N = 1L,
#'     Tp = 2L
#'   ),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### panel probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit",
#'     random_effects = c(
#'       x = "cn",
#'       z = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = c(0.2, -0.1),
#'   Omega = matrix(c(0.1, 0.02, 0.02, 0.1), nrow = 2),
#'   Sigma = diag(c(0, 1))
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(
#'     N = 1L,
#'     Tp = 2L
#'   ),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects,
#'   aggregate = "decider",
#'   cml = "ap"
#' )
#'
#' ### latent class logit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "logit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = list(c(0.2, -0.1), c(-0.2, 0.1)),
#'   weights = c(0.5, 0.5)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )
#'
#' ### latent class probit
#' set.seed(1)
#' effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ x + z | 0,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 2)
#' )
#' parameters <- choice_parameters(
#'   beta = list(c(0.2, -0.1), c(-0.2, 0.1)),
#'   Sigma = diag(c(0, 1)),
#'   weights = c(0.5, 0.5)
#' )
#' simulated_data <- generate_choice_data(
#'   choice_effects = effects,
#'   choice_identifiers = generate_choice_identifiers(N = 1L),
#'   choice_parameters = parameters
#' )
#' compute_choice_probabilities(
#'   choice_parameters = parameters,
#'   choice_data = simulated_data,
#'   choice_effects = effects
#' )

choice_probabilities <- function(
  data_frame,
  choice_only = TRUE,
  column_decider = "deciderID",
  column_occasion = NULL,
  cross_section = is.null(column_occasion),
  column_probabilities = NULL,
  logarithm = FALSE,
  aggregate = c("occasion", "decider")
) {

  check_not_missing(data_frame)
  check_choice_only(choice_only)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_cross_section(cross_section)
  oeli::input_check_response(
    check = checkmate::check_flag(logarithm),
    var_name = "logarithm"
  )
  aggregate <- match.arg(aggregate)
  if (is.null(column_probabilities) && isTRUE(choice_only)) {
    column_probabilities <- if (logarithm) {
      "log_choice_probability"
    } else {
      "choice_probability"
    }
  }
  check_column_probabilities(
    column_probabilities, len = if (choice_only) 1L, null.ok = FALSE
  )
  data_frame <- check_data_frame(
    data_frame,
    required_columns = c(
      column_decider, column_occasion, column_probabilities
    ),
    allow_missing_columns = if (choice_only) {
      column_probabilities
    } else {
      character()
    }
  )
  for (column in column_probabilities) {
    check <- if (logarithm) {
      checkmate::check_numeric(
        data_frame[[column]], upper = sqrt(.Machine$double.eps), finite = FALSE,
        any.missing = choice_only
      )
    } else {
      checkmate::check_numeric(
        data_frame[[column]], lower = 0, upper = 1,
        finite = TRUE, any.missing = choice_only
      )
    }
    oeli::input_check_response(check = check, var_name = column)
    if (any(is.nan(data_frame[[column]]))) {
      cli::cli_abort(
        "Column {.val {column}} must not contain NaN values.",
        call = NULL
      )
    }
  }
  if (!choice_only) {
    values <- as.matrix(data_frame[column_probabilities])
    sums <- if (logarithm) {
      apply(values, 1L, function(x) {
        maximum <- max(x)
        if (identical(maximum, -Inf)) return(-Inf)
        maximum + log(sum(exp(x - maximum)))
      })
    } else {
      rowSums(values)
    }
    target <- if (logarithm) 0 else 1
    if (any(!is.finite(sums)) ||
        any(abs(sums - target) > sqrt(.Machine$double.eps))) {
      message <- if (logarithm) {
        "Log-probabilities must sum to one on the probability scale in every row."
      } else {
        "Alternative probabilities must sum to one in every row."
      }
      cli::cli_abort(
        message,
        call = NULL
      )
    }
  }

  choice_identifiers <- choice_identifiers(
    data_frame = data_frame[c(column_decider, column_occasion)],
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )

  choice_probabilities <- tibble::as_tibble(cbind(
    choice_identifiers, data_frame[column_probabilities]
  ))
  structure(
    choice_probabilities,
    class = tibble_class("choice_probabilities", class(data_frame)),
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    cross_section = attr(choice_identifiers, "cross_section"),
    column_probabilities = column_probabilities,
    choice_only = choice_only,
    logarithm = logarithm,
    aggregate = aggregate
  )

}

#' @rdname choice_probabilities
#'
#' @param choice_parameters \[`choice_parameters` | `numeric()` | `list()`\]\cr
#' A \code{\link{choice_parameters}} object.
#' A numeric vector in optimization space, as created by
#' \code{\link{switch_parameter_space}}, is also accepted.
#'
#' @param choice_data \[`choice_data`\]\cr
#' A \code{\link{choice_data}} object.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param input_checks \[`logical(1)`\]\cr
#' Should additional internal input checks be performed before computing the
#' probabilities?
#'
#' @param aggregate \[`character(1)`\]\cr
#' Probability unit. `"occasion"` returns one result per choice occasion.
#' `"decider"` returns the joint result for each decider's observed sequence of
#' choices.
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
#' @export

compute_choice_probabilities <- function(
  choice_parameters,
  choice_data,
  choice_effects,
  choice_only = TRUE,
  input_checks = TRUE,
  aggregate = c("occasion", "decider"),
  logarithm = FALSE,
  ...
) {

  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  check_choice_only(choice_only)
  oeli::input_check_response(
    check = checkmate::check_flag(input_checks),
    var_name = "input_checks"
  )
  aggregate <- match.arg(aggregate)
  oeli::input_check_response(
    check = checkmate::check_flag(logarithm),
    var_name = "logarithm"
  )
  probability_args <- list(...)
  parameter_batch <- is.list(choice_parameters) &&
    !inherits(choice_parameters, "choice_parameters") &&
    length(choice_parameters) > 0L &&
    all(vapply(choice_parameters, function(x) {
      inherits(x, "choice_parameters") || is.numeric(x)
    }, logical(1)))
  parameter_names <- if (parameter_batch) names(choice_parameters) else NULL
  parameters <- if (parameter_batch) choice_parameters else list(choice_parameters)
  parameters <- lapply(parameters, function(x) {
    if (!is.list(x)) {
      x <- switch_parameter_space(
        choice_parameters = x,
        choice_effects = choice_effects
      )
    }
    is.choice_parameters(x, error = TRUE)
    validate_choice_parameters(
      x,
      choice_effects,
      allow_missing = FALSE
    )
  })
  return_result <- function(result) {
    if (!parameter_batch) return(result[[1L]])
    if (!is.null(parameter_names)) names(result) <- parameter_names
    result
  }

  choice_identifiers <- extract_choice_identifiers(choice_data)
  inputs <- prepare_choice_inputs(
    choice_data, choice_effects, choice_identifiers
  )
  design_list <- inputs$design_matrices
  choice_indices <- inputs$choice_indices

  ranked <- identical(attr(choice_data, "choice_type"), "ranked")
  joint_outcomes <- !isTRUE(choice_only) &&
    identical(aggregate, "decider")
  if (joint_outcomes) {
    return(return_result(lapply(parameters, function(x) {
      do.call(
        evaluate_choice_outcomes,
        c(
          list(
            design_list = design_list,
            choice_identifiers = choice_identifiers,
            choice_effects = choice_effects,
            choice_parameters = x,
            choice_indices = choice_indices,
            logarithm = logarithm
          ),
          probability_args
        )
      )
    })))
  }

  observed <- lengths(choice_indices) > 0L
  all_identifiers <- NULL
  all_sequence_identifiers <- NULL
  if (isTRUE(choice_only) && any(!observed)) {
    if (identical(aggregate, "occasion")) {
      all_identifiers <- choice_identifiers
    } else {
      column_decider <- attr(choice_identifiers, "column_decider")
      sequence_data <- data.frame(
        unique(choice_identifiers[[column_decider]]),
        stringsAsFactors = FALSE
      )
      names(sequence_data) <- column_decider
      all_sequence_identifiers <- choice_identifiers(
        data_frame = sequence_data,
        column_decider = column_decider,
        column_occasion = NULL,
        cross_section = TRUE
      )
    }
    design_class <- class(design_list)
    alternatives <- attr(design_list, "alternatives")
    availability <- attr(design_list, "availability")
    choice_type <- attr(design_list, "choice_type")
    design_list <- design_list[observed]
    choice_indices <- choice_indices[observed]
    choice_identifiers <- choice_identifiers[observed, , drop = FALSE]
    if (any(observed)) {
      Tp <- read_Tp(choice_identifiers)
    } else {
      Tp <- integer()
    }
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

  probabilities <- lapply(parameters, function(x) {
    if (!length(design_list)) return(NULL)
    do.call(
      evaluate_choice_probabilities,
      c(
        list(
          design_list = design_list,
          choice_identifiers = choice_identifiers,
          choice_effects = choice_effects,
          choice_parameters = x,
          choice_only = choice_only,
          choice_indices = choice_indices,
          ranked = ranked && isTRUE(choice_only),
          input_checks = input_checks,
          aggregate = aggregate,
          logarithm = logarithm
        ),
        probability_args
      )
    )
  })
  probabilities <- lapply(probabilities, function(value) {
    if (!is.null(all_sequence_identifiers)) {
      probability_column <- if (logarithm) {
        "log_choice_probability"
      } else {
        "choice_probability"
      }
      probability <- rep(NA_real_, nrow(all_sequence_identifiers))
      if (length(design_list)) {
        result_column <- attr(value, "column_probabilities")
        column_decider <- attr(all_sequence_identifiers, "column_decider")
        result_position <- match(
          value[[column_decider]],
          all_sequence_identifiers[[column_decider]]
        )
        probability[result_position] <- value[[result_column]]
      }
      probability_data <- all_sequence_identifiers
      probability_data[[probability_column]] <- probability
      return(choice_probabilities(
        data_frame = probability_data,
        choice_only = TRUE,
        column_decider = attr(all_sequence_identifiers, "column_decider"),
        column_occasion = NULL,
        cross_section = TRUE,
        column_probabilities = probability_column,
        logarithm = logarithm,
        aggregate = "decider"
      ))
    }
    if (is.null(all_identifiers)) return(value)
    probability <- rep(NA_real_, nrow(all_identifiers))
    if (any(observed)) {
      probability_column <- attr(value, "column_probabilities")
      probability[observed] <- value[[probability_column]]
    }
    probability_column <- if (logarithm) {
      "log_choice_probability"
    } else {
      "choice_probability"
    }
    probability_data <- all_identifiers
    probability_data[[probability_column]] <- probability
    choice_probabilities(
      data_frame = probability_data,
      choice_only = TRUE,
      column_decider = attr(all_identifiers, "column_decider"),
      column_occasion = attr(all_identifiers, "column_occasion"),
      cross_section = attr(all_identifiers, "cross_section"),
      column_probabilities = probability_column,
      logarithm = logarithm,
      aggregate = aggregate
    )
  })
  return_result(probabilities)
}

#' @noRd

evaluate_choice_outcomes <- function(
  design_list, choice_identifiers, choice_effects, choice_parameters,
  choice_indices, logarithm = FALSE, ...
) {

  oeli::input_check_response(
    check = checkmate::check_flag(logarithm),
    var_name = "logarithm"
  )

  if (is.null(choice_parameters$beta)) {
    choice_parameters$beta <- numeric()
    if (!is.null(choice_parameters$weights)) {
      ### Empty latent classes are observationally equivalent.
      choice_parameters$weights <- NULL
    }
  }
  dots <- list(...)
  if (!is.null(dots$cml) && !identical(dots$cml, "no")) {
    cli::cli_warn(
      "All-outcome probabilities use the full likelihood, not CML.",
      call = NULL
    )
  }
  dots$cml <- NULL
  dots$logarithm <- NULL
  dots$numeric_only <- NULL

  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term
  alternatives <- as.character(
    attr(choice_effects, "choice_alternatives")
  )
  ranked <- identical(attr(design_list, "choice_type"), "ranked")
  ordered <- isTRUE(attr(
    attr(choice_effects, "choice_alternatives"), "ordered"
  ))
  availability <- attr(design_list, "availability")
  if (is.null(availability)) {
    availability <- Map(seq_len, lapply(design_list, nrow))
  }
  mixing <- as.character(stats::na.omit(choice_effects$mixing))

  omega_ref <- choice_parameters$Omega
  if (is.list(omega_ref)) {
    omega_ref <- omega_ref[[1]]
  }
  needs_draws <- !is.null(omega_ref) && (
    identical(error_term, "logit") ||
      any(random_effect_distribution(mixing) != "n")
  )
  if (needs_draws && is.null(dots$draws)) {
    n_draws <- if (is.null(dots$n_draws)) 200L else dots$n_draws
    check_n_draws(n_draws)
    dots$draws <- matrix(
      stats::rnorm(n_draws * nrow(omega_ref)),
      nrow = n_draws,
      ncol = nrow(omega_ref)
    )
  }

  prob_fun <- if (identical(error_term, "logit")) {
    choiceprob_logit
  } else {
    choiceprob_probit
  }
  fixed_coefficients <- is.null(choice_parameters$Omega)
  column_decider <- attr(choice_identifiers, "column_decider")
  decider <- choice_identifiers[[column_decider]]
  groups <- split(
    seq_along(decider),
    factor(decider, levels = unique(decider))
  )
  outcome_group <- vector("list", length(groups))
  prob_group <- vector("list", length(groups))
  decider_group <- vector("list", length(groups))

  for (g in seq_along(groups)) {
    idx <- groups[[g]]
    depths <- unique(lengths(choice_indices[idx]))
    depths <- depths[depths > 0L]
    missing_depth <- if (length(depths) == 1L) depths else NA_integer_
    options <- vector("list", length(idx))
    for (t in seq_along(idx)) {
      i <- idx[t]
      if (ordered) {
        options[[t]] <- as.list(seq_along(alternatives))
      } else if (!ranked) {
        options[[t]] <- as.list(as.integer(availability[[i]]))
      } else {
        depth <- length(choice_indices[[i]])
        if (!depth) {
          depth <- if (is.na(missing_depth)) {
            length(availability[[i]])
          } else {
            min(missing_depth, length(availability[[i]]))
          }
        }
        options[[t]] <- cpp_rankings(
          as.integer(availability[[i]]), as.integer(depth)
        )
      }
    }

    sizes <- lengths(options)
    grid <- expand.grid(
      lapply(sizes, seq_len),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    sequences <- vector("list", nrow(grid))
    for (r in seq_len(nrow(grid))) {
      sequence <- vector("list", length(options))
      for (t in seq_along(options)) {
        sequence[[t]] <- options[[t]][[grid[r, t]]]
      }
      sequences[[r]] <- sequence
    }

    prob <- numeric(nrow(grid))
    if (fixed_coefficients && length(idx) > 1L) {
      class_betas <- choice_parameters$beta
      if (is.null(class_betas) || !is.list(class_betas)) {
        class_betas <- list(class_betas)
      }
      class_weights <- choice_parameters$weights
      if (is.null(class_weights)) {
        class_weights <- 1
      }
      class_probs <- vector("list", length(class_betas))
      for (class in seq_along(class_betas)) {
        option_prob <- vector("list", length(options))
        for (t in seq_along(options)) {
          value <- numeric(length(options[[t]]))
          for (o in seq_along(options[[t]])) {
            prob_args <- c(
              list(
                X = design_list[idx[t]],
                y = list(options[[t]][[o]]),
                beta = class_betas[[class]],
                Omega = NULL,
                gamma = choice_parameters$gamma,
                weights = NULL,
                availability = availability[idx[t]],
                ranked = ranked,
                re_mixing = mixing,
                input_checks = FALSE,
                logarithm = FALSE
              ),
              dots
            )
            if (identical(error_term, "probit")) {
              prob_args$Sigma <- choice_parameters$Sigma
              prob_args$cml <- "no"
            }
            value[o] <- as.numeric(do.call(prob_fun, prob_args))
          }
          option_prob[[t]] <- value
        }
        class_probs[[class]] <- option_prob
      }
      for (r in seq_len(nrow(grid))) {
        class_values <- numeric(length(class_betas))
        for (class in seq_along(class_betas)) {
          class_values[class] <- 1
          for (t in seq_along(options)) {
            class_values[class] <- class_values[class] *
              class_probs[[class]][[t]][grid[r, t]]
          }
        }
        prob[r] <- sum(class_weights * class_values)
      }
    } else {
      for (r in seq_along(sequences)) {
        prob_args <- c(
          list(
            X = design_list[idx],
            y = sequences[[r]],
            Tp = if (length(idx) > 1L) length(idx) else NULL,
            beta = choice_parameters$beta,
            Omega = choice_parameters$Omega,
            gamma = choice_parameters$gamma,
            weights = choice_parameters$weights,
            availability = availability[idx],
            ranked = ranked,
            re_mixing = mixing,
            input_checks = FALSE,
            logarithm = FALSE
          ),
          dots
        )
        if (identical(error_term, "probit")) {
          prob_args$Sigma <- choice_parameters$Sigma
          prob_args$cml <- "no"
        }
        prob[r] <- as.numeric(do.call(prob_fun, prob_args))
      }
    }

    total <- sum(prob)
    if (any(!is.finite(prob)) || any(prob < 0) || total <= 0) {
      cli::cli_abort(
        "Joint outcome probabilities could not be evaluated.",
        call = NULL
      )
    }
    if (abs(total - 1) > 1e-3) {
      cli::cli_abort(
        "Joint outcome probabilities must sum to one.",
        call = NULL
      )
    }
    prob <- prob / total

    labels <- vector("list", length(sequences))
    for (r in seq_along(sequences)) {
      values <- vector("list", length(sequences[[r]]))
      for (t in seq_along(values)) {
        values[[t]] <- alternatives[sequences[[r]][[t]]]
      }
      if (ranked) {
        labels[[r]] <- if (length(values) == 1L) values[[1]] else values
      } else {
        values <- unlist(values, use.names = FALSE)
        labels[[r]] <- if (length(values) == 1L) values[[1]] else values
      }
    }
    outcome_group[[g]] <- labels
    prob_group[[g]] <- prob
    decider_group[[g]] <- rep(decider[idx[1]], length(prob))
  }

  outcome_all <- unlist(outcome_group, recursive = FALSE)
  prob_all <- unlist(prob_group, use.names = FALSE)
  decider_all <- do.call(c, decider_group)
  out <- data.frame(decider_all)
  names(out) <- column_decider
  out$outcome <- I(outcome_all)
  probability_column <- if (logarithm) {
    "log_choice_probability"
  } else {
    "choice_probability"
  }
  out[[probability_column]] <- if (logarithm) log(prob_all) else prob_all
  out <- tibble::as_tibble(out)
  structure(
    out,
    class = tibble_class("choice_probabilities", class(out)),
    column_decider = column_decider,
    column_occasion = NULL,
    cross_section = attr(choice_identifiers, "cross_section"),
    column_probabilities = probability_column,
    column_outcome = "outcome",
    choice_only = FALSE,
    joint = TRUE,
    aggregate = "decider",
    logarithm = logarithm
  )
}

#' @noRd

pmvnorm_cdf_default <- function(upper, corr, lower = -Inf) {
  corr_mat <- as.matrix(corr)
  if (!length(upper)) {
    return(1)
  }
  mvtnorm::pmvnorm(
    lower = lower,
    upper = upper,
    sigma = corr_mat,
    algorithm = mvtnorm::GenzBretz()
  )
}

#' @noRd

evaluate_choice_probabilities <- function(
    design_list,
    choice_identifiers,
    choice_effects,
    choice_parameters,
    choice_only,
    choice_indices = NULL,
    input_checks = TRUE,
    numeric_only = FALSE,
    logarithm = FALSE,
    aggregate = c("occasion", "decider"),
    ranked = identical(attr(design_list, "choice_type"), "ranked"),
    ...
  ) {

  aggregate <- match.arg(aggregate)

  if (isTRUE(choice_only) && is.null(choice_indices)) {
    cli::cli_abort(
      "Computing choice-only probabilities requires observed choice indices.",
      call = NULL
    )
  }
  if (nrow(choice_identifiers) != length(design_list)) {
    cli::cli_abort(
      "Probability evaluation returned a mismatched number of rows.",
      call = NULL
    )
  }

  beta_vec <- choice_parameters$beta
  if (is.null(beta_vec)) {
    beta_vec <- numeric()
  }
  weights <- choice_parameters$weights
  if (!length(beta_vec) && !is.null(weights)) {
    ### Empty latent classes are observationally equivalent.
    weights <- NULL
  }
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term
  Tp <- attr(design_list, "Tp")
  availability <- attr(design_list, "availability")
  if (is.null(availability)) {
    availability <- Map(seq_len, lapply(design_list, nrow))
  }
  column_decider <- attr(choice_identifiers, "column_decider")
  decider_ids <- choice_identifiers[[column_decider]]
  has_panel <- !is.null(Tp) && length(Tp) && any(Tp > 1)
  joint_panel <- isTRUE(choice_only) && has_panel &&
    identical(aggregate, "decider")
  eval_order <- seq_along(design_list)
  if (joint_panel) {
    decider_index <- match(decider_ids, unique(decider_ids))
    eval_order <- order(decider_index, seq_along(decider_index))
  }

  prob_args <- c(
    list(
      X = design_list[eval_order],
      y = if (isTRUE(choice_only)) choice_indices[eval_order] else NULL,
      Tp = if (joint_panel) Tp else NULL,
      beta = beta_vec,
      Omega = choice_parameters$Omega,
      Sigma = choice_parameters$Sigma,
      gamma = choice_parameters$gamma,
      weights = weights,
      availability = availability[eval_order],
      ranked = ranked,
      re_mixing = as.character(stats::na.omit(choice_effects$mixing)),
      input_checks = input_checks,
      logarithm = logarithm && isTRUE(choice_only)
    ),
    list(...)
  )
  if (identical(error_term, "logit") && !is.null(prob_args$cml)) {
    if (!identical(prob_args$cml, "no")) {
      cli::cli_abort(
        "Composite marginal likelihood is only available for probit.",
        call = NULL
      )
    }
    prob_args$cml <- NULL
  }

  probability <- switch(
    error_term,
    "probit" = {
      fixed <- is.null(choice_parameters$Omega) &&
        is.null(weights)
      if (joint_panel && fixed) {
        cml <- if (is.null(prob_args$cml)) "no" else prob_args$cml
        cml <- match.arg(cml, c("no", "fp", "ap"))
        cml_type <- match(cml, c("no", "fp", "ap")) - 1L
        prob_args$Tp <- NULL
        prob_args$cml <- NULL
        prob_args$logarithm <- TRUE
        obs_log <- do.call(choiceprob_probit, prob_args)
        panel_log <- cpp_cml_log(obs_log, as.integer(Tp), cml_type)
        if (logarithm) panel_log else exp(panel_log)
      } else {
        do.call(choiceprob_probit, prob_args)
      }
    },
    "logit" = {
      prob_args$Sigma <- NULL
      do.call(choiceprob_logit, prob_args)
    },
    cli::cli_abort(
      "Unsupported error term {.val {error_term}}.",
      call = NULL
    )
  )
  if (logarithm && !isTRUE(choice_only)) {
    probability <- log(probability)
  }
  invalid_probability <- !is.numeric(probability) || anyNA(probability) ||
    any(is.nan(probability)) || any(probability == Inf) ||
    if (logarithm) {
      any(probability > sqrt(.Machine$double.eps))
    } else {
      any(!is.finite(probability))
    }
  if (invalid_probability) {
    cli::cli_abort(
      "Choice {if (logarithm) 'log-' else ''}probabilities could not be
      evaluated to valid values.",
      call = NULL
    )
  }

  if (isTRUE(numeric_only)) {
    return(as.numeric(probability))
  }

  cross_section <- isTRUE(attr(choice_identifiers, "cross_section"))
  column_occasion <- attr(choice_identifiers, "column_occasion")
  output_identifiers <- choice_identifiers
  sequence_output <- joint_panel
  if (sequence_output) {
    decider_order <- unique(decider_ids[eval_order])
    identifier_data <- data.frame(decider_order, stringsAsFactors = FALSE)
    names(identifier_data) <- column_decider
    output_identifiers <- choice_identifiers(
      data_frame = identifier_data,
      column_decider = column_decider,
      column_occasion = NULL,
      cross_section = TRUE
    )
    cross_section <- TRUE
    column_occasion <- NULL
  }
  expected_rows <- nrow(output_identifiers)
  panel_observations <- (!cross_section) && !is.null(column_occasion)
  Tp_sum <- if (!is.null(Tp)) sum(Tp) else NA_integer_
  if (panel_observations && !is.null(Tp) && length(Tp) > 0 &&
      !is.na(Tp_sum) && Tp_sum == expected_rows) {
    if (is.numeric(probability) && length(probability) == length(Tp)) {
      index <- match(decider_ids, unique(decider_ids))
      probability <- probability[index]
    } else if (is.matrix(probability) &&
        nrow(probability) == length(Tp)) {
      index <- match(decider_ids, unique(decider_ids))
      probability <- probability[index, , drop = FALSE]
    }
  }

  choice_probabilities_df <- if (isTRUE(choice_only)) {
    probability_column <- if (logarithm) {
      "log_choice_probability"
    } else {
      "choice_probability"
    }
    stats::setNames(
      data.frame(as.numeric(probability)),
      probability_column
    )
  } else {
    as.data.frame(probability)
  }
  actual_rows <- nrow(choice_probabilities_df)
  if (!identical(actual_rows, expected_rows)) {
    cli::cli_abort(
      c(
        "Probability evaluation returned a mismatched number of rows.",
        "x" = "Expected {expected_rows} rows based on the choice identifiers but
        received {actual_rows}."
      ),
      call = NULL
    )
  }
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  column_probabilities <- if (isTRUE(choice_only)) {
    if (logarithm) "log_choice_probability" else "choice_probability"
  } else if (!is.null(choice_alternatives) && length(choice_alternatives)) {
    as.character(choice_alternatives)
  } else {
    colnames(choice_probabilities_df)
  }
  if (length(column_probabilities) == ncol(choice_probabilities_df)) {
    colnames(choice_probabilities_df) <- column_probabilities
  }

  choice_probabilities(
    data_frame = cbind(output_identifiers, choice_probabilities_df),
    choice_only = choice_only,
    column_decider = attr(output_identifiers, "column_decider"),
    column_occasion = attr(output_identifiers, "column_occasion"),
    cross_section = attr(output_identifiers, "cross_section"),
    column_probabilities = column_probabilities,
    logarithm = logarithm,
    aggregate = aggregate
  )
}

#' @noRd

build_panel_chunks <- function(Tp_n, cml_type, block = 1L) {
  if (Tp_n == 0) {
    return(list())
  }
  if (!cml_type %in% 0:2) {
    cli::cli_abort(
      "Unsupported composite marginal likelihood type {.val {cml_type}}.",
      call = NULL
    )
  }
  if (cml_type > 0L && Tp_n < 2L) {
    return(list())
  }
  if (length(block) > 1L) {
    if (length(block) != Tp_n) {
      cli::cli_abort(
        "Panel block sizes must match the number of occasions.",
        call = NULL
      )
    }
    ends <- cumsum(block)
    starts <- ends - block + 1L
    occasion <- vector("list", Tp_n)
    for (t in seq_len(Tp_n)) {
      occasion[[t]] <- if (!block[t]) {
        integer()
      } else {
        seq.int(starts[t], ends[t])
      }
    }
    if (cml_type == 0L) {
      return(list(unlist(occasion, use.names = FALSE)))
    }
    pairs <- if (cml_type == 1L) {
      utils::combn(seq_len(Tp_n), 2L, simplify = FALSE)
    } else {
      pair_list <- vector("list", Tp_n - 1L)
      for (t in seq_len(Tp_n - 1L)) {
        pair_list[[t]] <- c(t, t + 1L)
      }
      pair_list
    }
    chunks <- vector("list", length(pairs))
    for (k in seq_along(pairs)) {
      chunks[[k]] <- unlist(occasion[pairs[[k]]], use.names = FALSE)
    }
    return(chunks)
  }
  cpp_cml_chunks(
    as.integer(Tp_n), as.integer(block), as.integer(cml_type)
  )
}

#' @noRd

compute_chunk_product <- function(
  upper, corr, gcdf, chunk_indices, lower = NULL,
  logarithm = FALSE, return_chunks = FALSE
) {
  if (length(chunk_indices) == 0) {
    if (return_chunks) {
      return(numeric())
    }
    return(if (logarithm) 0 else 1)
  }
  probs <- numeric(length(chunk_indices))
  for (k in seq_along(chunk_indices)) {
    idx <- chunk_indices[[k]]
    corr_chunk <- corr[idx, idx, drop = FALSE]
    args <- list("upper" = upper[idx], "corr" = corr_chunk)
    if (is.null(lower)) {
      prob <- do.call(gcdf, args)
    } else if ("lower" %in% names(formals(gcdf))) {
      args$lower <- lower[idx]
      prob <- do.call(gcdf, args)
    } else {
      corners <- expand.grid(rep(list(c(FALSE, TRUE)), length(idx)))
      corner_prob <- numeric(nrow(corners))
      for (i in seq_len(nrow(corners))) {
        use_lower <- as.logical(corners[i, ])
        bound <- upper[idx]
        bound[use_lower] <- lower[idx][use_lower]
        corner_prob[i] <- (-1)^sum(use_lower) * do.call(
          gcdf,
          list("upper" = bound, "corr" = corr_chunk)
        )
      }
      prob <- sum(corner_prob)
    }
    probs[k] <- max(as.numeric(prob), 0)
  }
  if (return_chunks) {
    if (logarithm) {
      return(log(probs + .Machine$double.xmin))
    }
    return(probs)
  }
  cpp_prob_prod(probs, log = logarithm)
}

#' @noRd

block_diagonal <- function(matrices) {
  rows <- vapply(matrices, nrow, integer(1))
  columns <- vapply(matrices, ncol, integer(1))
  out <- matrix(0, sum(rows), sum(columns))
  row_end <- cumsum(rows)
  column_end <- cumsum(columns)
  for (i in seq_along(matrices)) {
    if (rows[i] && columns[i]) {
      out[
        seq.int(row_end[i] - rows[i] + 1L, row_end[i]),
        seq.int(column_end[i] - columns[i] + 1L, column_end[i])
      ] <- matrices[[i]]
    }
  }
  out
}

#' @noRd

compute_panel_probability <- function(
  X_n, y_n, beta, Omega_completed, Sigma, Tp_n, J,
  gcdf, ranked, cml_type, logarithm, availability_n,
  return_chunks = FALSE
) {
  V_n <- as.numeric(X_n %*% beta)
  delta_n <- vector("list", Tp_n)
  for (t in seq_len(Tp_n)) {
    ind <- (t - 1L) * J + seq_len(J)
    delta_n[[t]] <- cpp_probit_d(
      V_n[ind], as.integer(y_n[[t]]), ranked,
      as.integer(availability_n[[t]])
    )
  }
  D_n <- block_diagonal(lapply(delta_n, `[[`, "D"))
  cov_n <- cpp_probit_cov(
    X_n, Omega_completed, Sigma, D_n, as.integer(Tp_n)
  )
  scale <- as.numeric(cov_n$scale)
  upper <- unlist(lapply(delta_n, `[[`, "upper")) / scale
  blocks <- lengths(availability_n) - 1L
  chunk_indices <- build_panel_chunks(Tp_n, cml_type, block = blocks)
  compute_chunk_product(
    upper = upper,
    corr = cov_n$corr,
    gcdf = gcdf,
    chunk_indices = chunk_indices,
    logarithm = logarithm,
    return_chunks = return_chunks
  )
}

#' @noRd

compute_ordered_panel_probability <- function(
  X_n, y_n, beta, Omega_completed, Sigma, Tp_n, gamma_augmented,
  gcdf, cml_type, logarithm, return_chunks = FALSE
) {
  V_n <- as.numeric(X_n %*% beta)
  cov_n <- cpp_probit_cov(
    X_n, Omega_completed, matrix(Sigma), diag(Tp_n), as.integer(Tp_n)
  )
  scale <- as.numeric(cov_n$scale)
  ub <- (gamma_augmented[y_n + 1] - V_n) / scale
  lb <- (gamma_augmented[y_n] - V_n) / scale
  chunk_indices <- build_panel_chunks(Tp_n, cml_type)
  compute_chunk_product(
    upper = ub,
    corr = cov_n$corr,
    gcdf = gcdf,
    chunk_indices = chunk_indices,
    lower = lb,
    logarithm = logarithm,
    return_chunks = return_chunks
  )
}

#' @noRd

choiceprob_probit <- function(
  X, y = NULL, Tp = NULL, cml = "no", beta, Omega = NULL, Sigma,
  gamma = NULL, weights = NULL, re_position = NULL, re_mixing = NULL,
  availability = NULL, draws = NULL, n_draws = 200,
  gcdf = pmvnorm_cdf_default, input_checks = TRUE,
  logarithm = FALSE,
  ordered = !is.null(gamma),
  ranked = if (!ordered && !is.null(y) && isTRUE(length(y) > 0)) {
    length(y[[1]]) > 1
  } else {
    FALSE
  },
  mixed = !is.null(Omega),
  panel = !is.null(y) && (!is.null(Omega) || !is.null(weights)) &&
    !is.null(Tp) && any(Tp > 1),
  lc = !is.null(weights)
) {

  if (isTRUE(input_checks)) {
    oeli::input_check_response(
      check = checkmate::check_flag(logarithm),
      var_name = "logarithm"
    )
  }
  if (is.null(availability)) {
    availability <- Map(seq_len, lapply(X, nrow))
  }
  omega_ref <- if (is.matrix(Omega)) {
    Omega
  } else if (is.list(Omega) && length(Omega) && is.matrix(Omega[[1]])) {
    Omega[[1]]
  }
  if (is.null(re_position) && !is.null(omega_ref)) {
    beta_ref <- if (is.list(beta)) beta[[1]] else beta
    re_position <- utils::tail(seq_along(beta_ref), nrow(omega_ref))
  }
  if (!is.null(omega_ref)) {
    if (is.null(re_mixing)) {
      re_mixing <- rep("cn", nrow(omega_ref))
    }
    oeli::input_check_response(
      check = checkmate::check_character(
        re_mixing, len = nrow(omega_ref), any.missing = FALSE
      ),
      var_name = "re_mixing"
    )
    if (!all(re_mixing %in% c("cn", "n", "cln", "ln", "cln-", "ln-"))) {
      cli::cli_abort(
        "Random-effect distributions must be cn, n, cln, ln, cln-, or ln-.",
        call = NULL
      )
    }
  }

  if (isTRUE(input_checks)) {
    input_res <- choiceprob_probit_input_checks(
      X, y, Tp, cml, beta, Omega, Sigma, gamma, weights, re_position, gcdf,
      availability, model_type = NA
    )
    Tp <- input_res$Tp
    cml <- input_res$cml
    weights <- input_res$weights
    re_position <- input_res$re_position
    lc <- !is.null(weights)
    panel <- !is.null(y) && (mixed || lc) &&
      !is.null(Tp) && any(Tp > 1)
  }
  logarithm <- isTRUE(logarithm) && !is.null(y)

  flags <- c(ordered, ranked, mixed, panel, lc)
  oeli::input_check_response(
    check = checkmate::check_logical(
      flags, len = length(flags), any.missing = FALSE
    ),
    var_name = "model option flags"
  )
  if (ordered && ranked) {
    cli::cli_abort(
      "Ordered and ranked probability modes cannot be combined.",
      call = NULL
    )
  }
  if (!mixed && !lc && panel) {
    cli::cli_abort(
      "Panel probit probabilities require random effects.",
      call = NULL
    )
  }
  model_type <- sum(flags * 2^(seq_along(flags) - 1))

  if (isTRUE(input_checks)) {
    input_res <- choiceprob_probit_input_checks(
      X, y, Tp, cml, beta, Omega, Sigma, gamma, weights, re_position, gcdf,
      availability, model_type = model_type
    )
    Tp <- input_res$Tp
    cml <- input_res$cml
    weights <- input_res$weights
    re_position <- input_res$re_position
    lc <- !is.null(weights)
    panel <- !is.null(y) && (mixed || lc) &&
      !is.null(Tp) && any(Tp > 1)
  }

  if (mixed && any(random_effect_distribution(re_mixing) != "n")) {
    return(choiceprob_smnp(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta,
      Omega = Omega, Sigma = Sigma, gamma = gamma,
      weights = weights, re_position = re_position,
      re_mixing = re_mixing, availability = availability,
      draws = draws, n_draws = n_draws, gcdf = gcdf,
      ranked = ranked, panel = panel, logarithm = logarithm
    ))
  }

  switch(
    as.character(model_type),
    `0` = choiceprob_mnp(
      X = X, y = y, beta = beta, Sigma = Sigma,
      gcdf = gcdf, ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `1` = choiceprob_mnp_ordered(
      X = X, y = y, beta = beta, Sigma = Sigma, gamma = gamma,
      logarithm = logarithm
    ),
    `2` = choiceprob_mnp(
      X = X, y = y, beta = beta, Sigma = Sigma,
      gcdf = gcdf, ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    `4` = choiceprob_mmnp(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, ranked = ranked,
      logarithm = logarithm, availability = availability
    ),
    `5` = choiceprob_mmnp_ordered(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      gamma = gamma, re_position = re_position, logarithm = logarithm
    ),
    `6` = choiceprob_mmnp(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, ranked = TRUE,
      logarithm = logarithm, availability = availability
    ),
    `12` = choiceprob_mmnp_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, re_position = re_position, gcdf = gcdf,
      ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `13` = choiceprob_mmnp_ordered_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, gamma = gamma, re_position = re_position,
      gcdf = gcdf, logarithm = logarithm
    ),
    `14` = choiceprob_mmnp_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, re_position = re_position, gcdf = gcdf,
      ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    `16` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = NULL, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `17` = choiceprob_mmnp_ordered_lc(
      X = X, y = y, beta = beta, Omega = NULL, Sigma = Sigma,
      gamma = gamma, weights = weights, re_position = re_position,
      logarithm = logarithm
    ),
    `18` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = NULL, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    `24` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = NULL,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `25` = choiceprob_mmnp_ordered_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = NULL,
      Sigma = Sigma, gamma = gamma, weights = weights,
      re_position = re_position, gcdf = gcdf,
      logarithm = logarithm
    ),
    `26` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = NULL,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    `20` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `21` = choiceprob_mmnp_ordered_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      gamma = gamma, weights = weights, re_position = re_position,
      logarithm = logarithm
    ),
    `22` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    `28` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, ranked = ranked, logarithm = logarithm,
      availability = availability
    ),
    `29` = choiceprob_mmnp_ordered_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, gamma = gamma, weights = weights,
      re_position = re_position, gcdf = gcdf,
      logarithm = logarithm
    ),
    `30` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, ranked = TRUE, logarithm = logarithm,
      availability = availability
    ),
    cli::cli_abort(
      "Unsupported combination of model options in {.fn choiceprob_probit}.",
      call = NULL
    )
  )
}

#' @noRd

choiceprob_smnp <- function(
  X, y, Tp, cml, beta, Omega, Sigma, gamma, weights,
  re_position, re_mixing, availability, draws, n_draws, gcdf,
  ranked, panel, logarithm
) {

  lc <- !is.null(weights)
  beta_list <- if (lc) beta else list(beta)
  omega_list <- if (lc) Omega else list(Omega)
  class_weights <- if (lc) weights else 1
  draw_list <- prepare_lc_draws(draws, n_draws, omega_list)

  if (is.null(gamma)) {
    conditional_fun <- choiceprob_mnp
    conditional_args <- list(
      X = X, y = y, Sigma = Sigma, gcdf = gcdf, ranked = ranked,
      logarithm = TRUE, availability = availability
    )
  } else {
    conditional_fun <- choiceprob_mnp_ordered
    conditional_args <- list(
      X = X, y = y, Sigma = Sigma, gamma = gamma,
      logarithm = TRUE
    )
  }

  if (is.null(y)) {
    conditional_args$logarithm <- FALSE
    class_probs <- vector("list", length(class_weights))
    for (class in seq_along(class_weights)) {
      class_probs[[class]] <- average_over_draws(
        draws = draw_list[[class]], beta = beta_list[[class]],
        re_position = re_position, Omega = omega_list[[class]],
        re_mixing = re_mixing, compute_fun = conditional_fun,
        compute_args = conditional_args, logarithm = FALSE
      )
    }
    return(cpp_lc_prob(
      class_probs, class_weights, log = FALSE
    ))
  }

  cml_type <- match(cml, c("no", "fp", "ap")) - 1L
  counts <- rep(1L, length(X))
  chunks <- NULL
  if (panel) {
    counts <- integer(length(Tp))
    chunks <- list()
    pos <- c(0L, cumsum(Tp))
    chunk_index <- 0L
    for (n in seq_along(Tp)) {
      chunks_n <- build_panel_chunks(Tp[n], cml_type)
      counts[n] <- length(chunks_n)
      for (k in seq_along(chunks_n)) {
        chunk_index <- chunk_index + 1L
        chunks[[chunk_index]] <- pos[n] + chunks_n[[k]]
      }
    }
  }
  if (!sum(counts)) {
    out <- numeric(length(counts))
    return(if (logarithm) out else exp(out))
  }

  class_logs <- vector("list", length(class_weights))
  for (class in seq_along(class_weights)) {
    class_logs[[class]] <- average_over_draws(
      draws = draw_list[[class]], beta = beta_list[[class]],
      re_position = re_position, Omega = omega_list[[class]],
      re_mixing = re_mixing, compute_fun = conditional_fun,
      compute_args = conditional_args, logarithm = TRUE,
      chunks = chunks
    )
  }
  component_log <- cpp_lc_prob(
    class_logs, class_weights, log = TRUE
  )
  if (!panel) {
    return(if (logarithm) component_log else exp(component_log))
  }

  ends <- cumsum(counts)
  starts <- ends - counts + 1L
  log_prob <- numeric(length(counts))
  for (n in seq_along(counts)) {
    if (counts[n]) {
      log_prob[n] <- sum(component_log[starts[n]:ends[n]])
    }
  }
  if (logarithm) log_prob else exp(log_prob)
}

#' @noRd

choiceprob_mnp <- function(
    X, y, beta, Sigma,
    gcdf = pmvnorm_cdf_default,
    ranked = FALSE, logarithm = FALSE,
    availability = Map(seq_len, lapply(X, nrow))
) {
  N <- length(X)
  J <- dim(Sigma)[1]
  if (J == 2L && identical(gcdf, pmvnorm_cdf_default)) {
    return(cpp_binary_probit(
      X, y, beta, Sigma, log = logarithm,
      availability = availability
    ))
  }
  if (is.null(y)) {
    probs_all <- matrix(0, N, J)
    for (n in seq_len(N)) {
      for (j in availability[[n]]) {
        probs_all[n, j] <- choiceprob_mnp(
          X = X[n], y = list(j), beta = beta, Sigma = Sigma,
          gcdf = gcdf, ranked = FALSE, logarithm = FALSE,
          availability = availability[n]
        )
      }
    }
    return(probs_all / rowSums(probs_all))
  }
  probabilities <- numeric(N)
  for (n in seq_len(N)) {
    if (length(availability[[n]]) == 1L) {
      probabilities[n] <- if (logarithm) 0 else 1
    } else {
      V_n <- as.numeric(X[[n]] %*% beta)
      delta_n <- cpp_probit_d(
        V_n, as.integer(y[[n]]), ranked,
        as.integer(availability[[n]])
      )
      omega <- matrix(0, ncol(X[[n]]), ncol(X[[n]]))
      cov_n <- cpp_probit_cov(
        X[[n]], omega, Sigma, delta_n$D, 1L
      )
      upper <- delta_n$upper / as.numeric(cov_n$scale)
      if (length(upper) == 1L &&
          (logarithm || identical(gcdf, pmvnorm_cdf_default))) {
        probabilities[n] <- stats::pnorm(upper, log.p = logarithm)
      } else {
        prob_n <- do.call(
          gcdf, list("upper" = upper, "corr" = cov_n$corr)
        )
        prob_n <- max(as.numeric(prob_n), 0)
        probabilities[n] <- if (logarithm) {
          log(prob_n + .Machine$double.xmin)
        } else {
          prob_n
        }
      }
    }
  }
  probabilities
}

#' @noRd

choiceprob_mnp_ordered <- function(
    X, y, beta, Sigma, gamma, logarithm = FALSE
) {
  cpp_ordered_probit(
    X, y, beta, Sigma, gamma, log = logarithm
  )
}

#' @noRd

choiceprob_mmnp <- function(
    X, y, beta, Omega, Sigma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    ranked = FALSE, logarithm = FALSE,
    availability = Map(seq_len, lapply(X, nrow))
) {
  N <- length(X)
  P <- length(beta)
  J <- dim(Sigma)[1]
  if (J == 2L && identical(gcdf, pmvnorm_cdf_default)) {
    return(cpp_binary_probit(
      X, y, beta, Sigma, omega = Omega,
      position = as.integer(re_position), log = logarithm,
      availability = availability
    ))
  }
  if (is.null(y)) {
    probs_all <- matrix(0, N, J)
    for (n in seq_len(N)) {
      for (j in availability[[n]]) {
        probs_all[n, j] <- choiceprob_mmnp(
          X = X[n], y = list(j), beta = beta, Omega = Omega,
          Sigma = Sigma, re_position = re_position, gcdf = gcdf,
          ranked = FALSE, logarithm = FALSE,
          availability = availability[n]
        )
      }
    }
    return(probs_all / rowSums(probs_all))
  }
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  probabilities <- numeric(N)
  for (n in seq_len(N)) {
    if (length(availability[[n]]) == 1L) {
      probabilities[n] <- if (logarithm) 0 else 1
    } else {
      V_n <- as.numeric(X[[n]] %*% beta)
      delta_n <- cpp_probit_d(
        V_n, as.integer(y[[n]]), ranked,
        as.integer(availability[[n]])
      )
      cov_n <- cpp_probit_cov(
        X[[n]], Omega_completed, Sigma, delta_n$D, 1L
      )
      upper <- delta_n$upper / as.numeric(cov_n$scale)
      if (length(upper) == 1L &&
          (logarithm || identical(gcdf, pmvnorm_cdf_default))) {
        probabilities[n] <- stats::pnorm(upper, log.p = logarithm)
      } else {
        prob_n <- do.call(
          gcdf, list("upper" = upper, "corr" = cov_n$corr)
        )
        prob_n <- max(as.numeric(prob_n), 0)
        probabilities[n] <- if (logarithm) {
          log(prob_n + .Machine$double.xmin)
        } else {
          prob_n
        }
      }
    }
  }
  probabilities
}

#' @noRd

choiceprob_mmnp_ordered <- function(
    X, y, beta, Omega, Sigma, gamma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    logarithm = FALSE
) {
  cpp_ordered_probit(
    X, y, beta, Sigma, gamma, omega = Omega,
    position = as.integer(re_position), log = logarithm
  )
}

#' @noRd

choiceprob_mmnp_lc <- function(
    X, y, beta, Omega, Sigma, weights,
    re_position = NULL,
    gcdf = pmvnorm_cdf_default,
    ranked = FALSE, logarithm = FALSE,
    availability = Map(seq_len, lapply(X, nrow))
) {
  if (is.null(re_position) && !is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
  }
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    class_prob <- if (is.null(Omega)) {
      choiceprob_mnp(
        X = X, y = y, beta = beta[[c]], Sigma = Sigma,
        gcdf = gcdf, ranked = ranked, logarithm = logarithm,
        availability = availability
      )
    } else {
      choiceprob_mmnp(
        X = X, y = y, beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
        re_position = re_position, gcdf = gcdf, ranked = ranked,
        logarithm = logarithm, availability = availability
      )
    }
    probs[[c]] <- class_prob
  }
  cpp_lc_prob(probs, weights, log = logarithm)
}

#' @noRd

choiceprob_mmnp_ordered_lc <- function(
    X, y, beta, Omega, Sigma, gamma, weights,
    re_position = NULL, logarithm = FALSE
) {
  if (is.null(re_position) && !is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
  }
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    class_prob <- if (is.null(Omega)) {
      choiceprob_mnp_ordered(
        X = X, y = y, beta = beta[[c]], Sigma = Sigma,
        gamma = gamma, logarithm = logarithm
      )
    } else {
      choiceprob_mmnp_ordered(
        X = X, y = y, beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
        gamma = gamma, re_position = re_position,
        logarithm = logarithm
      )
    }
    probs[[c]] <- class_prob
  }
  cpp_lc_prob(probs, weights, log = logarithm)
}

#' @noRd

choiceprob_mmnp_panel <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    ranked = FALSE, logarithm = FALSE,
    availability = Map(seq_len, lapply(X, nrow)),
    return_chunks = FALSE
) {
  N <- length(Tp)
  J <- dim(Sigma)[1]
  P <- length(beta)
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  cml_type <- switch(cml,
    "no" = 0,
    "fp" = 1,
    "ap" = 2
  )
  csTp <- c(0, cumsum(Tp))
  probabilities <- if (return_chunks) vector("list", N) else numeric(N)
  for (n in seq_len(N)) {
    ind_n <- (csTp[n] + 1):(csTp[n + 1])
    X_n <- do.call(rbind, X[ind_n])
    y_n <- y[ind_n]
    prob_n <- compute_panel_probability(
      X_n = X_n,
      y_n = y_n,
      beta = beta,
      Omega_completed = Omega_completed,
      Sigma = Sigma,
      Tp_n = Tp[n],
      J = J,
      gcdf = gcdf,
      ranked = ranked,
      cml_type = cml_type,
      logarithm = logarithm,
      availability_n = availability[ind_n],
      return_chunks = return_chunks
    )
    if (return_chunks) {
      probabilities[[n]] <- prob_n
    } else {
      probabilities[n] <- prob_n
    }
  }
  probabilities
}

#' @noRd

choiceprob_mmnp_ordered_panel <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, gamma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    logarithm = FALSE, return_chunks = FALSE
) {
  N <- length(Tp)
  P <- length(beta)
  J <- length(gamma) + 1
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  gamma_augmented <- c(-Inf, gamma, +Inf)
  cml_type <- switch(
    cml,
    "no" = 0,
    "fp" = 1,
    "ap" = 2
  )
  csTp <- c(0, cumsum(Tp))
  probabilities <- if (return_chunks) vector("list", N) else numeric(N)
  for (n in seq_len(N)) {
    ind_n <- (csTp[n] + 1):(csTp[n + 1])
    if (length(ind_n) == 1) {
      if (cml_type > 0L) {
        prob_n <- if (return_chunks) numeric() else {
          if (logarithm) 0 else 1
        }
      } else {
        prob_n <- choiceprob_mmnp_ordered(
          X = X[ind_n], y = y[ind_n], beta = beta, Omega = Omega,
          Sigma = Sigma, gamma = gamma, re_position = re_position,
          logarithm = logarithm
        )
        if (return_chunks) prob_n <- as.numeric(prob_n)
      }
    } else {
      X_n <- do.call(rbind, X[ind_n])
      y_n <- do.call(c, y[ind_n])
      prob_n <- compute_ordered_panel_probability(
        X_n = X_n,
        y_n = y_n,
        beta = beta,
        Omega_completed = Omega_completed,
        Sigma = Sigma,
        Tp_n = Tp[n],
        gamma_augmented = gamma_augmented,
        gcdf = gcdf,
        cml_type = cml_type,
        logarithm = logarithm,
        return_chunks = return_chunks
      )
    }
    if (return_chunks) {
      probabilities[[n]] <- prob_n
    } else {
      probabilities[n] <- prob_n
    }
  }
  probabilities
}

#' @noRd

choiceprob_mmnp_panel_lc <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, weights,
    re_position = NULL,
    gcdf = pmvnorm_cdf_default,
    ranked = FALSE, logarithm = FALSE,
    availability = Map(seq_len, lapply(X, nrow))
) {
  if (is.null(re_position) && !is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
  }
  C <- length(weights)
  cml_type <- match(cml, c("no", "fp", "ap")) - 1L
  class_chunks <- vector("list", C)
  for (c in seq_len(C)) {
    if (is.null(Omega)) {
      obs_log <- choiceprob_mnp(
        X = X, y = y, beta = beta[[c]], Sigma = Sigma,
        gcdf = gcdf, ranked = ranked, logarithm = TRUE,
        availability = availability
      )
      pos <- c(0L, cumsum(Tp))
      class_chunks[[c]] <- vector("list", length(Tp))
      for (n in seq_along(Tp)) {
        idx <- pos[n] + seq_len(Tp[n])
        chunks <- build_panel_chunks(Tp[n], cml_type)
        chunk_log <- numeric(length(chunks))
        for (k in seq_along(chunks)) {
          chunk_log[k] <- sum(obs_log[idx[chunks[[k]]]])
        }
        class_chunks[[c]][[n]] <- chunk_log
      }
    } else {
      class_chunks[[c]] <- choiceprob_mmnp_panel(
        X = X, y = y, Tp = Tp, cml = cml,
        beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
        re_position = re_position, gcdf = gcdf, ranked = ranked,
        logarithm = TRUE, availability = availability,
        return_chunks = TRUE
      )
    }
  }
  log_prob <- numeric(length(Tp))
  for (n in seq_along(Tp)) {
    if (length(class_chunks[[1]][[n]])) {
      chunks <- lapply(class_chunks, `[[`, n)
      log_prob[n] <- sum(cpp_lc_prob(chunks, weights, log = TRUE))
    }
  }
  if (logarithm) log_prob else exp(log_prob)
}

#' @noRd

choiceprob_mmnp_ordered_panel_lc <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, gamma, weights,
    re_position = NULL,
    gcdf = pmvnorm_cdf_default,
    logarithm = FALSE
) {
  if (is.null(re_position) && !is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
  }
  C <- length(weights)
  cml_type <- match(cml, c("no", "fp", "ap")) - 1L
  class_chunks <- vector("list", C)
  for (c in seq_len(C)) {
    if (is.null(Omega)) {
      obs_log <- choiceprob_mnp_ordered(
        X = X, y = y, beta = beta[[c]], Sigma = Sigma,
        gamma = gamma, logarithm = TRUE
      )
      pos <- c(0L, cumsum(Tp))
      class_chunks[[c]] <- vector("list", length(Tp))
      for (n in seq_along(Tp)) {
        idx <- pos[n] + seq_len(Tp[n])
        chunks <- build_panel_chunks(Tp[n], cml_type)
        chunk_log <- numeric(length(chunks))
        for (k in seq_along(chunks)) {
          chunk_log[k] <- sum(obs_log[idx[chunks[[k]]]])
        }
        class_chunks[[c]][[n]] <- chunk_log
      }
    } else {
      class_chunks[[c]] <- choiceprob_mmnp_ordered_panel(
        X = X, y = y, Tp = Tp, cml = cml,
        beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
        gamma = gamma, re_position = re_position, gcdf = gcdf,
        logarithm = TRUE, return_chunks = TRUE
      )
    }
  }
  log_prob <- numeric(length(Tp))
  for (n in seq_along(Tp)) {
    if (length(class_chunks[[1]][[n]])) {
      chunks <- lapply(class_chunks, `[[`, n)
      log_prob[n] <- sum(cpp_lc_prob(chunks, weights, log = TRUE))
    }
  }
  if (logarithm) log_prob else exp(log_prob)
}

#' @noRd

choiceprob_logit <- function(
  X, y = NULL, Tp = NULL, beta, Omega = NULL, gamma = NULL,
  weights = NULL, re_mixing = NULL, availability = NULL,
  input_checks = TRUE,
  ordered = !is.null(gamma),
  ranked = !ordered && !is.null(y) && length(y) > 0 && length(y[[1]]) > 1,
  panel = !is.null(Tp) && any(Tp > 1),
  lc = !is.null(weights),
  draws = NULL,
  n_draws = 200,
  logarithm = FALSE
) {

  if (is.null(availability)) {
    availability <- Map(seq_len, lapply(X, nrow))
  }

  if (isTRUE(input_checks)) {
    oeli::input_check_response(
      check = checkmate::check_flag(logarithm),
      var_name = "logarithm"
    )
    input_res <- choiceprob_logit_input_checks(
      X = X, y = y, Tp = Tp, beta = beta, Omega = Omega, gamma = gamma,
      weights = weights, ordered = ordered, ranked = ranked,
      panel = panel, lc = lc, draws = draws, n_draws = n_draws,
      availability = availability
    )
    weights <- input_res$weights
  }

  if (ordered && ranked) {
    cli::cli_abort(
      "Ranked outcomes are not supported for ordered logit models.",
      call = NULL
    )
  }

  if (lc) {
    lc_panel <- !is.null(y) && isTRUE(panel)
    if (!is.null(Omega)) {
      re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
      return(choiceprob_mmnl_lc(
        X = X, y = y, Tp = if (lc_panel) Tp else NULL,
        beta = beta, Omega = Omega, gamma = gamma, weights = weights,
        re_position = re_position, ranked = ranked,
        draws = draws, n_draws = n_draws, logarithm = logarithm,
        re_mixing = re_mixing, availability = availability
      ))
    }
    return(choiceprob_mnl_lc(
      X = X, y = y, Tp = if (lc_panel) Tp else NULL,
      beta = beta, gamma = gamma, weights = weights, ranked = ranked,
      logarithm = logarithm, availability = availability
    ))
  }

  if (!is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta), nrow(Omega))
    return(choiceprob_mmnl(
      X = X, y = y, Tp = if (!is.null(y) && isTRUE(panel)) Tp else NULL,
      beta = beta, Omega = Omega, gamma = gamma,
      re_position = re_position, ranked = ranked,
      draws = draws, n_draws = n_draws, logarithm = logarithm,
      re_mixing = re_mixing, availability = availability
    ))
  }

  choiceprob_mnl(
    X = X, y = y, Tp = if (!is.null(y) && isTRUE(panel)) Tp else NULL,
    beta = beta, gamma = gamma, ranked = ranked,
    logarithm = logarithm, availability = availability
  )
}

#' @noRd

choiceprob_mmnl <- function(
  X, y, beta, Omega, re_position, gamma = NULL, Tp = NULL,
  ranked = FALSE,
  draws = NULL, n_draws = 200, logarithm = FALSE,
  re_mixing = NULL,
  availability = Map(seq_len, lapply(X, nrow))
) {
  draws_mat <- prepare_mixed_logit_draws(draws, n_draws, Omega)
  if (!is.null(gamma)) {
    return(average_over_draws(
      draws = draws_mat,
      beta = beta,
      re_position = re_position,
      Omega = Omega,
      re_mixing = re_mixing,
      compute_fun = choiceprob_mnl,
      compute_args = list(
        X = X, y = y, gamma = gamma, Tp = Tp, ranked = ranked,
        logarithm = logarithm,
        availability = availability
      ),
      logarithm = logarithm
    ))
  }
  if (is.null(re_mixing)) {
    re_mixing <- rep("cn", length(re_position))
  }
  type <- match(
    random_effect_distribution(re_mixing), c("ln-", "n", "ln")
  ) - 2L
  oeli::input_check_response(
    check = checkmate::check_integer(
      type, len = length(re_position), any.missing = FALSE
    ),
    var_name = "re_mixing"
  )
  cpp_mmnl(
    X, y, beta, chol(Omega), draws_mat, as.integer(re_position), type,
    tp = if (is.null(Tp)) NULL else as.integer(Tp),
    ranked = ranked, log = logarithm, availability = availability
  )
}

#' @noRd

choiceprob_mmnl_lc <- function(
  X, y, beta, Omega, weights, re_position, gamma = NULL, Tp = NULL,
  ranked = FALSE,
  draws = NULL, n_draws = 200, logarithm = FALSE,
  re_mixing = NULL,
  availability = Map(seq_len, lapply(X, nrow))
) {
  draw_list <- prepare_lc_draws(draws, n_draws, Omega)
  probs <- vector("list", length = length(weights))
  for (c in seq_along(weights)) {
    probs[[c]] <- choiceprob_mmnl(
      X = X, y = y, beta = beta[[c]], Omega = Omega[[c]],
      gamma = gamma, Tp = Tp, re_position = re_position, ranked = ranked,
      draws = draw_list[[c]], n_draws = nrow(draw_list[[c]]),
      logarithm = logarithm, re_mixing = re_mixing,
      availability = availability
    )
  }
  cpp_lc_prob(probs, weights, log = logarithm)
}

#' @noRd

prepare_mixed_logit_draws <- function(draws, n_draws, Omega) {
  dim_random <- nrow(Omega)
  if (!length(dim_random) || dim_random == 0) {
    cli::cli_abort(
      "Random effect covariance {.var Omega} must have positive dimension.",
      call = NULL
    )
  }
  if (!is.null(draws)) {
    oeli::input_check_response(
      check = checkmate::check_matrix(
        draws, mode = "numeric", min.rows = 1,
        ncols = dim_random, any.missing = FALSE
      ),
      var_name = "draws"
    )
    oeli::input_check_response(
      check = checkmate::check_numeric(as.numeric(draws), finite = TRUE),
      var_name = "draws"
    )
    return(draws)
  }
  check_n_draws(n_draws)
  n <- as.integer(n_draws)
  matrix(stats::rnorm(n * dim_random), nrow = n, ncol = dim_random)
}

#' @noRd

prepare_lc_draws <- function(draws, n_draws, Omega_list) {
  shared_draws <- prepare_mixed_logit_draws(draws, n_draws, Omega_list[[1]])
  replicate(length(Omega_list), shared_draws, simplify = FALSE)
}

#' @noRd

average_over_draws <- function(
  draws, beta, re_position, compute_fun, compute_args, Omega,
  re_mixing = NULL, logarithm = FALSE, chunks = NULL
) {
  if (is.null(re_mixing)) {
    re_mixing <- rep("cn", length(re_position))
  }
  type <- match(
    random_effect_distribution(re_mixing), c("ln-", "n", "ln")
  ) - 2L
  oeli::input_check_response(
    check = checkmate::check_integer(
      type, len = length(re_position), any.missing = FALSE
    ),
    var_name = "re_mixing"
  )
  cpp_average_draws(
    draws, beta, as.integer(re_position), compute_fun, log = logarithm,
    chol = chol(Omega), type = type, args = compute_args, chunks = chunks
  )
}

#' @noRd

choiceprob_mnl <- function(
  X, y, beta, gamma = NULL, Tp = NULL, ranked = FALSE,
  logarithm = FALSE,
  availability = Map(seq_len, lapply(X, nrow))
) {
  if (is.null(gamma)) {
    probability <- if (is.null(y)) {
      cpp_mnl_all(X, beta, log = logarithm, availability = availability)
    } else {
      cpp_mnl_chosen(
        X, y, beta, ranked = ranked, log = logarithm,
        availability = availability
      )
    }
  } else {
    choices <- if (is.null(y)) NULL else as.integer(unlist(y))
    probability <- cpp_ologit(
      X, beta, gamma, y = choices, log = logarithm
    )
  }
  if (is.null(Tp) || is.null(y)) {
    return(probability)
  }
  cpp_panel_prod(
    probability, as.integer(Tp), log = logarithm,
    input_log = logarithm
  )
}

#' @noRd

choiceprob_mnl_lc <- function(
  X, y, beta, weights, gamma = NULL, Tp = NULL, ranked = FALSE,
  logarithm = FALSE,
  availability = Map(seq_len, lapply(X, nrow))
) {
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    probs[[c]] <- choiceprob_mnl(
      X = X, y = y, beta = beta[[c]], gamma = gamma, Tp = Tp,
      ranked = ranked, logarithm = logarithm,
      availability = availability
    )
  }
  cpp_lc_prob(probs, weights, log = logarithm)
}
