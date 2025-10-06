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
#' @return
#' A `choice_probabilities` S3 object (a data frame) that stores additional
#' metadata in attributes such as `column_probabilities`, `choice_only`, and the
#' identifier columns. These attributes are used by downstream helpers to
#' reconstruct the original structure.
#'
#' @export
#'
#' @keywords probability
#'
#' @examples
#' data(train_choice)
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | time,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(
#'     J = 2, alternatives = c("A", "B")
#'   )
#' )
#' choice_parameters <- generate_choice_parameters(choice_effects)
#' choice_data <- choice_data(
#'   data_frame = train_choice,
#'   format = "wide",
#'   column_choice = "choice",
#'   column_decider = "deciderID",
#'   column_occasion = "occasionID"
#' )
#' compute_choice_probabilities(
#'   choice_parameters = choice_parameters,
#'   choice_data = choice_data,
#'   choice_effects = choice_effects,
#'   choice_only = TRUE
#' )

choice_probabilities <- function(
  data_frame,
  choice_only = TRUE,
  column_decider = "deciderID",
  column_occasion = NULL,
  cross_section = FALSE,
  column_probabilities = if (choice_only) "choice_probability"
) {

  ### input checks
  check_not_missing(data_frame)
  check_choice_only(choice_only)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_cross_section(cross_section)
  check_column_probabilities(
    column_probabilities, len = if (choice_only) 1L, null.ok = FALSE
  )
  data_frame <- check_data_frame(
    data_frame,
    required_columns = c(column_decider, column_occasion, column_probabilities)
  )

  ### extract choice identifiers
  choice_identifiers <- choice_identifiers(
    data_frame = data_frame[c(column_decider, column_occasion)],
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )

  ### build 'choice_probabilities' object
  choice_probabilities <- cbind(
    choice_identifiers, data_frame[column_probabilities]
  )
  structure(
    choice_probabilities,
    class = unique(c("choice_probabilities", "data.frame", class(data_frame))),
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    cross_section = attr(choice_identifiers, "cross_section"),
    column_probabilities = column_probabilities,
    choice_only = choice_only
  )

}

#' @rdname choice_probabilities
#'
#' @param choice_parameters \[`choice_parameters` | `list`\]\cr
#' Either a \code{\link{choice_parameters}} object or a list in optimization
#' space as returned by \code{\link{switch_parameter_space}}.
#'
#' @param choice_data \[`choice_data`\]\cr
#' A \code{\link{choice_data}} object providing responses and covariates.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object defining the specification.
#'
#' @param input_checks \[`logical(1)`\]\cr
#' Should additional internal input checks be performed before computing the
#' probabilities?
#'
#' @param ...
#' Passed to the underlying probability computation routine.
#'
#' @export

compute_choice_probabilities <- function(
  choice_parameters,
  choice_data,
  choice_effects,
  choice_only = FALSE,
  input_checks = TRUE,
  ...
) {

  ### check inputs
  if (!is.list(choice_parameters)) {
    choice_parameters <- switch_parameter_space(
      choice_parameters = choice_parameters,
      choice_effects = choice_effects
    )
  }
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)

  ### build design matrices and choice identifiers
  choice_identifiers <- extract_choice_identifiers(choice_data)
  design_list <- design_matrices(
    x = choice_data,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )
  choice_indices <- if (isTRUE(choice_only)) {
    extract_choice_indices(
      choice_data = choice_data,
      choice_effects = choice_effects,
      choice_identifiers = choice_identifiers
    )
  } else {
    NULL
  }

  ### compute probabilities via shared evaluation helper
  evaluate_choice_probabilities(
    design_list = design_list,
    choice_identifiers = choice_identifiers,
    choice_effects = choice_effects,
    choice_parameters = choice_parameters,
    choice_only = choice_only,
    choice_indices = choice_indices,
    input_checks = input_checks,
    ...
  )
}

#' @noRd

pmvnorm_cdf_default <- function(upper, corr) {
  corr_mat <- as.matrix(corr)
  if (!length(upper)) {
    return(1)
  }
  mvtnorm::pmvnorm(
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
    ...
  ) {

  if (isTRUE(choice_only) && is.null(choice_indices)) {
    cli::cli_abort(
      "Computing choice-only probabilities requires observed choice indices.",
      call = NULL
    )
  }

  beta_vec <- choice_parameters$beta
  if (is.null(beta_vec)) {
    beta_vec <- numeric()
  }
  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term

  prob_args <- c(
    list(
      X = design_list,
      y = if (isTRUE(choice_only)) choice_indices else NULL,
      Tp = attr(design_list, "Tp"),
      beta = beta_vec,
      Omega = choice_parameters$Omega,
      Sigma = choice_parameters$Sigma,
      gamma = choice_parameters$gamma,
      input_checks = input_checks
    ),
    list(...)
  )

  if (isTRUE(choice_only) && identical(error_term, "logit")) {
    prob_args$Tp <- NULL
  }

  probability <- switch(
    error_term,
    "probit" = do.call(choiceprob_probit, prob_args),
    "logit" = {
      prob_args$Sigma <- NULL
      do.call(choiceprob_logit, prob_args)
    },
    cli::cli_abort(
      "Unsupported error term {.val {error_term}}.",
      call = NULL
    )
  )

  Tp <- attr(design_list, "Tp")
  cross_section <- isTRUE(attr(choice_identifiers, "cross_section"))
  column_occasion <- attr(choice_identifiers, "column_occasion")
  expected_rows <- nrow(choice_identifiers)
  panel_observations <- (!cross_section) && !is.null(column_occasion)
  Tp_sum <- if (!is.null(Tp)) sum(Tp) else NA_integer_
  if (panel_observations && !is.null(Tp) && length(Tp) > 0 &&
      !is.na(Tp_sum) && Tp_sum == expected_rows) {
    if (is.numeric(probability) && length(probability) == length(Tp)) {
      probability <- rep(probability, times = Tp)
    } else if (is.matrix(probability) &&
        nrow(probability) == length(Tp)) {
      probability <- probability[rep(seq_len(nrow(probability)), times = Tp),
        , drop = FALSE]
    }
  }

  choice_probabilities_df <- if (isTRUE(choice_only)) {
    data.frame(choice_probability = as.numeric(probability))
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
    "choice_probability"
  } else if (!is.null(choice_alternatives) && length(choice_alternatives)) {
    as.character(choice_alternatives)
  } else {
    colnames(choice_probabilities_df)
  }
  if (length(column_probabilities) == ncol(choice_probabilities_df)) {
    colnames(choice_probabilities_df) <- column_probabilities
  }

  choice_probabilities(
    data_frame = cbind(choice_identifiers, choice_probabilities_df),
    choice_only = choice_only,
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    cross_section = attr(choice_identifiers, "cross_section"),
    column_probabilities = column_probabilities
  )
}

#' @noRd

build_panel_chunks <- function(Tp_n, cml_type) {
  if (Tp_n == 0) {
    return(list())
  }
  if (Tp_n == 1 || cml_type == 0L) {
    list(seq_len(Tp_n))
  } else if (cml_type == 1L) {
    oeli::subsets(seq_len(Tp_n), n = 2)
  } else if (cml_type == 2L) {
    Map(c, seq_len(Tp_n)[-Tp_n], seq_len(Tp_n)[-1])
  } else {
    stop("Unsupported composite marginal likelihood type", call. = FALSE)
  }
}

#' @noRd

compute_chunk_product <- function(
    upper, corr, gcdf, lower_bound, chunk_indices, lower = NULL) {
  if (length(chunk_indices) == 0) {
    return(1)
  }
  probs <- vapply(chunk_indices, function(idx) {
    corr_chunk <- corr[idx, idx, drop = FALSE]
    prob <- do.call(
      gcdf,
      list("upper" = upper[idx], "corr" = corr_chunk)
    )
    if (!is.null(lower)) {
      prob <- prob - do.call(
        gcdf,
        list("upper" = lower[idx], "corr" = corr_chunk)
      )
    }
    max(prob, lower_bound)
  }, numeric(1))
  prod(probs)
}

#' @noRd

compute_panel_probability <- function(
    X_n, y_n, beta, Omega_completed, Sigma, Tp_n, J,
    gcdf, lower_bound, ranked, cml_type) {
  V_n <- X_n %*% beta
  delta_n <- lapply(seq_len(Tp_n), function(t) {
    if (ranked) {
      oeli::M(ranking = y_n[[t]], dim = J)
    } else {
      oeli::delta(ref = y_n[[t]], dim = J)
    }
  })
  D_n <- as.matrix(Matrix::bdiag(delta_n))
  Upsilon_n <- X_n %*% Omega_completed %*% t(X_n) + diag(Tp_n) %x% Sigma
  upper <- as.numeric(D_n %*% (-V_n))
  corr <- stats::cov2cor(D_n %*% Upsilon_n %*% t(D_n))
  chunk_indices <- lapply(
    build_panel_chunks(Tp_n, cml_type),
    oeli::map_indices,
    n = J - 1
  )
  compute_chunk_product(
    upper = upper,
    corr = corr,
    gcdf = gcdf,
    lower_bound = lower_bound,
    chunk_indices = chunk_indices
  )
}

#' @noRd

compute_ordered_panel_probability <- function(
    X_n, y_n, beta, Omega_completed, Sigma, Tp_n, gamma_augmented,
    gcdf, lower_bound, cml_type) {
  V_n <- as.numeric(X_n %*% beta)
  Upsilon_n <- X_n %*% Omega_completed %*% t(X_n) + diag(Tp_n) %x% Sigma
  ub <- gamma_augmented[y_n + 1] - V_n
  ub[is.infinite(ub)] <- 100
  lb <- gamma_augmented[y_n] - V_n
  lb[is.infinite(lb)] <- -100
  corr <- stats::cov2cor(Upsilon_n)
  chunk_indices <- build_panel_chunks(Tp_n, cml_type)
  compute_chunk_product(
    upper = ub,
    corr = corr,
    gcdf = gcdf,
    lower_bound = lower_bound,
    chunk_indices = chunk_indices,
    lower = lb
  )
}

#' Calculate probit choice probabilities
#'
#' @description
#' These helper functions calculate probit choice probabilities for various
#' scenarios:
#'
#' - in the regular (`choiceprob_mnp_*`), ordered (`*_ordered`), and
#'   ranked (`ranked = TRUE`) case,
#' - in the normally mixed (`choiceprob_mmnp_*`) and latent class (`*_lc`) case,
#' - for panel data (`*_panel`),
#' - based on the full likelihood (`cml = "no"`), the full pairwise composite
#'   marginal likelihood (`cml = "fp"`), and the adjacent pairwise composite
#'   marginal likelihood (`cml = "ap"`),
#' - for the observed choices or for all alternatives (if `y` is `NULL`).
#'
#' The function `choiceprob_probit()` is the general API which calls the
#' specialized functions and can perform input checks.
#'
#' @param X \[`list(N)`\]\cr
#' A `list` of length `N` (number observations) of design matrices, each of
#' dimension `J` (number alternatives) times `P` (number effects).
#'
#' In the ordered case (`ordered = TRUE`), the design matrices are of dimension
#' `1` times `P`.
#'
#' @param y \[`list(N)` | `NULL`\]\cr
#' A `list` of length `N` (number observations) of single integers from `1` to
#' `J` (number alternatives).
#'
#' In the ranked case (`ranked = TRUE`), the list entries each must be a
#' permutation of `1:J`, where the higher-ranked alternatives are in front.
#'
#' In the non-panel case (`panel = FALSE`), `y` can also be `NULL`, in which
#' case probabilities are calculated for all choice alternatives.
#' In the ranked case (`ranked = TRUE`), if `y` is `NULL`,
#' only first place choice probabilities are computed, which is equivalent to
#' computing choice probabilities in the regular (maximum utility) model.
#'
#' @param Tp \[`NULL` | `integer(N)`\]\cr
#' The panel identifier of length `N` (number observations) for panel data.
#' The number `Tp[1]` indicates, that the first `Tp[1]` observations in `X` and
#' `y` belong to decider 1, the next `Tp[2]` observations belong to decider 2,
#' and so on.
#'
#' Can be `NULL` for no panel data.
#'
#' @param cml \[`character(1)`\]\cr
#' The composite marginal likelihood (CML) type for panel data. It can be one of
#' `"no"` (full likelihood), `"fp"` (full pairwise), or `"ap"` (adjacent
#' pairwise).
#'
#' @param beta \[`numeric(P)` | `list`\]\cr
#' The coefficient vector of length `P` (number effects) for computing the
#' systematic utility \eqn{V = X\beta}.
#'
#' In the latent class case (`lc = TRUE`), `beta` is a `list` of length `C` of
#' such coefficients, where `C` is the number of latent classes.
#'
#' @param Omega \[`matrix(nrow = P_r, ncol = P_r)` | `NULL` | `list`\]\cr
#' The covariance matrix of random effects of dimension `P_r` times `P_r`,
#' where `P_r` less than `P` is the number of random effects.
#'
#' Can be `NULL` for no random effects.
#'
#' In the latent class case (`lc = TRUE`), `Omega` is a `list` of length `C` of
#' such covariance matrices, where `C` is the number of latent classes.
#'
#' @param Sigma \[`matrix(nrow = J, ncol = J)` | `numeric(1)`\]\cr
#' The covariance matrix of dimension `J` times `J` (number alternatives) for
#' the Gaussian error term \eqn{\epsilon = U - V}.
#'
#' In the ordered case (`ordered = TRUE`), `Sigma` is a single, non-negative
#' `numeric`.
#'
#' @param gamma \[`NULL` | `numeric(J - 1)`\]\cr
#' Only relevant in the ordered case (`ordered = TRUE`). It defines the
#' non-decreasing boundaries of the utility categories.
#'
#' The event \eqn{U \leq \gamma_j} means that alternative \eqn{j} is chosen,
#' while \eqn{U > \gamma_{J - 1}} means that alternative \eqn{J} is chosen.
#'
#' @param weights \[`NULL` | `numeric(C)`\]\cr
#' The weights for the latent classes in the latent class case (`lc = TRUE`).
#'
#' @param re_position \[`integer(P_r)`\]\cr
#' The index positions of the random effects in the coefficient vector `beta`.
#'
#' By default, the last \eqn{P_r} entries of `beta` are considered as random,
#' where \eqn{P_r} is the dimension of Omega.
#'
#' @param gcdf \[`function(upper, corr)`\]\cr
#' A function that computes (or approximates) the centered Gaussian CDF
#' (mean is zero) based on the upper integration limit `upper` and correlation
#' matrix `corr`. The output is expected to be a single `numeric` value between
#' zero and one.
#'
#' In the no-panel (`panel = FALSE`) ordered case (`ordered = TRUE`),
#' `stats::pnorm()` is used to calculate the one-dimensional Gaussian CDF.
#'
#' @param lower_bound \[`numeric(1)`\]\cr
#' A lower bound for the probabilities for numerical reasons. Probabilities are
#' returned as `max(prob, lower_bound)`.
#'
#' @param input_checks \[`logical(1)`\]\cr
#' Perform input checks. Set to `FALSE` to skip them.
#'
#' @param ordered,ranked,mixed,panel,lc \[`logical(1)`\]\cr
#' Flags indicating the model type. These are determined automatically based on
#' the input arguments.
#'
#' @return
#' A `numeric` `vector` of length `N`, the probabilities for the observed
#' choices `y`.
#'
#' In the panel case (`panel = TRUE`), the probabilities of the observed choice
#' sequence of length `length(Tp)`.
#'
#' If `y` is `NULL` and in the non-panel case (`panel = FALSE`), a matrix of
#' dimension `N` times `J`, the probabilities for all alternatives.
#' In the ranked case (`ranked = TRUE`), only first place choice probabilities
#' are computed, which is equivalent to computing choice probabilities in the
#' regular (maximum utility) model.
#'
#' @keywords probability
#'
#' @export

choiceprob_probit <- function(
    X, y = NULL, Tp = NULL, cml = "no", beta, Omega = NULL, Sigma, gamma = NULL,
    weights = NULL, re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default, lower_bound = 0, input_checks = TRUE,
    ordered = !is.null(gamma),
    ranked = if (!ordered && !is.null(y) && isTRUE(length(y) > 0)) {
      length(y[[1]]) > 1
    } else {
      FALSE
    },
    mixed = !is.null(Omega),
    panel = mixed & !is.null(Tp) & any(Tp > 1),
    lc = !is.null(weights)
) {

  if (isTRUE(input_checks)) {
    ### input checks without knowing the model type yet
    input_res <- choiceprob_probit_input_checks(
      X, y, Tp, cml, beta, Omega, Sigma, gamma, weights, re_position, gcdf,
      lower_bound, model_type = NA
    )
    Tp <- input_res$Tp
    cml <- input_res$cml
    weights <- input_res$weights
    panel <- mixed & !is.null(Tp) & any(Tp > 1)
    lc <- !is.null(weights)
  }

  ### determine the model type
  if (ordered & ranked) stop()
  if (!mixed & panel) stop()
  flags <- c(ordered, ranked, mixed, panel, lc)
  model_type <- sum(flags * 2^(seq_along(flags) - 1))
  #  0: MNP
  #  1: ordered MNP
  #  2: ranked MNP
  #  4: MMNP
  #  5: ordered MMNP
  #  6: ranked MMNP
  # 12: panel MMNP
  # 13: ordered panel MMNP
  # 14: ranked panel MMNP
  # 16: lc MNP
  # 17: ordered lc MNP
  # 18: ranked lc MNP
  # 20: lc MMNP
  # 21: lc ordered MMNP
  # 22: lc ranked MMNP
  # 28: lc panel MMNP
  # 29: lc ordered panel MMNP
  # 30: lc ranked panel MMNP

  if (isTRUE(input_checks)) {
    ### check inputs again conditioned on the model type
    input_res <- choiceprob_probit_input_checks(
      X, y, Tp, cml, beta, Omega, Sigma, gamma, weights, re_position, gcdf,
      lower_bound, model_type = model_type
    )
    Tp <- input_res$Tp
    cml <- input_res$cml
    weights <- input_res$weights
    panel <- mixed & !is.null(Tp) & any(Tp > 1)
    lc <- !is.null(weights)
  }

  ### compute choice probabilities
  switch(
    as.character(model_type),
    `0` = choiceprob_mnp(
      X = X, y = y, beta = beta, Sigma = Sigma,
      gcdf = gcdf, lower_bound = lower_bound, ranked = ranked
    ),
    `1` = choiceprob_mnp_ordered(
      X = X, y = y, beta = beta, Sigma = Sigma, gamma = gamma,
      lower_bound = lower_bound
    ),
    `2` = choiceprob_mnp(
      X = X, y = y, beta = beta, Sigma = Sigma,
      gcdf = gcdf, lower_bound = lower_bound, ranked = TRUE
    ),
    `4` = choiceprob_mmnp(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound,
      ranked = ranked
    ),
    `5` = choiceprob_mmnp_ordered(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      gamma = gamma, re_position = re_position, lower_bound = lower_bound
    ),
    `6` = choiceprob_mmnp(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound,
      ranked = TRUE
    ),
    `12` = choiceprob_mmnp_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, re_position = re_position, gcdf = gcdf,
      lower_bound = lower_bound, ranked = ranked
    ),
    `13` = choiceprob_mmnp_ordered_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, gamma = gamma, re_position = re_position,
      gcdf = gcdf, lower_bound = lower_bound
    ),
    `14` = choiceprob_mmnp_panel(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, re_position = re_position, gcdf = gcdf,
      lower_bound = lower_bound, ranked = TRUE
    ),
    `20` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      lower_bound = lower_bound, ranked = ranked
    ),
    `21` = choiceprob_mmnp_ordered_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      gamma = gamma, weights = weights, re_position = re_position,
      lower_bound = lower_bound
    ),
    `22` = choiceprob_mmnp_lc(
      X = X, y = y, beta = beta, Omega = Omega, Sigma = Sigma,
      weights = weights, re_position = re_position, gcdf = gcdf,
      lower_bound = lower_bound, ranked = TRUE
    ),
    `28` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, lower_bound = lower_bound, ranked = ranked
    ),
    `29` = choiceprob_mmnp_ordered_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, gamma = gamma, weights = weights,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound
    ),
    `30` = choiceprob_mmnp_panel_lc(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta, Omega = Omega,
      Sigma = Sigma, weights = weights, re_position = re_position,
      gcdf = gcdf, lower_bound = lower_bound, ranked = TRUE
    ),
    cli::cli_abort(
      "Unsupported combination of model options in {.fn choiceprob_probit}.",
      call = NULL
    )
  )
}

#' @noRd

check_beta_list <- function(beta) {
  if (!checkmate::test_list(beta)) {
    return("Must be a list of numeric vectors")
  }
  if (!length(beta)) {
    return("Must not be empty")
  }
  for (c in seq_along(beta)) {
    beta_c <- beta[[c]]
    check_res <- oeli::check_numeric_vector(
      beta_c, finite = TRUE, any.missing = FALSE
    )
    if (!isTRUE(check_res)) {
      return(sprintf("Element %d: %s", c, check_res))
    }
  }
  TRUE
}

#' @noRd

choiceprob_probit_input_checks <- function(
    X, y, Tp, cml, beta, Omega, Sigma, gamma, weights, re_position, gcdf,
    lower_bound, model_type
) {

  result <- list(Tp = Tp, cml = cml, weights = weights)
  if (is.na(model_type)) {
    ### general input checks without knowing the model type

    ### X is a list
    oeli::input_check_response(
      check = checkmate::check_list(X),
      var_name = "X"
    )

    ### y is a list of the same length as X or NULL
    oeli::input_check_response(
      check = checkmate::check_list(y, len = length(X), null.ok = TRUE),
      var_name = "y"
    )

    ### Tp
    if (!is.null(Tp)) {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          Tp, lower = 1, any.missing = FALSE
        ),
        var_name = "Tp"
      )
      if (length(Tp) == 0) {
        cli::cli_abort("Panel counts {.var Tp} must not be empty.", call = NULL)
      }
      if (sum(Tp) != length(X)) {
        cli::cli_abort(
          "Sum of {.var Tp} must match the number of observations in {.var X}.",
          call = NULL
        )
      }
    }

    ### cml
    oeli::input_check_response(
      check = checkmate::check_choice(
        cml, choices = c("no", "fp", "ap")
      ),
      var_name = "cml"
    )

    ### beta is a numeric vector or a list of numeric vectors (latent class)
    oeli::input_check_response(
      check = list(
        oeli::check_numeric_vector(beta, finite = TRUE, any.missing = FALSE),
        check_beta_list(beta)
      ),
      var_name = "beta"
    )

    ### prepare beta dimensions for downstream checks
    if (checkmate::test_list(beta)) {
      beta_lengths <- vapply(beta, length, integer(1))
      beta_dim <- if (length(beta_lengths)) beta_lengths[1] else 0L
    } else {
      beta_dim <- length(beta)
      beta_lengths <- beta_dim
    }

    ### Omega
    P_r <- 0L
    if (is.null(Omega)) {
      P_r <- 0L
    } else if (is.matrix(Omega)) {
      omega_check <- oeli::check_covariance_matrix(Omega)
      oeli::input_check_response(omega_check, var_name = "Omega")
      P_r <- nrow(Omega)
    } else if (checkmate::test_list(Omega)) {
      if (!length(Omega)) {
        cli::cli_abort(
          "Latent class covariance list {.var Omega} must not be empty.",
          call = NULL
        )
      }
      dims <- vapply(Omega, function(omega_c) {
        omega_check <- oeli::check_covariance_matrix(omega_c)
        if (!isTRUE(omega_check)) {
          cli::cli_abort(
            "Each latent class covariance in {.var Omega} must be a valid
            covariance matrix (problem: {omega_check}).",
            call = NULL
          )
        }
        nrow(omega_c)
      }, integer(1))
      if (length(unique(dims)) != 1L) {
        cli::cli_abort(
          "Latent class covariance matrices in {.var Omega} must share the same
          dimensions.",
          call = NULL
        )
      }
      P_r <- dims[1]
    } else {
      cli::cli_abort(
        "{.var Omega} must be NULL, a covariance matrix, or a list of covariance
        matrices.",
        call = NULL
      )
    }

    ### Sigma is a covariance matrix or a single, non-negative numeric
    oeli::input_check_response(
      check = list(
        oeli::check_covariance_matrix(Sigma),
        checkmate::check_number(Sigma, lower = 0)
      ),
      var_name = "Sigma"
    )

    ### gamma is NULL or a numeric vector sorted in ascending order
    oeli::input_check_response(
      check = oeli::check_numeric_vector(
        gamma, sorted = TRUE, any.missing = FALSE, null.ok = TRUE
      ),
      var_name = "gamma"
    )

    ### weights
    if (!is.null(weights)) {
      oeli::input_check_response(
        check = checkmate::check_numeric(
          weights, lower = 0, any.missing = FALSE, finite = TRUE, min.len = 1
        ),
        var_name = "weights"
      )
      C <- length(weights)
      if (!checkmate::test_list(beta, len = C)) {
        cli::cli_abort(
          "Latent class weights must match the number of coefficient vectors
          supplied in {.var beta}.",
          call = NULL
        )
      }
      if (checkmate::test_list(Omega) && length(Omega) != C) {
        cli::cli_abort(
          "Latent class weights must match the number of covariance matrices
          supplied in {.var Omega}.",
          call = NULL
        )
      }
      weight_sum <- sum(weights)
      if (weight_sum <= 0) {
        cli::cli_abort("Latent class weights must sum to a positive value.",
                       call = NULL)
      }
      if (!isTRUE(all.equal(weight_sum, 1))) {
        result$weights <- weights / weight_sum
        cli::cli_warn(
          "Latent class weights did not sum to one and were normalized.",
          call = NULL
        )
      }
    }

    ### re_position
    if (P_r > 0) {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          re_position, len = P_r, any.missing = FALSE, lower = 1
        ),
        var_name = "re_position"
      )
      re_position <- as.integer(re_position)
      if (length(unique(re_position)) != P_r) {
        cli::cli_abort(
          "Random effect positions in {.var re_position} must be unique.",
          call = NULL
        )
      }
      if (checkmate::test_list(beta)) {
        for (c in seq_along(beta_lengths)) {
          if (beta_lengths[c] < max(re_position)) {
            cli::cli_abort(
              "Random effect positions in {.var re_position} must not exceed the
              coefficient length in latent class {c}.",
              call = NULL
            )
          }
        }
      } else if (beta_dim < max(re_position)) {
        cli::cli_abort(
          "Random effect positions in {.var re_position} must not exceed the
          length of {.var beta}.",
          call = NULL
        )
      }
    }

    ### gcdf
    oeli::input_check_response(
      check = checkmate::check_function(gcdf, args = c("upper", "corr")),
      var_name = "gcdf"
    )
    gcdf_out <- try(
      do.call(gcdf, list("upper" = c(0, 0), "corr" = diag(2))),
      silent = TRUE
    )
    oeli::input_check_response(
      check = checkmate::check_number(gcdf_out, lower = 0, upper = 1),
      var_name = "do.call(gcdf, list(\"upper\" = c(0, 0), \"corr\" = diag(2)))"
    )

    ### lower_bound
    oeli::input_check_response(
      check = checkmate::check_number(lower_bound, finite = TRUE),
      var_name = "lower_bound"
    )

  } else {
    panel_model <- model_type %in% c(12, 13, 14, 28, 29, 30)
    if (panel_model) {
      if (is.null(Tp)) {
        cli::cli_abort("Panel models require {.var Tp} to be supplied.",
                       call = NULL)
      }
      if (!length(Tp) || sum(Tp) != length(X)) {
        cli::cli_abort(
          "Panel models require {.var Tp} whose sum matches the number of
          observations in {.var X}.",
          call = NULL
        )
      }
      if (!isTRUE(all(Tp >= 1))) {
        cli::cli_abort("Panel counts {.var Tp} must be at least one.",
                       call = NULL)
      }
      if (!checkmate::test_choice(cml, choices = c("no", "fp", "ap"))) {
        cli::cli_abort(
          "Composite marginal likelihood specification {.val {cml}} is
          unsupported.",
          call = NULL
        )
      }
    }
  }

  result
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mnp <- function(
    X, y, beta, Sigma,
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0, ranked = FALSE
) {
  N <- length(X)
  J <- dim(Sigma)[1]
  if (is.null(y)) {
    probs_all <- lapply(seq_len(J), function(j) {
      choiceprob_mnp(
        X = X, y = as.list(rep(j, times = N)), beta = beta,
        Sigma = Sigma, gcdf = gcdf, lower_bound = lower_bound, ranked = FALSE
      )
    })
    return(do.call(cbind, probs_all))
  }
  sapply(seq_len(N), function(n) {
    D_n <- if (ranked) {
      oeli::M(ranking = y[[n]], dim = J)
    } else {
      oeli::delta(ref = y[[n]], dim = J)
    }
    upper <- as.numeric(D_n %*% (-X[[n]] %*% beta))
    corr <- stats::cov2cor(as.matrix(D_n %*% Sigma %*% t(D_n)))
    prob_n <- do.call(gcdf, list("upper" = upper, "corr" = corr))
    max(prob_n, lower_bound)
  })
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mnp_ordered <- function(
    X, y, beta, Sigma, gamma, lower_bound = 0
) {
  N <- length(X)
  J <- length(gamma) + 1
  if (is.null(y)) {
    probs_all <- lapply(seq_len(J), function(j) {
      choiceprob_mnp_ordered(
        X = X, y = as.list(rep(j, times = N)), beta = beta,
        Sigma = Sigma, gamma = gamma, lower_bound = lower_bound
      )
    })
    return(do.call(cbind, probs_all))
  }
  gamma_augmented <- c(-Inf, gamma, +Inf)
  sapply(seq_len(N), function(n) {
    V_n <- as.numeric(X[[n]] %*% beta)
    ub <- (gamma_augmented[y[[n]] + 1] - V_n) / sqrt(Sigma)
    lb <- (gamma_augmented[y[[n]]] - V_n) / sqrt(Sigma)
    prob_n <- stats::pnorm(ub) - stats::pnorm(lb)
    max(prob_n, lower_bound)
  })
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp <- function(
    X, y, beta, Omega, Sigma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0, ranked = FALSE
) {
  N <- length(X)
  P <- length(beta)
  J <- dim(Sigma)[1]
  if (is.null(y)) {
    probs_all <- lapply(seq_len(J), function(j) {
      choiceprob_mmnp(
        X = X, y = as.list(rep(j, times = N)), beta = beta, Omega = Omega,
        Sigma = Sigma, re_position = re_position, gcdf = gcdf,
        lower_bound = lower_bound, ranked = FALSE
      )
    })
    return(do.call(cbind, probs_all))
  }
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  sapply(seq_len(N), function(n) {
    choiceprob_mnp(
      X = list(X[[n]]),
      y = list(y[[n]]),
      beta = beta,
      Sigma = X[[n]] %*% Omega_completed %*% t(X[[n]]) + Sigma,
      gcdf = gcdf,
      lower_bound = lower_bound,
      ranked = ranked
    )
  })
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_ordered <- function(
    X, y, beta, Omega, Sigma, gamma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)), lower_bound = 0
) {
  N <- length(X)
  P <- length(beta)
  J <- length(gamma) + 1
  if (is.null(y)) {
    probs_all <- lapply(seq_len(J), function(j) {
      choiceprob_mmnp_ordered(
        X = X, y = as.list(rep(j, times = N)), beta = beta, Omega = Omega,
        Sigma = Sigma, gamma = gamma, re_position = re_position,
        lower_bound = lower_bound
      )
    })
    return(do.call(cbind, probs_all))
  }
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  gamma_augmented <- c(-Inf, gamma, +Inf)
  sapply(seq_len(N), function(n) {
    V_n <- as.numeric(X[[n]] %*% beta)
    sd <- sqrt(X[[n]] %*% Omega_completed %*% t(X[[n]]) + Sigma)
    ub <- (gamma_augmented[y[[n]] + 1] - V_n) / sd
    lb <- (gamma_augmented[y[[n]]] - V_n) / sd
    prob_n <- stats::pnorm(ub) - stats::pnorm(lb)
    max(prob_n, lower_bound)
  })
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_lc <- function(
    X, y, beta, Omega, Sigma, weights,
    re_position = utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]])),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0, ranked = FALSE
) {
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * choiceprob_mmnp(
      X = X, y = y, beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound,
      ranked = ranked
    )
  }
  Reduce("+", probs)
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_ordered_lc <- function(
    X, y, beta, Omega, Sigma, gamma, weights,
    re_position = utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]])),
    lower_bound = 0
) {
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * choiceprob_mmnp_ordered(
      X = X, y = y, beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
      gamma = gamma, re_position = re_position, lower_bound = lower_bound
    )
  }
  Reduce("+", probs)
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_panel <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0, ranked = FALSE
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
  probabilities <- numeric(N)
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
      lower_bound = lower_bound,
      ranked = ranked,
      cml_type = cml_type
    )
    probabilities[n] <- prob_n
  }
  probabilities
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_ordered_panel <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, gamma,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0
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
  probabilities <- numeric(N)
  for (n in seq_len(N)) {
    ind_n <- (csTp[n] + 1):(csTp[n + 1])
    if (length(ind_n) == 1) {
      prob_n <- choiceprob_mmnp_ordered(
        X = X[ind_n], y = y[ind_n], beta = beta, Omega = Omega,
        Sigma = Sigma, gamma = gamma, re_position = re_position,
        lower_bound = lower_bound
      )
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
        lower_bound = lower_bound,
        cml_type = cml_type
      )
    }
    probabilities[n] <- prob_n
  }
  probabilities
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_panel_lc <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, weights,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0, ranked = FALSE
) {
  C <- length(weights)
  probs <- list()
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * choiceprob_mmnp_panel(
      X = X, y = y, Tp = Tp, cml = cml,
      beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound,
      ranked = ranked
    )
  }
  Reduce("+", probs)
}

#' @rdname choiceprob_probit
#' @export

choiceprob_mmnp_ordered_panel_lc <- function(
    X, y,
    Tp, cml,
    beta, Omega, Sigma, gamma, weights,
    re_position = utils::tail(seq_along(beta), nrow(Omega)),
    gcdf = pmvnorm_cdf_default,
    lower_bound = 0
) {
  C <- length(weights)
  probs <- list()
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * choiceprob_mmnp_ordered_panel(
      X = X, y = y, Tp = Tp, cml = cml,
      beta = beta[[c]], Omega = Omega[[c]], Sigma = Sigma, gamma = gamma,
      re_position = re_position, gcdf = gcdf, lower_bound = lower_bound
    )
  }
  Reduce("+", probs)
}

#' Calculate logit choice probabilities
#'
#' @description
#' These helper functions compute logit choice probabilities for unordered and
#' ordered outcomes. Panel inputs reuse the observation-level logit formulae,
#' which remain valid because the logit error term is independent across
#' occasions. Latent class models are supported via weighted averages of
#' class-specific probabilities. When `Omega` is supplied, the coefficients are
#' assumed to follow a multivariate normal distribution and the resulting
#' probabilities are evaluated by averaging over simulation draws.
#'
#' @inheritParams choiceprob_probit
#' @param weights \[`NULL` | `numeric()`\]\cr
#'   Optional class weights for latent class specifications.
#' @param draws \[`NULL` | `matrix` | `list`\]\cr
#'   Optional simulation draws for the random coefficients when `Omega` is not
#'   `NULL`. A matrix provides shared draws for all classes; a list can supply
#'   class-specific draw matrices.
#' @param n_draws \[`integer(1)`\]\cr
#'   Number of draws to generate when `draws` is `NULL` and `Omega` is provided.
#'
#' @param ordered,ranked,panel,lc \[`logical(1)`\]\cr
#'   Flags indicating whether the specification is ordered, ranked, panel, or
#'   latent class. These defaults are inferred from the other inputs so callers
#'   typically do not need to override them.
#'
#' @return
#' A numeric vector with the choice probabilities for the observed choices when
#' `y` is supplied. If `y` is `NULL`, a matrix with one row per observation and
#' one column per alternative is returned.
#'
#' @keywords probability
#'
#' @export

choiceprob_logit <- function(
    X, y = NULL, Tp = NULL, beta, Omega = NULL, gamma = NULL,
    weights = NULL, input_checks = TRUE,
    ordered = !is.null(gamma),
    ranked = !ordered && !is.null(y) && length(y) > 0 && length(y[[1]]) > 1,
    panel = !is.null(Tp) && any(Tp > 1),
    lc = !is.null(weights),
    draws = NULL,
    n_draws = 200
  ) {

  if (isTRUE(input_checks)) {
    choiceprob_logit_input_checks(
      X = X, y = y, Tp = Tp, beta = beta, Omega = Omega, gamma = gamma,
      weights = weights, ordered = ordered, ranked = ranked,
      panel = panel, lc = lc, draws = draws, n_draws = n_draws
    )
  }

  if (ordered && ranked) {
    cli::cli_abort(
      "Ranked outcomes are not supported for ordered logit models.",
      call = NULL
    )
  }

  compute_base_probabilities <- function(beta_values) {
    if (!is.null(y) && isTRUE(panel)) {
      if (ordered) {
        choiceprob_mnl_ordered_panel(
          X = X, y = y, beta = beta_values, gamma = gamma, Tp = Tp
        )
      } else {
        choiceprob_mnl_panel(
          X = X, y = y, beta = beta_values, Tp = Tp, ranked = ranked
        )
      }
    } else if (ordered) {
      choiceprob_mnl_ordered(
        X = X, y = y, beta = beta_values, gamma = gamma
      )
    } else {
      choiceprob_mnl(
        X = X, y = y, beta = beta_values, ranked = ranked
      )
    }
  }

  if (lc) {
    lc_panel <- !is.null(y) && isTRUE(panel)
    if (!is.null(Omega)) {
      re_position <- utils::tail(seq_along(beta[[1]]), nrow(Omega[[1]]))
      if (lc_panel) {
        return(choiceprob_mmnl_panel_lc(
          X = X, y = y, Tp = Tp, beta = beta, Omega = Omega,
          weights = weights, re_position = re_position, ranked = ranked,
          draws = draws, n_draws = n_draws
        ))
      }
      if (ordered) {
        return(choiceprob_mmnl_ordered_lc(
          X = X, y = y, beta = beta, Omega = Omega, gamma = gamma,
          weights = weights, re_position = re_position,
          draws = draws, n_draws = n_draws
        ))
      }
      return(choiceprob_mmnl_lc(
        X = X, y = y, beta = beta, Omega = Omega, weights = weights,
        re_position = re_position, ranked = ranked,
        draws = draws, n_draws = n_draws
      ))
    }
    if (ordered) {
      return(choiceprob_mnl_ordered_lc(
        X = X, y = y, beta = beta, gamma = gamma, weights = weights,
        panel = lc_panel, Tp = if (lc_panel) Tp else NULL
      ))
    }
    return(choiceprob_mnl_lc(
      X = X, y = y, beta = beta, weights = weights,
      ranked = ranked, panel = lc_panel, Tp = if (lc_panel) Tp else NULL
    ))
  }

  if (!is.null(Omega)) {
    re_position <- utils::tail(seq_along(beta), nrow(Omega))
    if (!is.null(y) && isTRUE(panel)) {
      return(choiceprob_mmnl_panel(
        X = X, y = y, Tp = Tp, beta = beta, Omega = Omega,
        re_position = re_position, ranked = ranked,
        draws = draws, n_draws = n_draws
      ))
    }
    if (ordered) {
      return(choiceprob_mmnl_ordered(
        X = X, y = y, beta = beta, Omega = Omega, gamma = gamma,
        re_position = re_position, draws = draws, n_draws = n_draws
      ))
    }
    return(choiceprob_mmnl(
      X = X, y = y, beta = beta, Omega = Omega,
      re_position = re_position, ranked = ranked,
      draws = draws, n_draws = n_draws
    ))
  }

  compute_base_probabilities(beta)
}

#' @noRd

choiceprob_logit_input_checks <- function(
    X, y, Tp, beta, Omega, gamma, weights, ordered, ranked, panel, lc,
    draws, n_draws
  ) {

  oeli::input_check_response(
    check = checkmate::check_list(X),
    var_name = "X"
  )

  if (panel) {
    oeli::input_check_response(
      check = checkmate::check_integer(Tp, lower = 1),
      var_name = "Tp"
    )
    if (!is.null(y)) {
      oeli::input_check_response(
        check = checkmate::check_list(y, len = sum(Tp)),
        var_name = "y"
      )
    }
  } else if (!is.null(y)) {
    oeli::input_check_response(
      check = checkmate::check_list(y, len = length(X)),
      var_name = "y"
    )
  }

  if (ordered) {
    oeli::input_check_response(
      check = oeli::check_numeric_vector(gamma, any.missing = FALSE),
      var_name = "gamma"
    )
  }

  if (lc) {
    oeli::input_check_response(
      check = checkmate::check_numeric(weights, lower = 0, finite = TRUE),
      var_name = "weights"
    )
    if (!checkmate::test_list(beta)) {
      cli::cli_abort(
        "Latent class logit probabilities require class-specific coefficient
        lists.",
        call = NULL
      )
    }
    if (length(beta) != length(weights)) {
      cli::cli_abort(
        "Number of coefficient vectors and class weights must match.",
        call = NULL
      )
    }
    if (!is.null(Omega)) {
      if (!checkmate::test_list(Omega, len = length(weights))) {
        cli::cli_abort(
          "Latent class random effects {.var Omega} must be a list matching the
          number of classes.",
          call = NULL
        )
      }
      dims <- vapply(Omega, function(omega_c) {
        oeli::input_check_response(
          check = oeli::check_covariance_matrix(omega_c),
          var_name = "Omega"
        )
        nrow(omega_c)
      }, integer(1))
      if (length(unique(dims)) != 1L) {
        cli::cli_abort(
          "Latent class covariance matrices in {.var Omega} must share the same
          dimensions.",
          call = NULL
        )
      }
      if (!is.null(draws)) {
        if (checkmate::test_list(draws)) {
          if (length(draws) != length(weights)) {
            cli::cli_abort(
              "Latent class draws must match the number of classes.",
              call = NULL
            )
          }
          for (c in seq_along(draws)) {
            oeli::input_check_response(
              check = checkmate::check_matrix(draws[[c]], ncols = dims[c]),
              var_name = "draws"
            )
          }
        } else {
          oeli::input_check_response(
            check = checkmate::check_matrix(draws, ncols = dims[1]),
            var_name = "draws"
          )
        }
      } else {
        oeli::input_check_response(
          check = checkmate::check_int(n_draws, lower = 1),
          var_name = "n_draws"
        )
      }
    }
  } else {
    oeli::input_check_response(
      check = oeli::check_numeric_vector(beta, any.missing = FALSE),
      var_name = "beta"
    )
    if (!is.null(Omega)) {
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(Omega),
        var_name = "Omega"
      )
      if (!is.null(draws)) {
        oeli::input_check_response(
          check = checkmate::check_matrix(draws, ncols = nrow(Omega)),
          var_name = "draws"
        )
      } else {
        oeli::input_check_response(
          check = checkmate::check_int(n_draws, lower = 1),
          var_name = "n_draws"
        )
      }
    }
  }

  if (!is.null(y) && !ordered) {
    lengths_y <- vapply(y, length, numeric(1))
    if (ranked && any(lengths_y < 2)) {
      cli::cli_abort(
        "Ranked outcomes require at least two ranks per observation.",
        call = NULL
      )
    }
  }
}

#' @noRd

choiceprob_mmnl <- function(
    X, y, beta, Omega, re_position, ranked = FALSE,
    draws = NULL, n_draws = 200
  ) {
  draws_mat <- prepare_mixed_logit_draws(draws, n_draws, Omega)
  average_over_draws(
    draws = draws_mat,
    beta = beta,
    re_position = re_position,
    compute_fun = function(beta_draw) {
      choiceprob_mnl(
        X = X, y = y, beta = beta_draw, ranked = ranked
      )
    }
  )
}

#' @noRd

choiceprob_mmnl_ordered <- function(
    X, y, beta, Omega, gamma, re_position,
    draws = NULL, n_draws = 200
  ) {
  draws_mat <- prepare_mixed_logit_draws(draws, n_draws, Omega)
  average_over_draws(
    draws = draws_mat,
    beta = beta,
    re_position = re_position,
    compute_fun = function(beta_draw) {
      choiceprob_mnl_ordered(
        X = X, y = y, beta = beta_draw, gamma = gamma
      )
    }
  )
}

#' @noRd

choiceprob_mmnl_panel <- function(
    X, y, Tp, beta, Omega, re_position, ranked = FALSE,
    draws = NULL, n_draws = 200
  ) {
  draws_mat <- prepare_mixed_logit_draws(draws, n_draws, Omega)
  average_over_draws(
    draws = draws_mat,
    beta = beta,
    re_position = re_position,
    compute_fun = function(beta_draw) {
      choiceprob_mnl_panel(
        X = X, y = y, beta = beta_draw, Tp = Tp, ranked = ranked
      )
    }
  )
}

#' @noRd

choiceprob_mmnl_ordered_panel <- function(
    X, y, Tp, beta, Omega, gamma, re_position,
    draws = NULL, n_draws = 200
  ) {
  draws_mat <- prepare_mixed_logit_draws(draws, n_draws, Omega)
  average_over_draws(
    draws = draws_mat,
    beta = beta,
    re_position = re_position,
    compute_fun = function(beta_draw) {
      choiceprob_mnl_ordered_panel(
        X = X, y = y, beta = beta_draw, gamma = gamma, Tp = Tp
      )
    }
  )
}

#' @noRd

choiceprob_mmnl_lc <- function(
    X, y, beta, Omega, weights, re_position, ranked = FALSE,
    draws = NULL, n_draws = 200
  ) {
  draw_list <- prepare_lc_draws(draws, n_draws, Omega)
  probs <- vector("list", length = length(weights))
  for (c in seq_along(weights)) {
    probs[[c]] <- weights[c] * choiceprob_mmnl(
      X = X, y = y, beta = beta[[c]], Omega = Omega[[c]],
      re_position = re_position, ranked = ranked,
      draws = draw_list[[c]], n_draws = nrow(draw_list[[c]])
    )
  }
  Reduce("+", probs)
}

#' @noRd

choiceprob_mmnl_ordered_lc <- function(
    X, y, beta, Omega, gamma, weights, re_position,
    draws = NULL, n_draws = 200
  ) {
  draw_list <- prepare_lc_draws(draws, n_draws, Omega)
  probs <- vector("list", length = length(weights))
  for (c in seq_along(weights)) {
    probs[[c]] <- weights[c] * choiceprob_mmnl_ordered(
      X = X, y = y, beta = beta[[c]], Omega = Omega[[c]], gamma = gamma,
      re_position = re_position,
      draws = draw_list[[c]], n_draws = nrow(draw_list[[c]])
    )
  }
  Reduce("+", probs)
}

#' @noRd

choiceprob_mmnl_panel_lc <- function(
    X, y, Tp, beta, Omega, weights, re_position, ranked = FALSE,
    draws = NULL, n_draws = 200
  ) {
  draw_list <- prepare_lc_draws(draws, n_draws, Omega)
  probs <- vector("list", length = length(weights))
  for (c in seq_along(weights)) {
    probs[[c]] <- weights[c] * choiceprob_mmnl_panel(
      X = X, y = y, Tp = Tp, beta = beta[[c]], Omega = Omega[[c]],
      re_position = re_position, ranked = ranked,
      draws = draw_list[[c]], n_draws = nrow(draw_list[[c]])
    )
  }
  Reduce("+", probs)
}

#' @noRd

choiceprob_mmnl_ordered_panel_lc <- function(
    X, y, Tp, beta, Omega, gamma, weights, re_position,
    draws = NULL, n_draws = 200
  ) {
  draw_list <- prepare_lc_draws(draws, n_draws, Omega)
  probs <- vector("list", length = length(weights))
  for (c in seq_along(weights)) {
    probs[[c]] <- weights[c] * choiceprob_mmnl_ordered_panel(
      X = X, y = y, Tp = Tp, beta = beta[[c]], Omega = Omega[[c]],
      gamma = gamma, re_position = re_position,
      draws = draw_list[[c]], n_draws = nrow(draw_list[[c]])
    )
  }
  Reduce("+", probs)
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
    draw_mat <- as.matrix(draws)
    if (nrow(draw_mat) == 0) {
      cli::cli_abort(
        "At least one draw is required to evaluate mixed logit probabilities.",
        call = NULL
      )
    }
    return(draw_mat)
  }
  n <- as.integer(n_draws)
  draw_mat <- mvtnorm::rmvnorm(n = n, sigma = Omega)
  if (n == 1) {
    draw_mat <- matrix(draw_mat, nrow = 1)
  }
  draw_mat
}

#' @noRd

prepare_lc_draws <- function(draws, n_draws, Omega_list) {
  if (is.null(draws)) {
    lapply(Omega_list, function(omega_c) {
      prepare_mixed_logit_draws(NULL, n_draws, omega_c)
    })
  } else if (checkmate::test_list(draws)) {
    lapply(seq_along(draws), function(idx) {
      draw_mat <- as.matrix(draws[[idx]])
      if (nrow(draw_mat) == 0) {
        cli::cli_abort(
          "At least one draw is required to evaluate mixed logit
          probabilities.",
          call = NULL
        )
      }
      draw_mat
    })
  } else {
    shared_draws <- prepare_mixed_logit_draws(draws, n_draws, Omega_list[[1]])
    replicate(length(Omega_list), shared_draws, simplify = FALSE)
  }
}

#' @noRd

average_over_draws <- function(draws, beta, re_position, compute_fun) {
  n_draws <- nrow(draws)
  result <- NULL
  for (r in seq_len(n_draws)) {
    beta_draw <- beta
    beta_draw[re_position] <- beta_draw[re_position] + draws[r, , drop = TRUE]
    draw_prob <- compute_fun(beta_draw)
    if (is.null(result)) {
      result <- draw_prob
    } else {
      result <- result + draw_prob
    }
  }
  result / n_draws
}

#' @noRd

choiceprob_mnl <- function(X, y, beta, ranked = FALSE) {
  N <- length(X)
  if (is.null(y)) {
    prob_list <- lapply(seq_len(N), function(n) {
      compute_logit_probabilities(X[[n]] %*% beta)
    })
    return(do.call(rbind, prob_list))
  }

  sapply(seq_len(N), function(n) {
    utilities <- X[[n]] %*% beta
    if (ranked) {
      compute_ranked_logit_probability(utilities, y[[n]])
    } else {
      prob_vec <- compute_logit_probabilities(utilities)
      prob_vec[y[[n]]]
    }
  })
}

#' @noRd

choiceprob_mnl_ordered <- function(X, y, beta, gamma) {
  N <- length(X)
  gamma_augmented <- c(-Inf, gamma, +Inf)
  if (is.null(y)) {
    prob_list <- lapply(seq_len(N), function(n) {
      V_n <- as.numeric(X[[n]] %*% beta)
      diff(stats::plogis(gamma_augmented - V_n))
    })
    return(do.call(rbind, prob_list))
  }

  sapply(seq_len(N), function(n) {
    V_n <- as.numeric(X[[n]] %*% beta)
    ub <- gamma_augmented[y[[n]] + 1] - V_n
    lb <- gamma_augmented[y[[n]]] - V_n
    stats::plogis(ub) - stats::plogis(lb)
  })
}

#' @noRd

choiceprob_mnl_panel <- function(X, y, beta, Tp, ranked = FALSE) {
  N <- length(Tp)
  csTp <- c(0, cumsum(Tp))
  sapply(seq_len(N), function(n) {
    ind_n <- (csTp[n] + 1):(csTp[n + 1])
    obs_probs <- choiceprob_mnl(
      X = X[ind_n], y = y[ind_n], beta = beta, ranked = ranked
    )
    prod(obs_probs)
  })
}

#' @noRd

choiceprob_mnl_ordered_panel <- function(X, y, beta, gamma, Tp) {
  N <- length(Tp)
  csTp <- c(0, cumsum(Tp))
  sapply(seq_len(N), function(n) {
    ind_n <- (csTp[n] + 1):(csTp[n + 1])
    obs_probs <- choiceprob_mnl_ordered(
      X = X[ind_n], y = y[ind_n], beta = beta, gamma = gamma
    )
    prod(obs_probs)
  })
}

#' @noRd

choiceprob_mnl_lc <- function(
    X, y, beta, weights, ranked = FALSE, panel = FALSE, Tp = NULL
  ) {
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * if (panel) {
      choiceprob_mnl_panel(
        X = X, y = y, beta = beta[[c]], Tp = Tp, ranked = ranked
      )
    } else {
      choiceprob_mnl(
        X = X, y = y, beta = beta[[c]], ranked = ranked
      )
    }
  }
  Reduce("+", probs)
}

#' @noRd

choiceprob_mnl_ordered_lc <- function(
    X, y, beta, gamma, weights, panel = FALSE, Tp = NULL
  ) {
  C <- length(weights)
  probs <- vector("list", length = C)
  for (c in seq_len(C)) {
    probs[[c]] <- weights[c] * if (panel) {
      choiceprob_mnl_ordered_panel(
        X = X, y = y, beta = beta[[c]], gamma = gamma, Tp = Tp
      )
    } else {
      choiceprob_mnl_ordered(
        X = X, y = y, beta = beta[[c]], gamma = gamma
      )
    }
  }
  Reduce("+", probs)
}

#' @noRd

compute_logit_probabilities <- function(utilities) {
  utilities <- as.numeric(utilities)
  centered <- utilities - max(utilities)
  exp_val <- exp(centered)
  exp_val / sum(exp_val)
}

#' @noRd

compute_ranked_logit_probability <- function(utilities, ranking) {
  ranking <- as.integer(ranking)
  available <- seq_along(utilities)
  prob <- 1
  for (pos in seq_along(ranking)) {
    choice <- ranking[pos]
    prob_vec <- compute_logit_probabilities(utilities[available])
    idx <- match(choice, available)
    prob <- prob * prob_vec[idx]
    available <- available[-idx]
  }
  prob
}
