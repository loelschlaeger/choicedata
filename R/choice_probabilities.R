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
#' A `choice_probabilities` tibble. With `choice_only = TRUE`, it contains one
#' row per occasion and a `choice_probability` column. A joint panel
#' probability is repeated over the observed occasions of its decider; missing
#' responses receive `NA`.
#'
#' With `choice_only = FALSE`, non-ranked cross-sectional models contain one
#' row per occasion and one column per alternative. An unavailable alternative
#' has probability zero. Ranked and panel models instead contain one row per
#' possible outcome, an `outcome` list-column, and `choice_probability`. These
#' rows cover complete or partial rankings and complete joint panel sequences,
#' and their probabilities sum to one per decider. Enumerating them can grow
#' combinatorially with ranking depth and panel length.
#' For a missing ranked response, a common observed depth of the same decider
#' is reused; without one, all available alternatives are ranked.
#'
#' @section Supported models:
#' The public API supports every combination of Logit or Probit errors, fixed,
#' correlated normal, or correlated log-normal coefficients, discrete,
#' ordered, or ranked responses, and cross-sectional or panel data. It also
#' supports latent classes with or without random effects. Probit panels can
#' use the full likelihood (`cml = "no"`), full pairwise CML (`"fp"`), or
#' adjacent pairwise CML (`"ap"`). All-outcome output always uses the full
#' likelihood.
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
  cross_section = is.null(column_occasion),
  column_probabilities = if (choice_only) "choice_probability"
) {

  # Validate values before attaching probability metadata.
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
    oeli::input_check_response(
      check = checkmate::check_numeric(
        data_frame[[column]], lower = 0, upper = 1,
        finite = TRUE, any.missing = choice_only
      ),
      var_name = column
    )
  }
  if (!choice_only) {
    sums <- rowSums(data_frame[column_probabilities])
    if (any(abs(sums - 1) > sqrt(.Machine$double.eps))) {
      cli::cli_abort(
        "Alternative probabilities must sum to one in every row.",
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

  # Preserve identifier metadata on the result.
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
    choice_only = choice_only
  )

}

#' @rdname choice_probabilities
#'
#' @param choice_parameters \[`choice_parameters` | `numeric()`\]\cr
#' Either a \code{\link{choice_parameters}} object or a numeric vector in
#' optimization space, as created by \code{\link{switch_parameter_space}}.
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
#' Additional probability arguments. Common choices are `draws` or `n_draws`
#' for simulated mixed models and `cml = "no"`, `"fp"`, or `"ap"` for Probit
#' panels. Supplied draws are standard normal and are transformed using the
#' current random-effect covariance matrix.
#'
#' @export

compute_choice_probabilities <- function(
  choice_parameters,
  choice_data,
  choice_effects,
  choice_only = TRUE,
  input_checks = TRUE,
  ...
) {

  # Convert optimization vectors before validating public inputs.
  if (!is.list(choice_parameters)) {
    choice_parameters <- switch_parameter_space(
      choice_parameters = choice_parameters,
      choice_effects = choice_effects
    )
  }
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  check_choice_only(choice_only)
  oeli::input_check_response(
    check = checkmate::check_flag(input_checks),
    var_name = "input_checks"
  )
  choice_parameters <- validate_choice_parameters(
    choice_parameters,
    choice_effects,
    allow_missing = FALSE
  )

  # Build shared model inputs once for every probability path.
  choice_identifiers <- extract_choice_identifiers(choice_data)
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

  ranked <- identical(attr(choice_data, "choice_type"), "ranked")
  # Joint output enumerates sequences instead of marginal occasions.
  joint_outcomes <- !isTRUE(choice_only) &&
    (ranked || any(attr(design_list, "Tp") > 1L))
  if (joint_outcomes) {
    return(evaluate_choice_outcomes(
      design_list = design_list,
      choice_identifiers = choice_identifiers,
      choice_effects = choice_effects,
      choice_parameters = choice_parameters,
      choice_indices = choice_indices,
      ...
    ))
  }

  # Keep missing responses as NA in public choice-only output.
  observed <- lengths(choice_indices) > 0L
  all_identifiers <- NULL
  if (isTRUE(choice_only) && any(!observed)) {
    all_identifiers <- choice_identifiers
    if (any(observed)) {
      design_class <- class(design_list)
      alternatives <- attr(design_list, "alternatives")
      availability <- attr(design_list, "availability")
      choice_type <- attr(design_list, "choice_type")
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
  }

  # Delegate observed outcomes to the common model dispatcher.
  probabilities <- if (is.null(all_identifiers) || any(observed)) {
    evaluate_choice_probabilities(
      design_list = design_list,
      choice_identifiers = choice_identifiers,
      choice_effects = choice_effects,
      choice_parameters = choice_parameters,
      choice_only = choice_only,
      choice_indices = choice_indices,
      ranked = ranked,
      input_checks = input_checks,
      ...
    )
  }
  if (is.null(all_identifiers)) {
    return(probabilities)
  }
  probability <- rep(NA_real_, nrow(all_identifiers))
  if (any(observed)) {
    probability[observed] <- probabilities$choice_probability
  }
  choice_probabilities(
    data_frame = cbind(
      all_identifiers,
      choice_probability = probability
    ),
    choice_only = TRUE,
    column_decider = attr(all_identifiers, "column_decider"),
    column_occasion = attr(all_identifiers, "column_occasion"),
    cross_section = attr(all_identifiers, "cross_section")
  )
}

#' @noRd

evaluate_choice_outcomes <- function(
  design_list, choice_identifiers, choice_effects, choice_parameters,
  choice_indices, ...
) {

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
    identical(error_term, "logit") || any(mixing != "cn")
  )
  if (needs_draws && is.null(dots$draws)) {
    n_draws <- if (is.null(dots$n_draws)) 200L else dots$n_draws
    oeli::input_check_response(
      check = checkmate::check_int(n_draws, lower = 1),
      var_name = "n_draws"
    )
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

  # Enumerate feasible joint outcomes separately for each decider.
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
      # Fixed coefficients allow occasion probabilities to be cached.
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
    # Normalize only after checking the numerical integration error.
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
  out$choice_probability <- prob_all
  out <- tibble::as_tibble(out)
  structure(
    out,
    class = tibble_class("choice_probabilities", class(out)),
    column_decider = column_decider,
    column_occasion = NULL,
    cross_section = attr(choice_identifiers, "cross_section"),
    column_probabilities = "choice_probability",
    column_outcome = "outcome",
    choice_only = FALSE,
    joint = TRUE
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
    ranked = identical(attr(design_list, "choice_type"), "ranked"),
    ...
  ) {

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
  joint_panel <- isTRUE(choice_only) && has_panel
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
      weights = choice_parameters$weights,
      availability = availability[eval_order],
      ranked = ranked,
      re_mixing = as.character(stats::na.omit(choice_effects$mixing)),
      input_checks = input_checks,
      logarithm = logarithm
    ),
    list(...)
  )
  if (identical(error_term, "logit") && !is.null(prob_args$cml)) {
    if (!identical(prob_args$cml, "no")) {
      cli::cli_abort(
        "Composite marginal likelihood is only available for Probit.",
        call = NULL
      )
    }
    prob_args$cml <- NULL
  }

  probability <- switch(
    error_term,
    "probit" = {
      fixed <- is.null(choice_parameters$Omega) &&
        is.null(choice_parameters$weights)
      if (joint_panel && fixed) {
        cml <- if (is.null(prob_args$cml)) "no" else prob_args$cml
        cml <- match.arg(cml, c("no", "fp", "ap"))
        cml_type <- match(cml, c("no", "fp", "ap")) - 1L
        prob_args$Tp <- NULL
        prob_args$cml <- NULL
        prob_args$logarithm <- TRUE
        obs_log <- do.call(choiceprob_probit, prob_args)
        pos <- c(0L, cumsum(Tp))
        panel_log <- numeric(length(Tp))
        for (n in seq_along(Tp)) {
          idx <- pos[n] + seq_len(Tp[n])
          chunks <- build_panel_chunks(Tp[n], cml_type)
          chunk_log <- numeric(length(chunks))
          for (k in seq_along(chunks)) {
            chunk_log[k] <- sum(obs_log[idx[chunks[[k]]]])
          }
          panel_log[n] <- sum(chunk_log)
        }
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

  if (isTRUE(numeric_only)) {
    return(as.numeric(probability))
  }

  cross_section <- isTRUE(attr(choice_identifiers, "cross_section"))
  column_occasion <- attr(choice_identifiers, "column_occasion")
  expected_rows <- nrow(choice_identifiers)
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
  D_n <- as.matrix(Matrix::bdiag(lapply(delta_n, `[[`, "D")))
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
#' In the ranked case (`ranked = TRUE`), each entry is a unique sequence of
#' available alternative indices in rank order. Its length determines the
#' observed ranking depth and may be smaller than the choice-set size.
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
#' @param re_mixing [`character(P_r)` | `NULL`\]\cr
#' Random-effect distributions: `"cn"`, `"cln+"`, or `"cln-"`.
#'
#' @param availability [`list(N)` | `NULL`\]\cr
#' Available global alternative indices for each observation. The default uses
#' every row of each design matrix.
#'
#' @param draws [`NULL` | `matrix` | `list`\]\cr
#' Standard normal simulation draws for non-normal random effects. A list can
#' supply class-specific draws.
#'
#' @param n_draws [`integer(1)`\]\cr
#' Number of standard normal draws generated when `draws` is `NULL`.
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
#' In the panel case (`panel = TRUE`), one joint or composite contribution is
#' returned per decider, so the vector has length `length(Tp)`.
#'
#' If `y` is `NULL` and in the non-panel case (`panel = FALSE`), a matrix of
#' dimension `N` times `J`, the probabilities for all alternatives.
#' In the ranked case (`ranked = TRUE`), only first place choice probabilities
#' are computed, which is equivalent to computing choice probabilities in the
#' regular (maximum utility) model.
#'
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
    re_mixing[re_mixing == "cln"] <- "cln+"
    oeli::input_check_response(
      check = checkmate::check_character(
        re_mixing, len = nrow(omega_ref), any.missing = FALSE
      ),
      var_name = "re_mixing"
    )
    if (!all(re_mixing %in% c("cn", "cln+", "cln-"))) {
      cli::cli_abort(
        "Random-effect distributions must be cn, cln+, or cln-.",
        call = NULL
      )
    }
  }

  if (isTRUE(input_checks)) {
    # Validate common inputs before deriving the model flags.
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

  # Encode ordered, ranked, mixed, panel, and class flags as bits.
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
    # Apply checks that depend on the encoded model type.
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

  if (mixed && any(re_mixing != "cn")) {
    return(choiceprob_smnp(
      X = X, y = y, Tp = Tp, cml = cml, beta = beta,
      Omega = Omega, Sigma = Sigma, gamma = gamma,
      weights = weights, re_position = re_position,
      re_mixing = re_mixing, availability = availability,
      draws = draws, n_draws = n_draws, gcdf = gcdf,
      ranked = ranked, panel = panel, logarithm = logarithm
    ))
  }

  # Dispatch once from the encoded model type.
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
      if (logarithm && length(upper) == 1L) {
        probabilities[n] <- stats::pnorm(upper, log.p = TRUE)
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
  N <- length(X)
  J <- length(gamma) + 1
  if (is.null(y)) {
    probs_all <- vector("list", J)
    for (j in seq_len(J)) {
      probs_all[[j]] <- choiceprob_mnp_ordered(
        X = X, y = as.list(rep(j, times = N)), beta = beta,
        Sigma = Sigma, gamma = gamma, logarithm = FALSE
      )
    }
    probs_all <- do.call(cbind, probs_all)
    return(probs_all / rowSums(probs_all))
  }
  gamma_augmented <- c(-Inf, gamma, +Inf)
  probabilities <- numeric(N)
  for (n in seq_len(N)) {
    V_n <- as.numeric(X[[n]] %*% beta)
    ub <- (gamma_augmented[y[[n]] + 1] - V_n) / sqrt(Sigma)
    lb <- (gamma_augmented[y[[n]]] - V_n) / sqrt(Sigma)
    if (logarithm) {
      if (lb > 0) {
        log_large <- stats::pnorm(lb, lower.tail = FALSE, log.p = TRUE)
        log_small <- stats::pnorm(ub, lower.tail = FALSE, log.p = TRUE)
      } else {
        log_large <- stats::pnorm(ub, log.p = TRUE)
        log_small <- stats::pnorm(lb, log.p = TRUE)
      }
      log_prob <- log_large + log1p(-exp(log_small - log_large))
      probabilities[n] <- if (is.finite(log_prob)) {
        log_prob
      } else {
        log(.Machine$double.xmin)
      }
    } else {
      prob_n <- stats::pnorm(ub) - stats::pnorm(lb)
      probabilities[n] <- max(prob_n, 0)
    }
  }
  probabilities
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
      if (logarithm && length(upper) == 1L) {
        probabilities[n] <- stats::pnorm(upper, log.p = TRUE)
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
  N <- length(X)
  P <- length(beta)
  J <- length(gamma) + 1
  if (is.null(y)) {
    probs_all <- vector("list", J)
    for (j in seq_len(J)) {
      probs_all[[j]] <- choiceprob_mmnp_ordered(
        X = X, y = as.list(rep(j, times = N)), beta = beta, Omega = Omega,
        Sigma = Sigma, gamma = gamma, re_position = re_position,
        logarithm = FALSE
      )
    }
    probs_all <- do.call(cbind, probs_all)
    return(probs_all / rowSums(probs_all))
  }
  Omega_completed <- matrix(0, P, P)
  Omega_completed[re_position, re_position] <- Omega
  gamma_augmented <- c(-Inf, gamma, +Inf)
  probabilities <- numeric(N)
  for (n in seq_len(N)) {
    V_n <- as.numeric(X[[n]] %*% beta)
    sd <- sqrt(X[[n]] %*% Omega_completed %*% t(X[[n]]) + Sigma)
    ub <- (gamma_augmented[y[[n]] + 1] - V_n) / sd
    lb <- (gamma_augmented[y[[n]]] - V_n) / sd
    if (logarithm) {
      if (lb > 0) {
        log_large <- stats::pnorm(lb, lower.tail = FALSE, log.p = TRUE)
        log_small <- stats::pnorm(ub, lower.tail = FALSE, log.p = TRUE)
      } else {
        log_large <- stats::pnorm(ub, log.p = TRUE)
        log_small <- stats::pnorm(lb, log.p = TRUE)
      }
      log_prob <- log_large + log1p(-exp(log_small - log_large))
      probabilities[n] <- if (is.finite(log_prob)) {
        log_prob
      } else {
        log(.Machine$double.xmin)
      }
    } else {
      prob_n <- stats::pnorm(ub) - stats::pnorm(lb)
      probabilities[n] <- max(prob_n, 0)
    }
  }
  probabilities
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
#'   Optional standard normal simulation draws when `Omega` is not `NULL`.
#'   They are transformed using `Omega`. A matrix provides shared draws for all
#'   classes; a list can supply class-specific draw matrices.
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
    # Latent classes reuse the same fixed or mixed Logit kernel.
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
    # Random coefficients are averaged over transformed standard draws.
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
  average_over_draws(
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
  matrix(stats::rnorm(n * dim_random), nrow = n, ncol = dim_random)
}

#' @noRd

prepare_lc_draws <- function(draws, n_draws, Omega_list) {
  if (is.null(draws)) {
    shared <- prepare_mixed_logit_draws(NULL, n_draws, Omega_list[[1]])
    replicate(length(Omega_list), shared, simplify = FALSE)
  } else if (checkmate::test_list(draws)) {
    draw_list <- vector("list", length(draws))
    for (idx in seq_along(draws)) {
      draw_mat <- as.matrix(draws[[idx]])
      if (nrow(draw_mat) == 0) {
        cli::cli_abort(
          "At least one draw is required to evaluate mixed logit
          probabilities.",
          call = NULL
        )
      }
      draw_list[[idx]] <- draw_mat
    }
    draw_list
  } else {
    shared_draws <- prepare_mixed_logit_draws(draws, n_draws, Omega_list[[1]])
    replicate(length(Omega_list), shared_draws, simplify = FALSE)
  }
}

#' @noRd

average_over_draws <- function(
  draws, beta, re_position, compute_fun, compute_args, Omega,
  re_mixing = NULL, logarithm = FALSE, chunks = NULL
) {
  if (is.null(re_mixing)) {
    re_mixing <- rep("cn", length(re_position))
  }
  re_mixing[re_mixing == "cln"] <- "cln+"
  type <- match(re_mixing, c("cln-", "cn", "cln+")) - 2L
  oeli::input_check_response(
    check = checkmate::check_integer(
      type, len = length(re_position), any.missing = FALSE
    ),
    var_name = "re_mixing"
  )
  # Scale and aggregate all draws in one native pass.
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
