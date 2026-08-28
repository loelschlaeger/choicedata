# Input helpers return the checked value invisibly or raise an error.

check_allow_missing <- function(allow_missing) {
  check_not_missing(allow_missing)
  check <- checkmate::check_flag(allow_missing)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var allow_missing} is bad: {check}", call = NULL)
  }
  invisible(allow_missing)
}

check_alternatives <- function(alternatives, J = length(alternatives)) {
  check_not_missing(alternatives)
  check_J(J)
  check <- checkmate::check_character(
    alternatives, min.chars = 1, any.missing = FALSE, len = J,
    unique = TRUE
  )
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var alternatives} is bad: {check}", call = NULL)
  }
  invisible(alternatives)
}

check_base <- function(base, alternatives) {
  check_not_missing(base)
  oeli::input_check_response(
    check = checkmate::check_choice(base, choices = alternatives),
    var_name = "base"
  )
  invisible(base)
}

check_cross_section <- function(cross_section) {
  check_not_missing(cross_section)
  oeli::input_check_response(
    check = checkmate::check_flag(cross_section),
    var_name = "cross_section"
  )
  invisible(cross_section)
}

check_choice_only <- function(choice_only) {
  check_not_missing(choice_only)
  oeli::input_check_response(
    check = checkmate::check_flag(choice_only),
    var_name = "choice_only"
  )
  invisible(choice_only)
}

check_choice_class_union <- function(
    x,
    class_names,
    error = TRUE,
    var_name = oeli::variable_name(x)
) {
  check_not_missing(x, var_name = var_name)
  oeli::input_check_response(
    check = lapply(class_names, function(class_name) {
      checkmate::check_class(x, class_name)
    }),
    var_name = var_name,
    error = error
  )
}

check_choice_object <- function(
    x,
    class_name,
    error = FALSE,
    var_name = oeli::variable_name(x)
) {
  check_not_missing(x, var_name = var_name)
  oeli::input_check_response(
    check = checkmate::check_class(x, class_name),
    var_name = var_name,
    error = error
  )
}

check_column_alternative <- function(column_alternative, null.ok = TRUE) {
  check_not_missing(column_alternative)
  check <- checkmate::check_string(
    column_alternative, min.chars = 1, null.ok = null.ok
  )
  if (!isTRUE(check)) {
    cli::cli_abort(
      "Input {.var column_alternative} is bad: {check}", call = NULL
    )
  }
  invisible(column_alternative)
}

check_column_choice <- function(column_choice, null.ok = TRUE) {
  check_not_missing(column_choice)
  check <- checkmate::check_string(
    column_choice, min.chars = 1, null.ok = null.ok
  )
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_choice} is bad: {check}", call = NULL)
  }
  invisible(column_choice)
}

check_column_covariates <- function(
  column_covariates, len = NULL, null.ok = TRUE, var_name = "column_covariates"
) {
  check_not_missing(column_covariates)
  oeli::input_check_response(
    check = checkmate::check_character(
      column_covariates, any.missing = FALSE, len = len, null.ok = null.ok
    ),
    var_name = var_name
  )
  if (!is.null(column_covariates)) {
    oeli::input_check_response(
      check = checkmate::check_names(column_covariates, type = "strict"),
      var_name = var_name
    )
  }
  invisible(column_covariates)
}

check_column_decider <- function(column_decider, null.ok = TRUE) {
  check_not_missing(column_decider)
  oeli::input_check_response(
    check = checkmate::check_string(
      column_decider, min.chars = 1, null.ok = null.ok
    ),
    var_name = "column_decider"
  )
  invisible(column_decider)
}

check_column_occasion <- function(
    column_occasion, column_decider, null.ok = TRUE
  ) {
  check_not_missing(column_occasion)
  column_decider <- check_column_decider(column_decider)
  oeli::input_check_response(
    check = checkmate::check_string(
      column_occasion, min.chars = 1, null.ok = null.ok
    ),
    var_name = "column_occasion"
  )
  if (identical(column_decider, column_occasion)) {
    cli::cli_abort(
      "Inputs {.var column_decider} and {.var column_occasion} must be
      different",
      call = NULL
    )
  }
  invisible(column_occasion)
}

check_column_roles <- function(...) {
  columns <- list(...)
  values <- unlist(columns, use.names = TRUE)
  if (!length(values)) return(invisible(values))
  duplicated_values <- unique(values[
    duplicated(values) | duplicated(values, fromLast = TRUE)
  ])
  if (length(duplicated_values)) {
    assignments <- vapply(duplicated_values, function(value) {
      paste(names(values)[values == value], collapse = ", ")
    }, character(1))
    cli::cli_abort(
      "Column role(s) must use distinct columns: {paste0(duplicated_values,
      ' (', assignments, ')', collapse = ', ')}.",
      call = NULL
    )
  }
  invisible(values)
}

check_column_probabilities <- function(
    column_probabilities, len = NULL, null.ok = TRUE
  ) {
  check_not_missing(column_probabilities)
  oeli::input_check_response(
    check = checkmate::check_character(
      column_probabilities, any.missing = FALSE, len = len, null.ok = null.ok
    ),
    var_name = "column_probabilities"
  )
  if (!is.null(column_probabilities)) {
    oeli::input_check_response(
      check = checkmate::check_names(column_probabilities, type = "strict"),
      var_name = "column_probabilities"
    )
  }
  invisible(column_probabilities)
}

check_data_frame <- function(
    data_frame,
    required_columns = character(),
    forbidden_columns = character(),
    allow_missing_columns = character()
  ) {
  check_not_missing(data_frame)
  checkmate::assert_character(required_columns, null.ok = TRUE)
  checkmate::assert_character(forbidden_columns, null.ok = TRUE)
  checkmate::assert_character(allow_missing_columns, null.ok = TRUE)
  if (inherits(data_frame, "choice_data")) {
    response_column <- attr(data_frame, "column_choice")
    response_columns <- response_column
    if (!is.null(response_column) &&
        identical(attr(data_frame, "choice_type"), "ranked")) {
      delimiter <- attr(data_frame, "delimiter")
      prefix <- paste0(response_column, delimiter)
      response_columns <- c(
        response_columns,
        names(data_frame)[startsWith(names(data_frame), prefix)]
      )
    }
    allow_missing_columns <- union(
      allow_missing_columns,
      response_columns
    )
  }
  oeli::input_check_response(
    check = checkmate::check_data_frame(
      data_frame, min.rows = 1, min.cols = 1
    ),
    var_name = "data_frame"
  )
  checked_columns <- setdiff(names(data_frame), allow_missing_columns)
  if (length(checked_columns)) {
    oeli::input_check_response(
      check = if (anyNA(data_frame[checked_columns])) {
        "Must not have NAs outside response columns"
      } else {
        TRUE
      },
      var_name = "data_frame"
    )
  }
  oeli::input_check_response(
    check = checkmate::check_names(
      colnames(data_frame), must.include = required_columns,
      disjunct.from = forbidden_columns, what = "colnames", type = "unique"
    ),
    var_name = "data_frame"
  )
  numeric_cols <- vapply(data_frame, is.numeric, logical(1))
  for (column in names(data_frame)[numeric_cols]) {
    oeli::input_check_response(
      check = checkmate::check_numeric(
        data_frame[[column]], finite = TRUE,
        any.missing = column %in% allow_missing_columns
      ),
      var_name = column
    )
  }
  invisible(data_frame)
}

check_delimiter <- function(delimiter) {
  check_not_missing(delimiter)
  oeli::input_check_response(
    check = checkmate::check_string(delimiter, min.chars = 1),
    var_name = "delimiter"
  )
  invisible(delimiter)
}

check_error_term <- function(error_term, choices) {
  check_not_missing(error_term)
  oeli::input_check_response(
    check = checkmate::check_choice(error_term, choices = choices),
    var_name = "error_term"
  )
  invisible(error_term)
}

check_format <- function(format, choices = c("wide", "long")) {
  check_not_missing(format)
  check <- checkmate::check_choice(format, choices = choices)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var format} is bad: {check}", call = NULL)
  }
  invisible(format)
}

check_formula <- function(formula) {
  check_not_missing(formula)
  oeli::input_check_response(
    check = checkmate::check_formula(formula),
    var_name = "formula"
  )
  invisible(formula)
}

check_J <- function(J) {
  check_not_missing(J)
  oeli::input_check_response(
    check = checkmate::check_int(J, lower = 2),
    var_name = "J"
  )
  invisible(J)
}

check_N <- function(N) {
  check_not_missing(N)
  oeli::input_check_response(
    check = checkmate::check_int(N, lower = 1),
    var_name = "N"
  )
  invisible(N)
}

check_n_draws <- function(n_draws) {
  check_not_missing(n_draws)
  oeli::input_check_response(
    check = checkmate::check_int(n_draws, lower = 1),
    var_name = "n_draws"
  )
  invisible(n_draws)
}

check_not_missing <- function(x, var_name = oeli::variable_name(x)) {
  if (missing(x)) {
    if (!isTRUE(checkmate::check_string(var_name, min.chars = 1))) {
      var_name <- "x"
    }
    cli::cli_abort("Please specify the input {.var {var_name}}", call = NULL)
  }
  invisible(x)
}

check_ordered <- function(ordered) {
  check_not_missing(ordered)
  oeli::input_check_response(
    check = checkmate::check_flag(ordered),
    var_name = "ordered"
  )
  invisible(ordered)
}

check_random_effects <- function(random_effects, choices) {
  check_not_missing(random_effects)
  oeli::input_check_response(
    check = checkmate::check_character(
      random_effects, any.missing = FALSE, names = "unique"
    ),
    var_name = "random_effects"
  )
  oeli::input_check_response(
    check = checkmate::check_subset(
      random_effects, choices = choices, empty.ok = TRUE
    ),
    var_name = "random_effects"
  )
  invisible(random_effects)
}

check_Tp <- function(Tp, N) {
  check_not_missing(Tp)
  N <- check_N(N)
  oeli::input_check_response(
    check = list(
      checkmate::check_int(Tp, lower = 1),
      checkmate::check_integerish(
        Tp, lower = 1, any.missing = FALSE, len = N
      )
    ),
    var_name = "Tp"
  )
  invisible(Tp)
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
    availability, model_type
) {

  result <- list(
    Tp = Tp, cml = cml, weights = weights, re_position = re_position
  )
  if (is.na(model_type)) {
    ### validate shared structure before applying model-specific checks
    oeli::input_check_response(
      check = checkmate::check_list(X, min.len = 1),
      var_name = "X"
    )

    oeli::input_check_response(
      check = checkmate::check_list(y, len = length(X), null.ok = TRUE),
      var_name = "y"
    )
    oeli::input_check_response(
      check = checkmate::check_list(availability, len = length(X)),
      var_name = "availability"
    )

    ### panel counts define the observation blocks
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

    ### composite likelihood requires observed panel responses
    oeli::input_check_response(
      check = checkmate::check_choice(
        cml, choices = c("no", "fp", "ap")
      ),
      var_name = "cml"
    )
    panel_input <- !is.null(y) &&
      (!is.null(Omega) || !is.null(weights)) &&
      !is.null(Tp) && any(Tp > 1)
    if (cml != "no" && !panel_input) {
      cli::cli_abort(
        "Composite marginal likelihood requires observed panel choices.",
        call = NULL
      )
    }
    ### latent classes store one coefficient vector per class
    oeli::input_check_response(
      check = list(
        oeli::check_numeric_vector(beta, finite = TRUE, any.missing = FALSE),
        check_beta_list(beta)
      ),
      var_name = "beta"
    )

    ### reuse coefficient dimensions in subsequent matrix checks
    if (checkmate::test_list(beta)) {
      beta_lengths <- vapply(beta, length, integer(1))
      beta_dim <- if (length(beta_lengths)) beta_lengths[1] else 0L
      if (length(unique(beta_lengths)) != 1L) {
        cli::cli_abort(
          "Coefficient vectors in {.var beta} must have equal lengths.",
          call = NULL
        )
      }
    } else {
      beta_dim <- length(beta)
      beta_lengths <- beta_dim
    }
    if (is.null(weights) && checkmate::test_list(beta)) {
      cli::cli_abort(
        "Class-specific coefficients in {.var beta} require latent class
        weights.",
        call = NULL
      )
    }

    ### random-effect covariance may be common or class-specific
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
    if (is.null(weights) && checkmate::test_list(Omega)) {
      cli::cli_abort(
        "Class-specific covariance matrices require latent class weights.",
        call = NULL
      )
    }
    if (!is.null(weights) && !is.null(Omega) &&
        !checkmate::test_list(Omega)) {
      cli::cli_abort(
        "Latent class covariance matrices in {.var Omega} must be a list.",
        call = NULL
      )
    }

    ### ordered models use a variance; unordered models use covariance
    if (is.null(gamma)) {
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(Sigma),
        var_name = "Sigma"
      )
    } else {
      oeli::input_check_response(
        check = checkmate::check_number(
          Sigma, lower = .Machine$double.eps
        ),
        var_name = "Sigma"
      )
    }

    ### ordered thresholds must be finite and strictly increasing
    oeli::input_check_response(
      check = oeli::check_numeric_vector(
        gamma, sorted = TRUE, finite = TRUE, any.missing = FALSE,
        min.len = 1, null.ok = TRUE
      ),
      var_name = "gamma"
    )
    if (!is.null(gamma) && any(diff(gamma) <= 0)) {
      cli::cli_abort(
        "Thresholds in {.var gamma} must be strictly increasing.",
        call = NULL
      )
    }

    ### every design and response must match its available choice set
    J <- if (is.matrix(Sigma)) nrow(Sigma) else length(gamma) + 1L
    if (is.null(gamma) && J < 2L) {
      cli::cli_abort(
        "Unordered probit models require at least two alternatives.",
        call = NULL
      )
    }
    expected_rows <- if (is.null(gamma)) J else 1L
    for (n in seq_along(X)) {
      oeli::input_check_response(
        check = checkmate::check_matrix(
          X[[n]], mode = "numeric", nrows = expected_rows,
          ncols = beta_dim, any.missing = FALSE
        ),
        var_name = paste0("X[[", n, "]]")
      )
      if (is.null(gamma)) {
        oeli::input_check_response(
          check = checkmate::check_integerish(
            availability[[n]], lower = 1, upper = J, min.len = 1,
            any.missing = FALSE, unique = TRUE
          ),
          var_name = paste0("availability[[", n, "]]")
        )
      }
      oeli::input_check_response(
        check = checkmate::check_numeric(
          as.numeric(X[[n]]), finite = TRUE, any.missing = FALSE
        ),
        var_name = paste0("X[[", n, "]]")
      )
    }
    if (!is.null(y)) {
      for (n in seq_along(y)) {
        oeli::input_check_response(
          check = checkmate::check_integerish(
            y[[n]], lower = 1, upper = J, min.len = 1,
            any.missing = FALSE, unique = TRUE
          ),
          var_name = paste0("y[[", n, "]]")
        )
        if (!is.null(gamma) && length(y[[n]]) != 1L) {
          cli::cli_abort(
            "Ordered choice indices in {.var y} must be scalar.",
            call = NULL
          )
        }
        if (is.null(gamma) &&
            !all(y[[n]] %in% availability[[n]])) {
          cli::cli_abort(
            "Observed choices must belong to their individual choice set.",
            call = NULL
          )
        }
      }
    }

    ### normalize valid class weights once for all downstream paths
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

    ### random-effect positions must index the coefficient vector
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

    ### the Gaussian CDF callback must accept upper bounds and correlation
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

  } else {
    panel_model <- model_type %in% c(12:14, 24:26, 28:30)
    ranked_model <- model_type %in% c(2, 6, 14, 18, 22, 26, 30)
    if (ranked_model && any(lengths(y) > lengths(availability))) {
      cli::cli_abort(
        "Ranked outcomes cannot exceed the available alternatives.",
        call = NULL
      )
    }
    unranked_model <- model_type %in% c(0, 4, 12, 16, 20, 24, 28)
    if (unranked_model && any(lengths(y) != 1L)) {
      cli::cli_abort(
        "Unranked choice indices in {.var y} must be scalar.",
        call = NULL
      )
    }
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

#' @noRd

choiceprob_logit_input_checks <- function(
    X, y, Tp, beta, Omega, gamma, weights, ordered, ranked, panel, lc,
    draws, n_draws, availability
  ) {

  result <- list(weights = weights)
  flags <- c(ordered, ranked, panel, lc)
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
  oeli::input_check_response(
    check = checkmate::check_list(X, min.len = 1),
    var_name = "X"
  )
  oeli::input_check_response(
    check = checkmate::check_list(availability, len = length(X)),
    var_name = "availability"
  )

  if (panel) {
    oeli::input_check_response(
      check = checkmate::check_integerish(
        Tp, lower = 1, any.missing = FALSE
      ),
      var_name = "Tp"
    )
    if (!length(Tp) || sum(Tp) != length(X)) {
      cli::cli_abort(
        "Panel counts {.var Tp} must sum to the length of {.var X}.",
        call = NULL
      )
    }
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
      check = oeli::check_numeric_vector(
        gamma, finite = TRUE, any.missing = FALSE, min.len = 1
      ),
      var_name = "gamma"
    )
    if (any(diff(gamma) <= 0)) {
      cli::cli_abort(
        "Thresholds in {.var gamma} must be strictly increasing.",
        call = NULL
      )
    }
  }

  if (lc) {
    oeli::input_check_response(
      check = checkmate::check_numeric(
        weights, lower = 0, finite = TRUE, any.missing = FALSE, min.len = 1
      ),
      var_name = "weights"
    )
    if (!checkmate::test_list(beta)) {
      cli::cli_abort(
        "Latent class logit probabilities require class-specific coefficient
        lists.",
        call = NULL
      )
    }
    oeli::input_check_response(check_beta_list(beta), var_name = "beta")
    if (length(beta) != length(weights)) {
      cli::cli_abort(
        "Number of coefficient vectors and class weights must match.",
        call = NULL
      )
    }
    weight_sum <- sum(weights)
    if (weight_sum <= 0) {
      cli::cli_abort(
        "Latent class weights must sum to a positive value.",
        call = NULL
      )
    }
    if (!isTRUE(all.equal(weight_sum, 1))) {
      result$weights <- weights / weight_sum
      cli::cli_warn(
        "Latent class weights did not sum to one and were normalized.",
        call = NULL
      )
    }
    beta_lengths <- vapply(beta, length, integer(1))
    if (length(unique(beta_lengths)) != 1L) {
      cli::cli_abort(
        "Coefficient vectors in {.var beta} must have equal lengths.",
        call = NULL
      )
    }
    beta_dim <- beta_lengths[1]
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
        oeli::input_check_response(
          check = checkmate::check_matrix(
            draws, mode = "numeric", min.rows = 1,
            ncols = dims[1], any.missing = FALSE
          ),
          var_name = "draws"
        )
        oeli::input_check_response(
          check = checkmate::check_numeric(
            as.numeric(draws), finite = TRUE
          ),
          var_name = "draws"
        )
      } else {
        check_n_draws(n_draws)
      }
    }
  } else {
    oeli::input_check_response(
      check = oeli::check_numeric_vector(
        beta, finite = TRUE, any.missing = FALSE
      ),
      var_name = "beta"
    )
    beta_dim <- length(beta)
    if (!is.null(Omega)) {
      oeli::input_check_response(
        check = oeli::check_covariance_matrix(Omega),
        var_name = "Omega"
      )
      if (!is.null(draws)) {
        oeli::input_check_response(
          check = checkmate::check_matrix(
            draws, mode = "numeric", min.rows = 1,
            ncols = nrow(Omega), any.missing = FALSE
          ),
          var_name = "draws"
        )
        oeli::input_check_response(
          check = checkmate::check_numeric(
            as.numeric(draws), finite = TRUE
          ),
          var_name = "draws"
        )
      } else {
        check_n_draws(n_draws)
      }
    }
  }

  for (n in seq_along(X)) {
    oeli::input_check_response(
      check = checkmate::check_matrix(
        X[[n]], mode = "numeric", any.missing = FALSE
      ),
      var_name = paste0("X[[", n, "]]")
    )
  }
  rows <- vapply(X, nrow, integer(1))
  expected_rows <- if (ordered) 1L else rows[1]
  if (!ordered && expected_rows < 2L) {
    cli::cli_abort(
      "Unordered choice design matrices require at least two rows.",
      call = NULL
    )
  }
  for (n in seq_along(X)) {
    oeli::input_check_response(
      check = checkmate::check_matrix(
        X[[n]], mode = "numeric", nrows = expected_rows,
        ncols = beta_dim, any.missing = FALSE
      ),
      var_name = paste0("X[[", n, "]]")
    )
    oeli::input_check_response(
      check = checkmate::check_numeric(
        as.numeric(X[[n]]), finite = TRUE, any.missing = FALSE
      ),
      var_name = paste0("X[[", n, "]]")
    )
    if (!ordered) {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          availability[[n]], lower = 1, upper = expected_rows,
          min.len = 1, any.missing = FALSE, unique = TRUE
        ),
        var_name = paste0("availability[[", n, "]]")
      )
    }
  }

  if (!is.null(y) && !ordered) {
    lengths_y <- vapply(y, length, numeric(1))
    if (ranked && any(lengths_y > lengths(availability))) {
      cli::cli_abort(
        "Ranked outcomes cannot exceed the available alternatives.",
        call = NULL
      )
    }
  }
  if (!is.null(y)) {
    J <- if (ordered) length(gamma) + 1L else expected_rows
    for (n in seq_along(y)) {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          y[[n]], lower = 1, upper = J, min.len = 1,
          any.missing = FALSE, unique = TRUE
        ),
        var_name = paste0("y[[", n, "]]")
      )
      if (!ranked && length(y[[n]]) != 1L) {
        cli::cli_abort(
          "Unranked choice indices in {.var y} must be scalar.",
          call = NULL
        )
      }
      if (!ordered && !all(y[[n]] %in% availability[[n]])) {
        cli::cli_abort(
          "Observed choices must belong to their individual choice set.",
          call = NULL
        )
      }
    }
  }
  result
}
