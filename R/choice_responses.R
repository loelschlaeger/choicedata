#' Define choice response
#'
#' @description
#' The `choice_responses` object defines the observed discrete responses.
#' Additional response columns (for example ranked choice indicators) are
#' preserved so they can be merged with covariates downstream.
#'
#' - `generate_choice_responses()` simulates choices
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice responses.
#'
#' @param column_choice \[`character(1)`\]\cr
#' The column name of `data_frame` with the choice responses.
#'
#' @inheritParams choice_identifiers
#'
#' @return
#' A `choice_responses` tibble.
#'
#' @export
#'
#' @keywords data
#'
#' @examples
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | time,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 5)
#' )
#' generate_choice_responses(choice_effects = choice_effects)

choice_responses <- function(
    data_frame,
    column_choice = "choice",
    column_decider = "deciderID",
    column_occasion = NULL,
    cross_section = is.null(column_occasion)
  ) {

  ### input checks
  check_not_missing(data_frame)
  check_column_choice(column_choice, null.ok = FALSE)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_cross_section(cross_section)
  id_cols <- c(column_decider, column_occasion)
  id_cols <- id_cols[!vapply(id_cols, is.null, logical(1))]
  choice_cols <- setdiff(names(data_frame), id_cols)
  check_data_frame(
    data_frame,
    required_columns = c(column_decider, column_occasion, column_choice),
    allow_missing_columns = choice_cols
  )
  choice_identifiers <- choice_identifiers(
    data_frame = data_frame[c(column_decider, column_occasion)],
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )

  ### build 'choice_responses' object
  if (!column_choice %in% choice_cols) {
    cli::cli_abort(
      "Column {.val {column_choice}} must be present in {.var data_frame} to
      build {.cls choice_responses}.",
      call = NULL
    )
  }
  choices <- data_frame[choice_cols]
  responses <- tibble::as_tibble(cbind(choice_identifiers, choices))
  structure(
    responses,
    class = tibble_class("choice_responses", class(data_frame)),
    column_decider = attr(choice_identifiers, "column_decider"),
    column_occasion = attr(choice_identifiers, "column_occasion"),
    cross_section = attr(choice_identifiers, "cross_section"),
    column_choice = column_choice,
    column_response_columns = choice_cols
  )
}

#' @noRd

is.choice_responses <- function(
    x,
    error = TRUE,
    var_name = oeli::variable_name(x)
  ) {
  validate_choice_object(
    x = x,
    class_name = "choice_responses",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_responses
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object describing the model structure.
#'
#' @param choice_covariates \[`choice_covariates`\]\cr
#' Covariates used to construct utilities.
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' Model parameters supplying the mean and covariance components.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' Identifiers describing the panel or cross-sectional structure.
#'
#' @param choice_preferences \[`choice_preferences`\]\cr
#' Preference draws to simulate the choices.
#'
#' @export
#'
#' @param choice_type \[`character(1)`\]\cr
#' The response type to simulate. Use `"auto"` (default) to derive the type
#' from `choice_alternatives`, or explicitly request `"discrete"`,
#' `"ordered"`, or `"ranked"` outcomes.

generate_choice_responses <- function(
  choice_effects,
  choice_covariates = generate_choice_covariates(
    choice_effects = choice_effects
  ),
  choice_parameters = generate_choice_parameters(
    choice_effects = choice_effects
  ),
  choice_identifiers = extract_choice_identifiers(choice_covariates),
  choice_preferences = generate_choice_preferences(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  ),
  column_choice = "choice",
  choice_type = c("auto", "discrete", "ordered", "ranked")
) {

  ### input checks
  check_not_missing(choice_effects)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_covariates(choice_covariates, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)
  is.choice_preferences(choice_preferences, error = TRUE)
  check_column_choice(column_choice = column_choice, null.ok = FALSE)
  choice_type <- match.arg(choice_type)

  ### extract objects
  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  Tp <- read_Tp(choice_identifiers)
  design_list <- design_matrices(
    x = choice_covariates,
    choice_effects = choice_effects,
    choice_identifiers = choice_identifiers
  )
  availability <- attr(design_list, "availability")
  choice_preferences <- split_choice_preferences(
    choice_preferences,
    choice_identifiers = choice_identifiers
  )
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")
  decider_ids <- get_decider_identifiers(choice_identifiers)
  preference_index <- match(
    choice_identifiers[[column_decider]],
    decider_ids
  )
  ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))
  inferred_type <- if (ordered_alternatives) "ordered" else "discrete"
  if (identical(choice_type, "auto")) {
    choice_type <- inferred_type
  }
  if (identical(choice_type, "ordered") && !ordered_alternatives) {
    cli::cli_abort(
      "Simulating ordered responses requires {.code ordered = TRUE}
      alternatives.",
      call = NULL
    )
  }
  if (identical(choice_type, "ranked") && ordered_alternatives) {
    cli::cli_abort(
      "Ranked simulations are not available for ordered alternatives.",
      call = NULL
    )
  }
  if (is.null(choice_alternatives)) {
    cli::cli_abort(
      "Choice alternatives must be supplied to simulate responses.",
      call = NULL
    )
  }
  alt_labels <- as.character(choice_alternatives)

  choice_formula <- attr(choice_effects, "choice_formula")
  error_term <- choice_formula$error_term

  if (identical(error_term, "probit")) {
    Sigma <- choice_parameters$Sigma
    if (is.null(Sigma)) {
      cli::cli_abort(
        "Input {.var choice_parameters} must include {.field Sigma} to simulate
        probit choices.",
        call = NULL
      )
    }
    if (identical(choice_type, "ordered")) {
      Sigma_sd <- sqrt(Sigma)
      gamma <- choice_parameters$gamma
      if (is.null(gamma)) {
        cli::cli_abort(
          "Ordered probit simulation requires threshold parameters
          {.field gamma}.",
          call = NULL
        )
      }
      gamma_augmented <- c(-Inf, gamma, +Inf)
    }
  } else if (identical(error_term, "logit")) {
    if (identical(choice_type, "ordered")) {
      gamma <- choice_parameters$gamma
      if (is.null(gamma)) {
        cli::cli_abort(
          "Ordered logit simulation requires threshold parameters
          {.field gamma}.",
          call = NULL
        )
      }
      gamma_augmented <- c(-Inf, gamma, +Inf)
    }
  } else {
    cli::cli_abort(
      "Unsupported error term {.val {error_term}} for simulating responses.",
      call = NULL
    )
  }

  ### simulate choices
  total_obs <- sum(Tp)
  top_choices <- vector("character", length = total_obs)
  if (identical(choice_type, "ordered")) {
    top_choices <- vector("list", length = total_obs)
  }
  ranked_matrix <- if (identical(choice_type, "ranked")) {
    matrix(NA_integer_, nrow = total_obs, ncol = length(choice_alternatives))
  }
  for (id in seq_len(total_obs)) {
    n <- preference_index[id]
    preference_n <- choice_preferences[[n]]
    design_matrix_nt <- design_list[[id]]
    if (identical(choice_type, "ordered")) {
      mean_val <- as.numeric(design_matrix_nt %*% preference_n)
      if (identical(error_term, "probit")) {
        utility <- stats::rnorm(n = 1L, mean = mean_val, sd = Sigma_sd)
      } else {
        utility <- mean_val + stats::rlogis(n = 1L)
      }
      idx <- findInterval(
        utility, gamma_augmented, all.inside = TRUE, left.open = TRUE
      )
      top_choices[[id]] <- choice_alternatives[idx]
    } else {
      available <- availability[[id]]
      design_matrix_nt <- design_matrix_nt[available, , drop = FALSE]
      if (identical(error_term, "probit")) {
        U_id <- oeli::rmvnorm(
          mean = as.vector(design_matrix_nt %*% preference_n),
          Sigma = Sigma[available, available, drop = FALSE]
        )
      } else {
        V_id <- as.vector(design_matrix_nt %*% preference_n)
        eps <- -log(-log(stats::runif(length(V_id))))
        U_id <- V_id + eps
      }
      if (identical(choice_type, "ranked")) {
        order_idx <- order(U_id, decreasing = TRUE)
        ranking <- choice_alternatives[available[order_idx]]
        top_choices[id] <- ranking[1]
        ranked_matrix[id, ] <- match(choice_alternatives, ranking)
      } else {
        top_choices[id] <- choice_alternatives[available[which.max(U_id)]]
      }
    }
  }

  ### create and return 'choice_responses' object
  data_frame <- as.data.frame(choice_identifiers, stringsAsFactors = FALSE)
  if (identical(choice_type, "ordered")) {
    choices <- vapply(top_choices, as.character, character(1))
    # Keep unused ordered categories available to later model construction.
    data_frame[[column_choice]] <- factor(
      choices, levels = alt_labels, ordered = TRUE
    )
  } else {
    data_frame[[column_choice]] <- as.character(top_choices)
  }
  if (identical(choice_type, "ranked")) {
    col_names <- paste(column_choice, alt_labels, sep = "_")
    dimnames(ranked_matrix) <- list(NULL, col_names)
    ranked_df <- as.data.frame(ranked_matrix, stringsAsFactors = FALSE)
    ranked_df[] <- lapply(ranked_df, as.integer)
    data_frame <- cbind(data_frame, ranked_df)
  }
  cross_section <- attr(choice_identifiers, "cross_section")
  choice_responses(
    data_frame = data_frame,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_choice = column_choice,
    cross_section = cross_section
  )
}
