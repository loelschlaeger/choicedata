#' Define choice response
#'
#' @description
#' The `choice_responses` object defines the observed choice responses.
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
#' ### generate choice responses from choice effects
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | time,
#'     error_term = "probit"
#'   ),
#'   choice_alternatives = choice_alternatives(J = 5)
#' )
#' (generate_choice_responses(
#'   choice_effects = choice_effects,
#'   choice_type = "ranked"
#' ))

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
  check_choice_object(
    x = x,
    class_name = "choice_responses",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_responses
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param choice_covariates \[`choice_covariates`\]\cr
#' A \code{\link{choice_covariates}} object.
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' A \code{\link{choice_parameters}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object.
#'
#' @param choice_preferences \[`choice_preferences`\]\cr
#' A \code{\link{choice_preferences}} object.
#'
#' @export
#'
#' @param choice_type \[`character(1)`\]\cr
#' The response type to simulate. Use `"unordered"` (default), `"ordered"`,
#' or `"ranked"`.

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
  choice_type = c("unordered", "ordered", "ranked")
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
  if (identical(choice_type, "ordered") && !ordered_alternatives) {
    cli::cli_abort(
      "Simulating ordered responses requires {.code ordered = TRUE}
      alternatives.",
      call = NULL
    )
  }
  if (identical(choice_type, "unordered") && ordered_alternatives) {
    cli::cli_abort(
      "Simulating unordered responses requires alternatives without an
      ordering.",
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
    ### keep unused ordered categories available to later model construction
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

#' @noRd

extract_choice_indices <- function(
    choice_data,
    choice_effects,
    choice_identifiers = extract_choice_identifiers(choice_data)
  ) {

  is.choice_data(choice_data, error = TRUE)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_identifiers(choice_identifiers, error = TRUE)

  prep <- prepare_choice_long_data(
    choice_data, choice_effects, choice_identifiers
  )
  column_choice <- prep$column_choice
  if (!prep$has_choice) {
    choice_list <- rep(list(integer()), nrow(prep$ids_df))
    return(structure(choice_list, Tp = prep$Tp))
  }
  if (is.null(column_choice) || !column_choice %in% names(prep$x_long)) {
    cli::cli_abort(
      "Cannot extract choices because column {.val {column_choice}} is
      missing.",
      call = NULL
    )
  }

  choice_list <- vector("list", length = nrow(prep$ids_df))
  for (k in seq_len(nrow(prep$ids_df))) {
    df_nt <- subset_choice_occasion(prep, k)
    values_raw <- df_nt[[column_choice]]
    if (is.factor(values_raw) && is.ordered(values_raw)) {
      values <- as.numeric(values_raw)
    } else if (is.factor(values_raw)) {
      values <- suppressWarnings(as.numeric(as.character(values_raw)))
    } else {
      values <- suppressWarnings(as.numeric(values_raw))
    }
    if (identical(prep$choice_type, "ranked")) {
      rank_values <- suppressWarnings(as.numeric(values_raw))
      rank_values <- rank_values[!is.na(rank_values)]
      if (!length(rank_values)) {
        choice_list[[k]] <- integer()
        next
      }
      rank_integers <- as.integer(rank_values)
      valid_ranking <- isTRUE(all.equal(rank_values, rank_integers)) &&
        identical(sort(rank_integers), seq_along(rank_integers))
      if (!valid_ranking) {
        cli::cli_abort(
          "Observed ranks must be consecutive and start at one.",
          call = NULL
        )
      }
      order_idx <- which(!is.na(values_raw))[order(rank_integers)]
      ranking_alts <- df_nt[["alternative"]][order_idx]
      choice_list[[k]] <- match(ranking_alts, prep$alts)
    } else if (identical(prep$choice_type, "ordered")) {
      non_missing <- !is.na(values_raw)
      if (!any(non_missing)) {
        choice_list[[k]] <- integer()
        next
      }
      if (length(unique(values_raw[non_missing])) != 1L) {
        cli::cli_abort(
          "Ordered choice data must report a single category per observation.",
          call = NULL
        )
      }
      selected <- values_raw[which(non_missing)][1]
      idx <- NA_integer_
      if (is.factor(values_raw)) {
        idx <- as.integer(selected)
      } else if (is.numeric(selected)) {
        idx <- as.integer(selected)
      } else {
        idx <- match(as.character(selected), prep$alts)
      }
      if (is.na(idx) || idx < 1L || idx > prep$J) {
        cli::cli_abort(
          "Ordered choice categories must align with declared alternatives.",
          call = NULL
        )
      }
      choice_list[[k]] <- idx
    } else {
      chosen_idx <- which(values == 1)
      if (!length(chosen_idx) && all(is.na(values))) {
        choice_list[[k]] <- integer()
        next
      }
      if (length(chosen_idx) != 1) {
        cli::cli_abort(
          "Choice data must contain exactly one chosen alternative per
          observation.",
          call = NULL
        )
      }
      chosen_alt <- df_nt[["alternative"]][chosen_idx]
      choice_list[[k]] <- match(chosen_alt, prep$alts)
    }
  }

  structure(choice_list, Tp = prep$Tp)
}
