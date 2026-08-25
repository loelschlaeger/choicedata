#' Define choice data
#'
#' @description
#' The `choice_data` object defines the choice data, it is a combination of
#' `choice_responses` and `choice_covariates`.
#'
#' @details
#' `choice_data()` acts as the main entry point for observed data. It accepts
#' either long or wide layouts and performs validation before
#' returning a tidy tibble with consistent identifiers. Columns that refer to
#' the same alternative are aligned using `delimiter` so that downstream
#' helpers can detect them automatically. For ranked or ordered choices, the
#' function checks the response encoding and reports invalid inputs.
#'
#' In long format, omit rows for alternatives that are unavailable in a choice
#' occasion. The long layout is retained so that individual choice sets remain
#' distinguishable. Missing responses are allowed, but covariates must be
#' observed because otherwise the corresponding utilities are undefined.
#'
#' - `generate_choice_data()` simulates choice data.
#' - `wide_to_long()` and `long_to_wide()` transform to wide and long format.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the choice data.
#'
#' @param format \[`character(1)`\]\cr
#' Format of `data_frame`. Use `"wide"` when each row contains all alternatives
#' of an occasion and `"long"` when each row contains a single alternative.
#'
#' @param column_choice \[`character(1)` | `NULL`\]\cr
#' Column name with the observed choices. In wide layout this column should
#' contain a single value per observation: for discrete data the value is the
#' label of the chosen alternative, for ordered data it is the ordered factor or
#' integer score, and for ranked data it is omitted in favor of one column per
#' alternative (see `choice_type`). In long layout the same column is evaluated
#' once per alternative: discrete data must use a binary indicator (1 for the
#' chosen alternative, 0 otherwise), ordered data repeats the ordinal value for
#' every alternative, and ranked data stores consecutive ranks `1:k` for the
#' observed top `k` alternatives and `NA` for unranked alternatives. An
#' entirely missing response marks an occasion that is omitted from the
#' likelihood. Set to `NULL` for purely covariate tables.
#'
#' @param column_as_covariates \[`character()` | `NULL`\]\cr
#' Column names of `data_frame` with alternative-specific covariates.
#'
#' @param column_decider \[`character(1)`\]\cr
#' Column name with decider identifiers.
#'
#' @param column_occasion \[`character(1)` | `NULL`\]\cr
#' Column name with occasion identifiers. Set to `NULL` in cross-sectional data.
#'
#' @param column_alternative \[`character(1)` | `NULL`\]\cr
#' Column name with alternative identifiers when `format = "long"`.
#'
#' @param column_ac_covariates \[`character()` | `NULL`\]\cr
#' Column names with alternative-constant covariates.
#'
#' @param delimiter \[`character(1)`\]\cr
#' Delimiter separating alternative identifiers from covariate names in wide
#' format. May consist of one or more characters.
#'
#' @param choice_type \[`character(1)`\]\cr
#' Type of choice responses. Use `"discrete"` for standard multinomial choice
#' data, `"ordered"` for ordered outcomes (declare the order via
#' `choice_alternatives(ordered = TRUE)`), and `"ranked"` for ranked choice
#' data. Ranked responses may be complete or partial and use consecutive
#' integers `1:k`, where `k` can vary by observation. In wide format, rankings
#' are stored in columns named
#' `paste0(column_choice, delimiter, alternative)`; use `NA` for an unranked
#' alternative.
#'
#' @inheritParams choice_alternatives
#' @inheritParams choice_responses
#'
#' @return
#' `choice_data()` and `generate_choice_data()` return a `choice_data` tibble.
#' `long_to_wide()` and `wide_to_long()` return a tibble in the requested
#' layout.
#'
#' @seealso
#' [choice_responses()], [choice_covariates()], and [choice_identifiers()] for
#' the helper objects that feed into `choice_data()`.
#'
#' @export
#'
#' @keywords data
#'
#' @examples
#' ### simulate data from a multinomial probit model
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ A | B, error_term = "probit",
#'     random_effects = c("A" = "cn")
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#' generate_choice_data(choice_effects)
#'
#' ### transform between long/wide format
#' long_to_wide(
#'   data_frame = travel_mode_choice,
#'   column_alternative = "mode",
#'   column_decider = "individual"
#' )
#' wide_to_long(
#'   data_frame = train_choice
#' )
#'
#' ### individual choice sets and a missing response
#' partial_data <- data.frame(
#'   deciderID = c(1, 1, 2),
#'   alternative = c("A", "B", "B"),
#'   choice = c(1L, 0L, NA),
#'   cost = c(1.2, 1.5, 0.8)
#' )
#' choice_data(
#'   data_frame = partial_data,
#'   format = "long",
#'   column_decider = "deciderID",
#'   column_alternative = "alternative",
#'   column_as_covariates = "cost"
#' )

choice_data <- function(
  data_frame,
  format = "wide",
  column_choice = "choice",
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  delimiter = "_",
  cross_section = is.null(column_occasion),
  choice_type = c("discrete", "ordered", "ranked")
) {

  ### input checks
  check_not_missing(data_frame)
  check_format(format)
  check_column_choice(column_choice, null.ok = TRUE)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_column_alternative(column_alternative, null.ok = format == "wide")
  if (format == "wide") column_alternative <- NULL
  check_column_covariates(
    column_ac_covariates, null.ok = TRUE, var_name = "column_ac_covariates"
  )
  check_column_covariates(
    column_as_covariates, null.ok = TRUE, var_name = "column_as_covariates"
  )
  check_delimiter(delimiter)
  check_cross_section(cross_section)
  choice_type <- match.arg(choice_type)
  if (identical(choice_type, "ranked")) {
    check_column_choice(column_choice, null.ok = FALSE)
  }
  rank_cols <- character()
  if (identical(format, "wide") && identical(choice_type, "ranked")) {
    prefix <- paste0(column_choice, delimiter)
    rank_cols <- names(data_frame)[startsWith(names(data_frame), prefix)]
  }
  choice_column_check <- column_choice
  if (identical(format, "wide") && identical(choice_type, "ranked") &&
      !column_choice %in% names(data_frame)) {
    choice_column_check <- NULL
  }
  ac_as_covariates <- check_as_covariates(
    data_frame,
    format = format,
    column_choice = choice_column_check,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    column_ac_covariates = column_ac_covariates,
    column_as_covariates = column_as_covariates,
    delimiter = delimiter,
    allow_missing_columns = c(column_choice, rank_cols)
  )
  column_ac_covariates <- ac_as_covariates$column_ac_covariates
  column_as_covariates <- ac_as_covariates$column_as_covariates
  column_as_covariates_wide <- ac_as_covariates$column_as_covariates_wide
  alternatives_ranked <- character()
  if (identical(format, "wide") && identical(choice_type, "ranked")) {
    alternatives_ranked <- substring(rank_cols, nchar(prefix) + 1L)
    check_alternatives(alternatives_ranked)
    column_as_covariates <- setdiff(
      column_as_covariates,
      column_choice
    )
    column_as_covariates_wide <- setdiff(
      column_as_covariates_wide,
      rank_cols
    )
    ranks <- as.matrix(data_frame[rank_cols])
    J <- length(alternatives_ranked)
    oeli::input_check_response(
      check = checkmate::check_integerish(
        ranks, lower = 1, upper = J, any.missing = TRUE
      ),
      var_name = "ranking columns"
    )
    valid_ranking <- apply(
      ranks,
      1,
      function(x) {
        x <- x[!is.na(x)]
        !length(x) || identical(sort(as.integer(x)), seq_along(x))
      }
    )
    oeli::input_check_response(
      check = if (all(valid_ranking)) TRUE else paste(
        "Observed ranks must be consecutive and start at one"
      ),
      var_name = "ranking columns"
    )
  }
  required_columns <- c(
    choice_column_check, column_decider, column_occasion, column_alternative,
    column_ac_covariates,
    if (format == "long") column_as_covariates else column_as_covariates_wide
  )
  data_frame <- check_data_frame(
    data_frame,
    forbidden_columns = if (format == "wide") column_alternative,
    required_columns = required_columns,
    allow_missing_columns = c(column_choice, rank_cols)
  )

  ### transform to wide format
  alternatives <- NULL
  if (identical(format, "long") && !is.null(column_alternative)) {
    alternatives <- data_frame[[column_alternative]]
    alternatives <- alternatives[!is.na(alternatives)]
    alternatives <- unique(as.character(alternatives))
  }

  data_frame_wide <- if (format == "long") {
    long_to_wide(
      data_frame = data_frame,
      column_as_covariates = column_as_covariates,
      column_choice = column_choice,
      column_alternative = column_alternative,
      column_decider = column_decider,
      column_occasion = column_occasion,
      alternatives = alternatives,
      delimiter = delimiter,
      choice_type = choice_type
    )
  } else {
    data_frame
  }

  ### extract choice identifiers
  identifier_data <- if (identical(format, "long")) {
    data_frame[c(column_decider, column_occasion)]
  } else {
    data_frame_wide[c(column_decider, column_occasion)]
  }
  choice_identifiers <- choice_identifiers(
    data_frame = identifier_data,
    format = format,
    column_decider = column_decider,
    column_occasion = column_occasion,
    cross_section = cross_section
  )
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")

  ### build 'choice_data' object
  choice_data <- if (format == "long") {
    columns <- c(
      column_decider, column_occasion, column_alternative, column_choice,
      column_ac_covariates, column_as_covariates
    )
    columns <- unique(columns[!is.na(columns)])
    tibble::as_tibble(data_frame[, columns, drop = FALSE])
  } else {
    choice_choice_cols <- if (!is.null(column_choice) &&
        column_choice %in% names(data_frame)) {
      data_frame[column_choice]
    } else {
      data_frame[, character(), drop = FALSE]
    }
    choice_covariate_cols <- data_frame[
      , c(column_ac_covariates, column_as_covariates_wide, rank_cols),
      drop = FALSE
    ]
    choice_data <- cbind(
      choice_identifiers, choice_choice_cols, choice_covariate_cols
    )
  }
  structure(
    choice_data,
    class = tibble_class("choice_data", class(data_frame)),
    format = format,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    column_ac_covariates = column_ac_covariates,
    column_as_covariates = column_as_covariates,
    column_as_covariates_wide = column_as_covariates_wide,
    delimiter = delimiter,
    cross_section = attr(choice_identifiers, "cross_section"),
    choice_type = choice_type
  )
}

#' @noRd

is.choice_data <- function(
    x,
    error = TRUE,
    var_name = oeli::variable_name(x)
  ) {
  validate_choice_object(
    x = x,
    class_name = "choice_data",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_data
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object describing the model.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object that provides the decider and
#' occasion identifiers.
#'
#' @param choice_covariates \[`choice_covariates`\]\cr
#' Covariates to include in the generated data.
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' Model parameters supplying utilities and covariance structures.
#'
#' @param choice_preferences \[`choice_preferences`\]\cr
#' Decider-specific preference draws used for simulation.
#'
#' @param choice_type \[`character(1)`\]\cr
#' Requested response type. Use `"auto"` (default) to infer the mode from
#' `choice_alternatives()`, or explicitly simulate `"discrete"`, `"ordered"`,
#' or `"ranked"` outcomes.
#'
#' @details
#' The generated `choice_data` object inherits a `choice_type` attribute for
#' the requested simulation mode. Ordered alternatives (`ordered = TRUE`)
#' yield ordered responses, unordered alternatives default to discrete
#' multinomial outcomes, and ranked simulations return complete rankings for
#' every observation.
#'
#' @export

generate_choice_data <- function(
  choice_effects,
  choice_identifiers = generate_choice_identifiers(N = 100),
  choice_covariates = generate_choice_covariates(
    choice_effects, choice_identifiers
  ),
  choice_parameters = generate_choice_parameters(choice_effects),
  choice_preferences = generate_choice_preferences(
    choice_effects, choice_parameters, choice_identifiers
  ),
  column_choice = "choice",
  choice_type = c("auto", "discrete", "ordered", "ranked")
) {

  choice_alternatives <- attr(choice_effects, "choice_alternatives")
  ordered_alternatives <- if (is.null(choice_alternatives)) {
    FALSE
  } else {
    isTRUE(attr(choice_alternatives, "ordered"))
  }
  inferred_type <- if (ordered_alternatives) "ordered" else "discrete"
  choice_type <- match.arg(choice_type)
  if (identical(choice_type, "auto")) {
    choice_type <- inferred_type
  }
  if (identical(choice_type, "ordered") && !ordered_alternatives) {
    cli::cli_abort(
      "Simulating ordered choice data requires {.code ordered = TRUE}
      alternatives.",
      call = NULL
    )
  }
  if (identical(choice_type, "ranked") && ordered_alternatives) {
    cli::cli_abort(
      "Ranked simulations are not available when alternatives encode an
      ordering.",
      call = NULL
    )
  }

  ### simulate choices
  choice_responses <- generate_choice_responses(
    choice_effects = choice_effects,
    choice_covariates = choice_covariates,
    choice_parameters = choice_parameters,
    choice_identifiers = choice_identifiers,
    choice_preferences = choice_preferences,
    column_choice = column_choice,
    choice_type = choice_type
  )

  ### merge choices and covariates
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")
  cov_format <- attr(choice_covariates, "format")
  long_format <- identical(cov_format, "long")
  choice_responses_df <- tibble::as_tibble(choice_responses)
  choice_covariates_df <- tibble::as_tibble(choice_covariates)
  if (
    !is.null(column_choice) && column_choice %in% names(choice_covariates_df)
  ) {
    choice_covariates_df[[column_choice]] <- NULL
  }

  join_columns <- c(column_decider, column_occasion)
  duplicates_responses <- duplicated(choice_responses_df[join_columns])
  duplicates_covariates <- if (long_format) {
    rep(FALSE, nrow(choice_covariates_df))
  } else {
    duplicated(choice_covariates_df[join_columns])
  }
  data_frame <- dplyr::inner_join(
    choice_covariates_df,
    choice_responses_df,
    by = join_columns
  )
  responses_only <- dplyr::anti_join(
    choice_responses_df[join_columns],
    choice_covariates_df[join_columns],
    by = join_columns
  )
  covariates_only <- dplyr::anti_join(
    choice_covariates_df[join_columns],
    choice_responses_df[join_columns],
    by = join_columns
  )

  if (nrow(data_frame) != nrow(choice_covariates_df) ||
      nrow(responses_only) || nrow(covariates_only) ||
      any(duplicates_responses) || any(duplicates_covariates)) {
    key_label <- paste(join_columns, collapse = ", ")
    duplicate_responses <- if (any(duplicates_responses)) {
      choice_responses_df[
        duplicates_responses, join_columns, drop = FALSE
      ]
    } else {
      choice_responses_df[0, , drop = FALSE]
    }
    duplicate_covariates <- if (any(duplicates_covariates)) {
      choice_covariates_df[
        duplicates_covariates, join_columns, drop = FALSE
      ]
    } else {
      choice_covariates_df[0, join_columns, drop = FALSE]
    }

    summarise_keys <- function(df) {
      if (is.null(df) || nrow(df) == 0L) {
        return(NULL)
      }
      df <- unique(df)
      df <- utils::head(df, 3L)
      if (ncol(df) == 1L) {
        rows <- df[[1]]
      } else {
        rows <- apply(df, 1, paste, collapse = ", ")
      }
      paste(rows, collapse = "; ")
    }

    bullets <- c(
      "x" = sprintf(
        "Each covariate row must match one unique response on %s.",
        key_label
      ),
      "i" = sprintf(
        "Join produced %d rows; expected %d covariate rows.",
        nrow(data_frame), nrow(choice_covariates_df)
      )
    )
    resp_missing <- summarise_keys(responses_only)
    if (!is.null(resp_missing)) {
      bullets <- c(
        bullets,
        "i" = sprintf("Missing covariates for keys: %s", resp_missing)
      )
    }
    cov_missing <- summarise_keys(covariates_only)
    if (!is.null(cov_missing)) {
      bullets <- c(
        bullets,
        "i" = sprintf("Missing responses for keys: %s", cov_missing)
      )
    }
    dup_resp <- summarise_keys(duplicate_responses)
    if (!is.null(dup_resp)) {
      bullets <- c(
        bullets,
        "i" = sprintf("Duplicate response keys: %s", dup_resp)
      )
    }
    dup_cov <- summarise_keys(duplicate_covariates)
    if (!is.null(dup_cov)) {
      bullets <- c(
        bullets,
        "i" = sprintf("Duplicate covariate keys: %s", dup_cov)
      )
    }
    cli::cli_abort(bullets, call = NULL)
  }

  if (long_format) {
    column_alternative <- attr(choice_covariates, "column_alternative")
    alternative <- as.character(data_frame[[column_alternative]])
    if (identical(choice_type, "discrete")) {
      chosen <- as.character(data_frame[[column_choice]])
      data_frame[[column_choice]] <- as.integer(chosen == alternative)
    } else if (identical(choice_type, "ranked")) {
      alternatives <- as.character(choice_alternatives)
      delimiter <- attr(choice_covariates, "delimiter")
      rank_cols <- paste(column_choice, alternatives, sep = delimiter)
      ranks <- as.matrix(data_frame[rank_cols])
      index <- cbind(
        seq_len(nrow(data_frame)), match(alternative, alternatives)
      )
      data_frame[[column_choice]] <- ranks[index]
      data_frame[rank_cols] <- NULL
    }
  }

  ### create and return 'choice_data' object
  column_as_covariates <- attr(choice_covariates, "column_as_covariates")
  if (!is.null(column_choice) && length(column_as_covariates)) {
    column_as_covariates <- setdiff(column_as_covariates, column_choice)
  }
  if (!long_format && identical(choice_type, "ranked") &&
      !is.null(column_choice)) {
    column_as_covariates <- unique(c(column_as_covariates, column_choice))
  }
  choice_data(
    data_frame = data_frame,
    format = cov_format,
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = attr(choice_covariates, "column_alternative"),
    column_ac_covariates = attr(choice_covariates, "column_ac_covariates"),
    column_as_covariates = column_as_covariates,
    delimiter = attr(choice_covariates, "delimiter"),
    cross_section = attr(choice_identifiers, "cross_section"),
    choice_type = choice_type
  )
}

#' @rdname choice_data
#' @export

long_to_wide <- function(
  data_frame,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  column_choice = "choice",
  column_alternative = "alternative",
  column_decider = "deciderID",
  column_occasion = NULL,
  alternatives = unique(data_frame[[column_alternative]]),
  delimiter = "_",
  choice_type = c("discrete", "ordered", "ranked")
) {

  ### input checks
  check_column_choice(column_choice, null.ok = TRUE)
  check_column_alternative(column_alternative, null.ok = FALSE)
  check_column_covariates(
    column_ac_covariates, null.ok = TRUE, var_name = "column_ac_covariates"
  )
  check_column_covariates(
    column_as_covariates, null.ok = TRUE, var_name = "column_as_covariates"
  )
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  required_columns <- c(
    column_alternative, column_ac_covariates, column_as_covariates,
    column_choice, column_decider, column_occasion
  )
  check_data_frame(
    data_frame,
    required_columns = required_columns,
    allow_missing_columns = column_choice
  )
  check_alternatives(alternatives)
  check_delimiter(delimiter)
  choice_type <- match.arg(choice_type)

  ### validate observation rows and responses
  id_cols <- c(column_decider, column_occasion)
  id_cols <- id_cols[!vapply(id_cols, is.null, logical(1))]
  obs <- interaction(data_frame[id_cols], drop = TRUE, lex.order = TRUE)
  alt <- as.character(data_frame[[column_alternative]])
  alt_by_obs <- split(alt, obs)
  valid_sets <- vapply(
    alt_by_obs,
    function(x) {
      length(x) > 0L && !anyDuplicated(x) && all(x %in% alternatives)
    },
    logical(1)
  )
  oeli::input_check_response(
    check = if (all(valid_sets)) TRUE else paste(
      "Must contain unique declared alternatives in every observation"
    ),
    var_name = "data_frame"
  )
  if (!is.null(column_choice)) {
    response <- data_frame[[column_choice]]
    response_by_obs <- split(response, obs)
    if (identical(choice_type, "discrete")) {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          response, lower = 0, upper = 1, any.missing = TRUE
        ),
        var_name = column_choice
      )
      one_choice <- vapply(response_by_obs, function(x) {
        sum(x == 1, na.rm = TRUE) == 1L || all(is.na(x))
      }, logical(1))
      oeli::input_check_response(
        check = if (all(one_choice)) TRUE else paste(
          "Must identify exactly one chosen alternative per observation"
        ),
        var_name = column_choice
      )
    } else if (identical(choice_type, "ordered")) {
      if (is.factor(response)) {
        oeli::input_check_response(
          check = if (is.ordered(response)) TRUE else paste(
            "Must be an ordered factor"
          ),
          var_name = column_choice
        )
      }
      one_category <- vapply(
        response_by_obs,
        function(x) length(unique(x[!is.na(x)])) <= 1L,
        logical(1)
      )
      oeli::input_check_response(
        check = if (all(one_category)) TRUE else paste(
          "Must contain one category per observation"
        ),
        var_name = column_choice
      )
    } else {
      oeli::input_check_response(
        check = checkmate::check_integerish(
          response, lower = 1, upper = length(alternatives),
          any.missing = TRUE
        ),
        var_name = column_choice
      )
      valid_ranking <- vapply(
        response_by_obs,
        function(x) {
          x <- x[!is.na(x)]
          !length(x) || identical(sort(as.integer(x)), seq_along(x))
        },
        logical(1)
      )
      oeli::input_check_response(
        check = if (all(valid_ranking)) TRUE else paste(
          "Observed ranks must be consecutive and start at one"
        ),
        var_name = column_choice
      )
    }
  }

  ### determine (ac/as) covariate columns
  ac_as <- check_as_covariates(
    data_frame,
    format = "long",
    column_choice = column_choice,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    delimiter = delimiter,
    allow_missing_columns = column_choice
  )
  if (is.null(column_ac_covariates)) {
    column_ac_covariates <- ac_as$column_ac_covariates
  }
  if (is.null(column_as_covariates)) {
    column_as_covariates <- ac_as$column_as_covariates
  }

  ### transform to wide
  id_cols_pivot <- c(column_decider, column_occasion, column_ac_covariates)
  id_cols_pivot <- id_cols_pivot[!vapply(id_cols_pivot, is.null, TRUE)]
  value_cols <- column_as_covariates
  if (!is.null(column_choice) && identical(choice_type, "ranked")) {
    value_cols <- unique(c(value_cols, column_choice))
  }
  generated <- as.vector(
    outer(value_cols, alternatives, paste, sep = delimiter)
  )
  conflicts <- intersect(
    generated,
    c(id_cols_pivot, column_choice)
  )
  if (anyDuplicated(generated) || length(conflicts)) {
    cli::cli_abort(
      "The delimiter creates ambiguous output column names.",
      call = NULL
    )
  }

  if (length(value_cols) == 0L) {
    wide <- dplyr::distinct(
      data_frame, dplyr::across(dplyr::all_of(id_cols_pivot))
    )
  } else {
    wide <- tidyr::pivot_wider(
      data_frame,
      id_cols    = dplyr::all_of(id_cols_pivot),
      names_from = dplyr::all_of(column_alternative),
      values_from = dplyr::all_of(value_cols),
      names_glue = paste0("{.value}", delimiter, "{", column_alternative, "}")
    )
  }
  id_cols_join <- id_cols
  if (!is.null(column_choice)) {
    if (identical(choice_type, "discrete")) {
      choice_labels <- data_frame |>
        dplyr::filter(.data[[column_choice]] == 1) |>
        dplyr::distinct(dplyr::across(
          dplyr::all_of(c(id_cols_join, column_alternative)))
        ) |>
        dplyr::rename(!!column_choice := !!rlang::sym(column_alternative))
    } else if (identical(choice_type, "ranked")) {
      choice_labels <- data_frame |>
        dplyr::filter(.data[[column_choice]] == 1L) |>
        dplyr::distinct(dplyr::across(
          dplyr::all_of(c(id_cols_join, column_alternative))
        )) |>
        dplyr::rename(!!column_choice := !!rlang::sym(column_alternative))
    } else { # ordered
      choice_labels <- data_frame |>
        dplyr::group_by(dplyr::across(dplyr::all_of(id_cols_join))) |>
        dplyr::summarise(
          !!column_choice := dplyr::first(.data[[column_choice]]),
          .groups = "drop"
        )
    }
    wide <- dplyr::left_join(wide, choice_labels, by = id_cols_join)
  }
  tibble::as_tibble(wide)
}

#' @noRd

guess_alternatives_wide <- function(
  data_frame,
  column_choice = NULL,
  delimiter = "_",
  allow_missing_columns = column_choice
) {
  check_data_frame(
    data_frame,
    allow_missing_columns = allow_missing_columns
  )
  check_column_choice(column_choice, null.ok = TRUE)
  check_delimiter(delimiter)
  missing_choice <- !is.null(column_choice) &&
    !column_choice %in% names(data_frame) &&
    !column_choice %in% allow_missing_columns
  if (missing_choice) {
    cli::cli_abort(
      "Column {.val {column_choice}} is missing from {.var data_frame}.",
      call = NULL
    )
  }
  esc <- function(x) gsub("([][{}()+*^$|\\.?*\\\\])", "\\\\\\1", x)
  alts_from_choice <- character()
  if (!is.null(column_choice)) {
    alts_from_choice <- data_frame[[column_choice]] |>
      as.character() |>
      stats::na.omit() |>
      unique()
  }
  alts_from_names <- vapply(
    names(data_frame),
    function(col) {
      matches <- gregexpr(delimiter, col, fixed = TRUE)[[1]]
      if (length(matches) == 1L && matches[1] == -1L) {
        return(NA_character_)
      }
      last <- matches[length(matches)]
      start <- last + nchar(delimiter)
      if (start > nchar(col)) {
        return(NA_character_)
      }
      substring(col, start)
    },
    character(1)
  )
  alts_from_names <- stats::na.omit(alts_from_names)
  if (length(alts_from_names)) {
    alts_from_names <- as.character(alts_from_names)
    alts_from_names <- alts_from_names[nzchar(alts_from_names)]
  }
  sort(unique(c(alts_from_choice, alts_from_names)))
}

#' @rdname choice_data
#' @export

wide_to_long <- function(
  data_frame,
  column_choice = "choice",
  column_alternative = "alternative",
  alternatives = NULL,
  delimiter = "_",
  choice_type = c("discrete", "ordered", "ranked")
) {

  ### input checks
  check_column_choice(column_choice, null.ok = TRUE)
  check_column_alternative(column_alternative, null.ok = FALSE)
  choice_type <- match.arg(choice_type)
  rank_cols <- character()
  if (!is.null(column_choice) && identical(choice_type, "ranked")) {
    rank_cols <- names(data_frame)[startsWith(
      names(data_frame), paste0(column_choice, delimiter)
    )]
  }
  required_choice_cols <- if (!is.null(column_choice) &&
      !identical(choice_type, "ranked")) {
    column_choice
  } else {
    character()
  }
  check_data_frame(
    data_frame,
    required_columns = required_choice_cols,
    forbidden_columns = column_alternative,
    allow_missing_columns = c(column_choice, rank_cols)
  )
  if (is.null(alternatives)) {
    alternatives <- guess_alternatives_wide(
      data_frame,
      column_choice = column_choice,
      delimiter = delimiter,
      allow_missing_columns = c(column_choice, rank_cols)
    )
  }
  check_alternatives(alternatives)
  check_delimiter(delimiter)
  if (!is.null(column_choice) && identical(choice_type, "discrete")) {
    observed <- stats::na.omit(as.character(data_frame[[column_choice]]))
    unknown <- setdiff(observed, alternatives)
    oeli::input_check_response(
      check = if (!length(unknown)) TRUE else paste(
        "Must only contain declared alternatives"
      ),
      var_name = column_choice
    )
  }
  if (!is.null(column_choice) && identical(choice_type, "ordered") &&
      is.factor(data_frame[[column_choice]])) {
    oeli::input_check_response(
      check = if (is.ordered(data_frame[[column_choice]])) TRUE else paste(
        "Must be an ordered factor"
      ),
      var_name = column_choice
    )
  }
  esc <- function(x) gsub("([][{}()+*^$|\\.?*\\\\])", "\\\\\\1", x)
  alt_rx <- paste(esc(alternatives), collapse = "|")
  if (!is.null(column_choice) && column_choice %in% names(data_frame) &&
      identical(choice_type, "ranked")) {
    data_frame[[column_choice]] <- NULL
  }
  if (!is.null(column_choice) && identical(choice_type, "ranked")) {
    rank_pattern <- paste0(
      "^", esc(column_choice), esc(delimiter), "(", alt_rx, ")$"
    )
    rank_cols <- grep(rank_pattern, names(data_frame), value = TRUE)
    if (length(rank_cols) != length(alternatives)) {
      cli::cli_abort(
        "Ranked choice data in wide format must include a ranking column for
        each alternative.",
        call = NULL
      )
    }
    ranks <- as.matrix(data_frame[rank_cols])
    J <- length(alternatives)
    oeli::input_check_response(
      check = checkmate::check_integerish(
        ranks, lower = 1, upper = J, any.missing = TRUE
      ),
      var_name = "ranking columns"
    )
    valid_ranking <- apply(
      ranks,
      1,
      function(x) {
        x <- x[!is.na(x)]
        !length(x) || identical(sort(as.integer(x)), seq_along(x))
      }
    )
    oeli::input_check_response(
      check = if (all(valid_ranking)) TRUE else paste(
        "Observed ranks must be consecutive and start at one"
      ),
      var_name = "ranking columns"
    )
  }

  ### transform to long
  pat <- paste0("^(.+?)", esc(delimiter), "(", alt_rx, ")$")
  cols_to_pivot <- grep(pat, names(data_frame), value = TRUE)
  matches <- regmatches(
    cols_to_pivot,
    regexec(pat, cols_to_pivot)
  )
  if (length(matches)) {
    base_names <- vapply(matches, `[[`, character(1), 2)
    alt_names <- vapply(matches, `[[`, character(1), 3)
    alt_by_base <- split(alt_names, base_names)
    complete <- vapply(
      alt_by_base,
      function(x) identical(sort(x), sort(alternatives)),
      logical(1)
    )
    oeli::input_check_response(
      check = if (all(complete)) TRUE else paste(
        "Must provide one column per alternative for every covariate"
      ),
      var_name = "data_frame"
    )
    fixed_cols <- setdiff(names(data_frame), cols_to_pivot)
    conflicts <- intersect(unique(base_names), fixed_cols)
    if (identical(choice_type, "ranked")) {
      conflicts <- setdiff(conflicts, column_choice)
    }
    if (length(conflicts)) {
      cli::cli_abort(
        "Wide columns map to existing columns: {.val {conflicts}}.",
        call = NULL
      )
    }
  }
  if (length(cols_to_pivot) == 0L) {
    if (!is.null(column_choice) && identical(choice_type, "ranked")) {
      cli::cli_abort(
        "Ranked choice data in wide format must provide rankings for every
        alternative.",
        call = NULL
      )
    }
    ids <- rep(seq_len(nrow(data_frame)), each = length(alternatives))
    long <- data_frame[ids, , drop = FALSE]
    long[[column_alternative]] <- rep(alternatives, times = nrow(data_frame))
  } else {
    long <- tidyr::pivot_longer(
      data_frame,
      cols = tidyr::all_of(cols_to_pivot),
      names_to = c(".value", column_alternative),
      names_pattern = pat
    )
  }
  if (!is.null(column_choice)) {
    if (identical(choice_type, "ranked")) {
      long[[column_choice]] <- as.integer(long[[column_choice]])
    } else if (identical(choice_type, "discrete")) {
      long[[column_choice]] <- as.integer(
        long[[column_alternative]] == long[[column_choice]]
      )
      long <- dplyr::relocate(
        long, dplyr::all_of(column_alternative),
        .after = dplyr::all_of(column_choice)
      )
    }
  }
  tibble::as_tibble(long)
}

#' @noRd

check_as_covariates <- function(
  data_frame,
  format = "wide",
  column_choice = NULL,
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  column_ac_covariates = NULL,
  column_as_covariates = NULL,
  delimiter = "_",
  allow_missing_columns = character()
) {

  ### input checks
  check_not_missing(data_frame)
  check_format(format)
  check_column_choice(column_choice, null.ok = TRUE)
  check_column_decider(column_decider, null.ok = FALSE)
  check_column_occasion(column_occasion, column_decider, null.ok = TRUE)
  check_column_alternative(column_alternative, null.ok = format == "wide")
  if (format == "wide") column_alternative <- NULL
  check_column_covariates(
    column_ac_covariates, null.ok = TRUE, var_name = "column_ac_covariates"
  )
  check_column_covariates(
    column_as_covariates, null.ok = TRUE, var_name = "column_as_covariates"
  )
  check_delimiter(delimiter)
  check_data_frame(
    data_frame,
    forbidden_columns = if (format == "wide") column_alternative,
    required_columns = c(
      column_choice, column_decider, column_occasion, column_alternative
    ),
    allow_missing_columns = allow_missing_columns
  )

  esc <- function(x) gsub("([][{}()+*^$|\\.?*\\\\])", "\\\\\\1", x)
  column_ac <- character()
  column_as <- character()
  column_as_wide <- character()
  if (format == "long") {
    id_cols <- c(column_decider, column_occasion)
    id_cols <- id_cols[!vapply(id_cols, is.null, TRUE)]
    candidates <- setdiff(
      names(data_frame), c(id_cols, column_choice, column_alternative)
    )
    if (length(candidates) > 0) {
      as_flags <- vapply(
        candidates,
        function(cn) {
          sumry <- dplyr::summarise(
            dplyr::group_by(
              data_frame, dplyr::across(dplyr::all_of(id_cols)), .drop = FALSE
            ),
            n_alt = dplyr::n_distinct(.data[[column_alternative]]),
            n_val = dplyr::n_distinct(.data[[cn]]),
            .groups = "drop"
          )
          any(sumry$n_alt > 1 & sumry$n_val > 1, na.rm = TRUE)
        },
        logical(1)
      )
      column_as <- candidates[as_flags]
      column_ac <- setdiff(candidates, column_as)
    }
    alts <- data_frame[[column_alternative]] |>
      as.character() |> stats::na.omit() |> unique() |> sort()
    if (!is.null(column_as_covariates)) {
      missing_as <- setdiff(column_as_covariates, names(data_frame))
      if (length(missing_as)) {
        cli::cli_abort(
          "Unknown alternative-specific covariate(s): {.val {missing_as}}",
          call = NULL
        )
      }
      wrong_as <- setdiff(column_as_covariates, column_as)
      if (length(wrong_as)) {
        cli::cli_abort(
          "Found constant alternative-specific covariate(s): {.val {wrong_as}}",
          call = NULL
        )
      }
      column_as <- sort(unique(column_as_covariates))
    }
    if (!is.null(column_ac_covariates)) {
      missing_ac <- setdiff(column_ac_covariates, names(data_frame))
      if (length(missing_ac)) {
        cli::cli_abort(
          "Unknown alternative-constant covariate(s): {.val {missing_ac}}",
          call = NULL
        )
      }
      wrong_ac <- intersect(column_ac_covariates, column_as)
      if (length(wrong_ac)) {
        cli::cli_abort(
          "Found varying alternative-constant covariate(s): {.val {wrong_ac}}",
          call = NULL
        )
      }
      column_ac <- sort(unique(column_ac_covariates))
    }
    if (length(column_as) > 0 && length(alts) > 0) {
      column_as_wide <- as.vector(
        outer(column_as, alts, function(a, b) paste0(a, delimiter, b))
      )
    }
  } else {
    alternatives <- guess_alternatives_wide(
      data_frame,
      column_choice = column_choice,
      delimiter = delimiter,
      allow_missing_columns = allow_missing_columns
    )
    pattern <- paste0(
      "^(.+?)", esc(delimiter), "(",
      paste0(esc(alternatives), collapse = "|"), ")$"
    )
    matches <- pattern |>
      regexec(text = names(data_frame)) |>
      regmatches(x = names(data_frame))
    is_as <- vapply(matches, function(z) length(z) == 3L, logical(1))
    column_as_wide_detected <- names(data_frame)[is_as]
    base_vars_detected <- vapply(matches[is_as], function(z) z[2], character(1))
    column_as <- sort(unique(base_vars_detected))
    column_as_wide <- column_as_wide_detected
    id_cols <- c(column_decider, column_occasion)
    id_cols <- id_cols[!vapply(id_cols, is.null, TRUE)]
    column_ac <- setdiff(
      names(data_frame),
      c(id_cols, column_choice, column_as_wide_detected)
    ) |> sort()
    if (!is.null(column_as_covariates)) {
      alt_rx <- paste0("(", paste0(esc(alternatives), collapse = "|"), ")$")
      ok_as <- vapply(
        column_as_covariates,
        function(b) {
          grepl(
            paste0("^", esc(b), esc(delimiter), alt_rx),
            names(data_frame)
          ) |> any()
        },
        logical(1)
      )
      if (any(!ok_as)) {
        mb <- column_as_covariates[!ok_as]
        cli::cli_abort(
          "Alternative-specific columns for covariate(s) missing: {.val {mb}}",
          call = NULL
        )
      }
      column_as <- sort(unique(column_as_covariates))
      keep_pat <- paste0(
        "^(", paste0(esc(column_as), collapse = "|"), ")", esc(delimiter),
        "(", paste0(esc(alternatives), collapse = "|"), ")$"
      )
      column_as_wide <- grep(keep_pat, names(data_frame), value = TRUE)
    }
    if (!is.null(column_ac_covariates)) {
      missing_ac <- setdiff(column_ac_covariates, names(data_frame))
      if (length(missing_ac)) {
        cli::cli_abort(
          "Unknown alternative-constant covariate(s): {.val {missing_ac}}",
          call = NULL
        )
      }
      column_ac <- sort(unique(intersect(column_ac_covariates, column_ac)))
    }
  }
  list(
    "column_ac_covariates" = column_ac,
    "column_as_covariates" = column_as,
    "column_as_covariates_wide" = column_as_wide
  )
}
