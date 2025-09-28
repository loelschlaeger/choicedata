#' Define choice preferences
#'
#' @description
#' The `choice_preferences` object defines the deciders' preferences in the
#' choice model.
#'
#' - `choice_preferences()` constructs a `choice_preferences` object.
#' - `generate_choice_preferences()` samples choice preferences at random.
#'
#' @param data_frame \[`data.frame`\]\cr
#' Contains the deciders' preferences.
#'
#' @param column_decider \[`character(1)` | `NULL`\]\cr
#' The column name of `data_frame` with the decider identifiers.
#' If `NULL`, decider identifiers are generated.
#'
#'
#' @return
#' An object of class `choice_preferences`, which is a `data.frame` with the
#' deciders' preferences. The column names are the names of the effects in the
#' choice model. The first column contains the decider identifiers.
#'
#' @export
#'
#' @keywords model
#'
#' @examples
#' ### generate choice preferences from choice parameters and effects
#' choice_effects <- choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | comfort,
#'     error_term = "probit",
#'     random_effects = c(
#'       "price" = "cn",
#'       "income" = "cn"
#'     )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#'
#' choice_parameters <- generate_choice_parameters(
#'   choice_effects = choice_effects
#' )
#'
#' ids <- generate_choice_identifiers(N = 4)
#'
#' (choice_preferences <- generate_choice_preferences(
#'   choice_parameters = choice_parameters,
#'   choice_effects = choice_effects,
#'   choice_identifiers = ids
#' ))
#'
#' ### inspect decider-specific preference vectors
#' head(choice_preferences)

choice_preferences <- function(
    data_frame, column_decider = colnames(data_frame)[1]
  ) {

  ### input checks
  check_not_missing(data_frame)
  check_column_decider(column_decider, null.ok = TRUE)
  check_data_frame(data_frame, required_columns = column_decider)

  ### include decider identifiers if missing
  if (is.null(column_decider)) {
    choice_identifiers <- generate_choice_identifiers(N = nrow(data_frame))
    column_decider <- attr(choice_identifiers, "column_decider")
    data_frame <- cbind(choice_identifiers[column_decider], data_frame)
  }

  ### ensure that 'column_decider' is first column
  column_order <- c(column_decider, setdiff(colnames(data_frame), column_decider))
  data_frame <- data_frame[, column_order]

  ### build 'choice_preferences' object
  structure(
    data_frame,
    class = c("choice_preferences", "data.frame"),
    column_decider = column_decider
  )
}

#' @noRd

is.choice_preferences <- function(
    x,
    error = FALSE,
    var_name = oeli::variable_name(x)
  ) {
  validate_choice_object(
    x = x,
    class_name = "choice_preferences",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_preferences
#'
#' @param choice_parameters \[`choice_parameters`\]\cr
#' A \code{\link{choice_parameters}} object.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' A \code{\link{choice_identifiers}} object.
#'
#' @export

generate_choice_preferences <- function(
    choice_effects,
    choice_parameters = NULL,
    choice_identifiers = generate_choice_identifiers(N = 100)
  ) {

  if (missing(choice_parameters)) {
    choice_parameters <- generate_choice_parameters(
      choice_effects = choice_effects
    )
  }

  ### input checks
  check_not_missing(choice_effects)
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  choice_parameters <- validate_choice_parameters(
    choice_parameters = choice_parameters,
    choice_effects = choice_effects,
    allow_missing = FALSE
  )
  is.choice_identifiers(choice_identifiers, error = TRUE)
  Tp <- read_Tp(choice_identifiers)
  N <- length(Tp)
  P <- compute_P(choice_effects)
  P_r <- compute_P_r(choice_effects)

  ### extract (unique) decider identifiers
  decider_identifiers <- get_decider_identifiers(choice_identifiers)
  column_decider <- attr(choice_identifiers, "column_decider")

  preferences <- as.data.frame(matrix(NA_real_, nrow = N, ncol = P))
  colnames(preferences) <- choice_effects[, "effect_name"]
  re_position <- utils::tail(seq_len(P), P_r)
  Omega_completed <- matrix(0, nrow = P, ncol = P)
  Omega_completed[re_position, re_position] <- choice_parameters$Omega
  for (n in seq_len(N)) {
    preferences[n, ] <- oeli::rmvnorm(
      mean = choice_parameters$beta,
      Sigma = Omega_completed
    )
  }

  ### build 'choice_preferences' object
  data_frame <- cbind(decider_identifiers, preferences)
  colnames(data_frame)[1] <- column_decider
  choice_preferences(
    data_frame = data_frame,
    column_decider = column_decider
  )

}

#' @noRd

split_choice_preferences <- function(
    choice_preferences,
    choice_identifiers = NULL
  ) {

  ### input checks
  is.choice_preferences(choice_preferences, error = TRUE)
  if (!is.null(choice_identifiers)) {
    is.choice_identifiers(choice_identifiers, error = TRUE)
  }

  ### extract preference columns
  column_decider <- attr(choice_preferences, "column_decider")
  if (is.null(column_decider)) {
    column_decider <- colnames(choice_preferences)[1]
  }
  pref_cols <- setdiff(colnames(choice_preferences), column_decider)
  preferences <- as.matrix(choice_preferences[, pref_cols, drop = FALSE])
  rownames(preferences) <- choice_preferences[[column_decider]]

  ### determine decider order
  decider_ids <- if (is.null(choice_identifiers)) {
    rownames(preferences)
  } else {
    get_decider_identifiers(choice_identifiers)
  }
  idx <- match(decider_ids, rownames(preferences))
  if (anyNA(idx)) {
    missing_ids <- decider_ids[is.na(idx)]
    cli::cli_abort(
      "Missing preference vectors for deciders {.val {missing_ids}}.",
      call = NULL
    )
  }

  ### split preferences into list by decider
  effect_names <- colnames(preferences)
  lapply(idx, function(i) {
    prefs <- preferences[i, , drop = TRUE]
    if (!length(prefs)) {
      numeric(0)
    } else {
      stats::setNames(as.numeric(prefs), effect_names)
    }
  })
}

