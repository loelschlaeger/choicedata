# The following functions are helper functions for input checks.
#
# The first argument is always the argument to be checked, potentially
# followed but additional arguments required for the check.
#
# Each check function throws an error if the check failed, otherwise it
# returns invisibly the first argument.

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
    alternatives, any.missing = FALSE, len = J, unique = TRUE
  )
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var alternatives} is bad: {check}", call = NULL)
  }
  invisible(alternatives)
}

check_cross_section <- function(cross_section) {
  check_not_missing(cross_section)
  oeli::input_check_response(
    check = checkmate::check_flag(cross_section),
    var_name = "cross_section"
  )
  invisible(cross_section)
}

check_base <- function(base, alternatives, J) {
  check_not_missing(base)
  alternatives <- check_alternatives(alternatives = alternatives, J = J)
  oeli::input_check_response(
    check = checkmate::check_choice(base, choices = alternatives),
    var_name = "base"
  )
  invisible(base)
}

check_choice_only <- function(choice_only) {
  check_not_missing(choice_only)
  oeli::input_check_response(
    check = checkmate::check_flag(choice_only),
    var_name = "choice_only"
  )
  invisible(choice_only)
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

check_column_effects <- function(column_effects, len = NULL, null.ok = TRUE) {
  check_not_missing(column_effects)
  oeli::input_check_response(
    check = checkmate::check_character(
      column_effects, any.missing = FALSE, len = len, null.ok = null.ok
    ),
    var_name = "column_effects"
  )
  if (!is.null(column_effects)) {
    oeli::input_check_response(
      check = checkmate::check_names(column_effects, type = "strict"),
      var_name = "column_effects"
    )
  }
  invisible(column_effects)
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
    data_frame, required_columns = character(), forbidden_columns = character()
  ) {
  check_not_missing(data_frame)
  checkmate::assert_character(required_columns, null.ok = TRUE)
  checkmate::assert_character(forbidden_columns, null.ok = TRUE)
  oeli::input_check_response(
    check = checkmate::check_data_frame(data_frame),
    var_name = "data_frame"
  )
  oeli::input_check_response(
    check = checkmate::check_names(
      colnames(data_frame), must.include = required_columns,
      disjunct.from = forbidden_columns, what = "colnames"
    ),
    var_name = "data_frame"
  )
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

check_list <- function(x, types = character(0), len = NULL) {
  check_not_missing(x)
  oeli::input_check_response(
    check = checkmate::check_list(x, types = types, len = len),
    var_name = "x"
  )
  invisible(x)
}

check_N <- function(N) {
  check_not_missing(N)
  oeli::input_check_response(
    check = checkmate::check_int(N, lower = 1),
    var_name = "N"
  )
  invisible(N)
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
