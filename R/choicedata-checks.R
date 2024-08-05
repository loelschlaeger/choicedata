# The following functions are helper functions for input checks.
#
# The first argument is always the argument to be checked, potentially
# followed but additional arguments required for the check.
#
# Each check function throws an error if the check failed, otherwise it
# returns invisibly the first argument (except for the `check_consistency_*`
# functions, they return invisibly TRUE).

check_allow_missing <- function(allow_missing) {
  check_not_missing(allow_missing)
  check <- checkmate::check_flag(allow_missing)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var allow_missing} is bad: {check}", call = NULL)
  }
  invisible(allow_missing)
}

check_alternatives <- function(alternatives, J) {
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

check_as_cross_section <- function(as_cross_section) {
  check_not_missing(as_cross_section)
  check <- checkmate::check_flag(as_cross_section)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var as_cross_section} is bad: {check}", call = NULL)
  }
  invisible(as_cross_section)
}

check_base <- function(base, alternatives, J) {
  check_not_missing(base)
  check_alternatives(alternatives = alternatives, J = J)
  check <- checkmate::check_choice(base, choices = alternatives)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var base} is bad: {check}", call = NULL)
  }
  invisible(base)
}

check_C <- function(C, latent_classes) {
  check_not_missing(C)
  latent_classes <- check_latent_classes(latent_classes)
  check <- if (length(latent_classes) == 0) {
    if (C != 1) {
      "Must be equal to 1 (latent classes undefined)"
    } else {
      TRUE
    }
  } else {
    checkmate::check_int(C, lower = 2)
  }
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var C} is bad: {check}", call = NULL)
  }
  C <- as.integer(C)
  invisible(C)
}

check_choice_formula_ordered_valid <- function(choice_formula) {
  is.choice_formula(choice_formula, error = TRUE, var_name = "choice_formula")
  check <- if (isFALSE(choice_formula$ordered_valid)) {
    "In the ordered case, {.var formula} must be of the form
    {.code choice ~ 0 | A + B + 0} (i.e., no alternative-specific covariates and
    no ASCs)"
  } else {
    TRUE
  }
  if (!isTRUE(check)) {
    cli::cli_abort(paste("Input {.var choice_formula} is bad:", check), call = NULL)
  }
  invisible(choice_formula)
}

check_column_alternatives <- function(column_alternatives, na.ok = TRUE) {
  check_not_missing(column_alternatives)
  check <- checkmate::check_string(column_alternatives, min.chars = 1, na.ok = na.ok, null.ok = FALSE)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_alternatives} is bad: {check}", call = NULL)
  }
  invisible(column_alternatives)
}

check_column_choice <- function(column_choice, null.ok = TRUE) {
  check_not_missing(column_choice)
  check <- checkmate::check_string(column_choice, min.chars = 1, null.ok = null.ok)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_choice} is bad: {check}", call = NULL)
  }
  invisible(column_choice)
}

check_column_covariates_alternative_constant <- function(column_covariates_alternative_constant) {
  check_not_missing(column_covariates_alternative_constant)
  check <- checkmate::check_character(column_covariates_alternative_constant, any.missing = FALSE)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_covariates_alternative_constant} is bad: {check}", call = NULL)
  }
  invisible(column_covariates_alternative_constant)
}

check_column_covariates_alternative_varying <- function(column_covariates_alternative_varying) {
  check_not_missing(column_covariates_alternative_varying)
  check <- checkmate::check_character(column_covariates_alternative_varying, any.missing = FALSE)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_covariates_alternative_varying} is bad: {check}", call = NULL)
  }
  invisible(column_covariates_alternative_varying)
}

check_column_covariates <- function(column_covariates, len = NULL, null.ok = TRUE) {
  check_not_missing(column_covariates)
  if (isTRUE(null.ok) && is.null(column_covariates)) {
    return(invisible(NULL))
  }
  check <- checkmate::check_character(column_covariates, len = len)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_covariates} is bad: {check}", call = NULL)
  }
  invisible(column_covariates)
}

check_column_decider <- function(column_decider, null.ok = TRUE) {
  check_not_missing(column_decider)
  check <- checkmate::check_string(column_decider, min.chars = 1, null.ok = null.ok)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_decider} is bad: {check}", call = NULL)
  }
  if (identical(column_decider, "occasionID")) {
    cli::cli_abort(
      "Input {.var column_decider} must not equal {.val occasionID}",
      call = NULL
    )
  }
  invisible(column_decider)
}

check_column_occasion <- function(column_occasion, column_decider, null.ok = TRUE) {
  check_not_missing(column_occasion)
  check_column_decider(column_decider)
  check <- checkmate::check_string(column_occasion, min.chars = 1, null.ok = null.ok)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var column_occasion} is bad: {check}", call = NULL)
  }
  if (identical(column_decider, column_occasion)) {
    cli::cli_abort(
      "Inputs {.var column_decider} and {.var column_occasion} must be different",
      call = NULL
    )
  }
  invisible(column_occasion)
}

check_consistency <- function(
  choice_effects = NULL,
  choice_parameters = NULL
) {

  if (!is.null(choice_effects) && !is.null(choice_parameters)) {
    check_consistency_effects_parameters(
      choice_effects = choice_effects,
      choice_parameters = choice_parameters
    )
  }

  invisible(TRUE)
}

check_consistency_effects_parameters <- function(choice_effects, choice_parameters) {
  is.choice_effects(choice_effects, error = TRUE)
  is.choice_parameters(choice_parameters, error = TRUE)
  P_f <- sum(choice_effects$mixing == "none")
  P_r <- sum(choice_effects$mixing != "none")
  P <- P_f + P_r
  if (P_f > 0) {
    check <- checkmate::check_numeric(choice_parameters$alpha, len = P_f)
    if (!isTRUE(check)) {
      cli::cli_abort("...: {check}", call = NULL)
    }
  }
  # TODO
  invisible(TRUE)
}

check_data_frame <- function(data_frame, required_columns = character()) {
  check_not_missing(data_frame)
  checkmate::assert_character(required_columns)
  check <- checkmate::check_data_frame(data_frame)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var data_frame} is bad: {check}", call = NULL)
  }
  check <- checkmate::check_names(
    colnames(data_frame), must.include = required_columns, what = "colnames"
  )
  if (!isTRUE(check)) {
    cli::cli_abort("Columns of input {.var data_frame} are bad: {check}", call = NULL)
  }
  invisible(data_frame)
}

check_delimiter <- function(delimiter) {
  check_not_missing(delimiter)
  check <- checkmate::check_string(delimiter, n.chars = 1)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var delimiter} is bad: {check}", call = NULL)
  }
  invisible(delimiter)
}

check_error_term <- function(error_term, choices = c("logit", "probit")) {
  check_not_missing(error_term)
  check <- checkmate::check_choice(error_term, choices = choices)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var error_term} is bad: {check}", call = NULL)
  }
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
  check <- checkmate::check_formula(formula)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var formula} is bad: {check}", call = NULL)
  }
  invisible(formula)
}

check_J <- function(J) {
  check_not_missing(J)
  check <- checkmate::check_int(J, lower = 2)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var J} is bad: {check}", call = NULL)
  }
  invisible(J)
}

check_latent_classes <- function(latent_classes) {
  check_not_missing(latent_classes)
  if (is.null(latent_classes)) {
    latent_classes <- character()
  }
  check <- checkmate::check_names(latent_classes, type = "unique")
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var latent_classes} is bad: {check}", call = NULL)
  }
  invisible(latent_classes)
}

check_model_type <- function(model_type) {
  check_not_missing(model_type)
  check <- checkmate::check_choice(model_type, choices = c("probit", "logit"))
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var model_type} is bad: {check}", call = NULL)
  }
  invisible(model_type)
}

check_N <- function(N) {
  check_not_missing(N)
  check <- checkmate::check_int(N, lower = 1)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var N} is bad: {check}", call = NULL)
  }
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
  check <- checkmate::check_flag(ordered)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var ordered} is bad: {check}", call = NULL)
  }
  invisible(ordered)
}

check_ranked <- function(ranked) {
  check_not_missing(ranked)
  check <- checkmate::check_flag(ranked)
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var ranked} is bad: {check}", call = NULL)
  }
  invisible(ranked)
}

check_random_effects <- function(random_effects) {
  check_not_missing(random_effects)
  if (is.null(random_effects)) {
    random_effects <- character()
  }
  check <- checkmate::check_names(random_effects, type = "unique")
  if (!isTRUE(check)) {
    cli::cli_abort("Input {.var random_effects} is bad: {check}", call = NULL)
  }
  invisible(random_effects)
}

check_Tp <- function(Tp, N) {
  check_not_missing(Tp)
  check_N(N)
  check <- checkmate::check_int(Tp, lower = 1)
  if (!isTRUE(check)) {
    check <- checkmate::check_integerish(
      Tp, lower = 1, any.missing = FALSE, len = N
    )
    if (!isTRUE(check)) {
      cli::cli_abort("Input {.var Tp} is bad: {check}", call = NULL)
    }
  }
  invisible(Tp)
}
