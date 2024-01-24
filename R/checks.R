check_not_missing <- function(x, error = TRUE) {
  check <- !missing(x)
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort("Please specify the input {.var {var_name}}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_delimiter <- function(delimiter, error = TRUE) {
  check <- check_not_missing(delimiter, error = error) &&
    checkmate::check_string(delimiter, n.chars = 1)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var delimiter} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_formula <- function(formula, error = TRUE) {
  check <- check_not_missing(formula, error = error) &&
    checkmate::check_formula(formula)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var formula} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_re <- function(re, error = TRUE) {
  check <- check_not_missing(re, error = error) &&
    checkmate::check_character(
      re, any.missing = FALSE, unique = TRUE, null.ok = TRUE
    )
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var re} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_ordered <- function(ordered, error = TRUE) {
  check <- check_not_missing(ordered, error = error) &&
    checkmate::check_flag(ordered)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var ordered} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_J <- function(J, ordered, error = TRUE) {
  check <- check_not_missing(J, error = error) &&
    check_ordered(ordered, error = error) &&
    checkmate::check_int(J, lower = 2 + ordered)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var J} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_alternatives <- function(alternatives, J, ordered, error = TRUE) {
  check <- check_not_missing(alternatives, error = error) &&
    check_J(J, ordered, error = error) &&
    checkmate::check_character(
      alternatives, any.missing = FALSE, len = J, unique = TRUE
    )
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var alternatives} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_base <- function(base, alternatives, J, ordered, error = TRUE) {
  check <- check_not_missing(base, error = error) &&
    check_alternatives(
      alternatives = alternatives, J = J, ordered = ordered, error = error
    ) && checkmate::check_choice(base, choices = alternatives)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var base} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_model_type <- function(model_type, error = TRUE) {
  check <- check_not_missing(model_type, error = error) &&
    checkmate::check_choice(model_type, choices = c("probit", "logit"))
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var model_type} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_latent_classes <- function(latent_classes, error = TRUE) {
  check <- check_not_missing(latent_classes, error = error) &&
    checkmate::check_choice(
      latent_classes, choices = c("none", "fe", "re", "both")
    )
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var latent_classes} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_C <- function(C, latent_classes, error = TRUE) {
  check <- check_not_missing(C, error = error) &&
    check_latent_classes(latent_classes, error = error) &&
    if (latent_classes == "none") {
      checkmate::check_int(C, lower = 1, upper = 1)
    } else {
      checkmate::check_int(C, lower = 1)
    }
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var C} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_N <- function(N, error = TRUE) {
  check <- check_not_missing(N, error = error) && checkmate::check_int(N, lower = 1)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var N} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}

check_allow_missing <- function(allow_missing, error = TRUE) {
  check <- check_not_missing(allow_missing, error = error) &&
    checkmate::check_flag(allow_missing)
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort("Input {.var allow_missing} is bad: {check}", call = NULL)
  } else {
    isTRUE(check)
  }
}


check_model_consistent <- function(

) {

}

check_model_ordered <- function() {

}

check_model_ranked <- function() {

}
