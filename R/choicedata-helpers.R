#' Documentation helper
#'
#' @name doc-helper
#'
#' @param choice_alternatives
#' A \code{\link{choice_alternatives}} object.
#' @param choice_covariates
#' A \code{\link{choice_covariates}} object.
#' @param choice_formula
#' A \code{\link{choice_formula}} object.
#' @param choice_parameters
#' A \code{\link{choice_parameters}} object.
#' @param ranked
#' Either \code{TRUE} for ranked choices or \code{FALSE} (default), else.
#' @param x
#' An object of the corresponding class.
#' @param ...
#' Currently not used.
#'
#' @keywords internal

NULL

#' @keywords internal

covariate_spec_sugar <- function(
    covariate_spec, choice_formula, choice_alternatives, delimiter, named_vector
) {

  ### syntactic sugar for selecting alternative-specific covariate
  checkmate::assert_flag(named_vector)
  if (named_vector) {
    checkmate::assert_vector(covariate_spec, any.missing = FALSE, names = "strict")
  } else {
    checkmate::assert_character(covariate_spec, any.missing = FALSE, unique = TRUE)
  }
  covariate_spec_input <- covariate_spec
  choice_effects <- choice_effects(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )

  ### quick and dirty check if alternative-specific covariate has been specified
  ### without alternative label
  for (i in seq_along(covariate_spec)) {

    cov_name_i <- ifelse(named_vector, names(covariate_spec)[i], covariate_spec[i])

    if (!cov_name_i %in% covariate_names) {
      if (cov_name_i %in% choice_formula$var_types[c(1, 3)]) {

        ### extend covariate name by all alternative labels
        cov_name_i_extended <- paste(
          cov_name_i, as.character(choice_alternatives), sep = delimiter
        )

        if (named_vector) {

          ### remove redundancies
          cov_name_i_extended <- setdiff(cov_name_i_extended, names(covariate_spec))

          ### add covariate name with alternative labels
          covariate_spec_input[cov_name_i_extended] <- covariate_spec_input[cov_name_i]

          ### remove bad covariate specification without alternative label
          covariate_spec_input <- covariate_spec_input[names(covariate_spec_input) != cov_name_i]

        } else {

          ### remove redundancies
          cov_name_i_extended <- setdiff(cov_name_i_extended, covariate_spec)

          covariate_spec_input <- c(covariate_spec_input, cov_name_i_extended)
          covariate_spec_input <- covariate_spec_input[covariate_spec_input != cov_name_i]

        }
      }
    }
  }

  if (!named_vector) {
    covariate_spec_input <- unique(covariate_spec_input)
  }

  return(covariate_spec_input)
}

#' @keywords internal

effect_is_ASC <- function(effect_name, delimiter) {
  checkmate::assert_string(effect_name)
  checkmate::assert_string(delimiter, n.chars = 1)
  startsWith(effect_name, paste0("ASC", delimiter))
}





#' @keywords internal

check_covariate_levels <- function(
    covariate_levels, choice_formula, choice_alternatives, delimiter
) {
  checkmate::assert_numeric(
    covariate_levels, finite = FALSE, lower = 1, any.missing = FALSE
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  covariate_levels <- round(covariate_levels, digits = 0)
  if (checkmate::test_named(covariate_levels, type = "strict")) {
    covariate_levels <- covariate_spec_sugar(
      covariate_spec = covariate_levels, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
    checkmate::assert_subset(names(covariate_levels), covariate_names)
    covariate_levels_input <- covariate_levels
    covariate_levels <- rep(Inf, length.out = covariate_number)
    names(covariate_levels) <- covariate_names
    covariate_levels[names(covariate_levels_input)] <- covariate_levels_input
  } else {
    checkmate::assert_number(covariate_levels, lower = 1, finite = FALSE)
    covariate_levels <- rep(covariate_levels, length.out = covariate_number)
    names(covariate_levels) <- covariate_names
  }
  return(covariate_levels)
}

#' @keywords internal

check_occasion_constant <- function(
    occasion_constant, choice_formula, choice_alternatives, delimiter
) {
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  occasion_constant <- covariate_spec_sugar(
    covariate_spec = occasion_constant, choice_formula = choice_formula,
    choice_alternatives = choice_alternatives, delimiter = delimiter,
    named_vector = FALSE
  )
  checkmate::assert_subset(
    occasion_constant, covariate_names, empty.ok = TRUE
  )
  return(occasion_constant)
}

check_covariate_mean <- function(
    covariate_mean, choice_formula, choice_alternatives, delimiter
) {
  checkmate::assert_numeric(covariate_mean, finite = TRUE, any.missing = FALSE)
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_named(covariate_mean, type = "strict")) {
    covariate_mean <- covariate_spec_sugar(
      covariate_spec = covariate_mean, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
    checkmate::assert_subset(names(covariate_mean), covariate_names)
    covariate_mean_input <- covariate_mean
    covariate_mean <- rep(0, length.out = covariate_number)
    names(covariate_mean) <- covariate_names
    covariate_mean[names(covariate_mean_input)] <- covariate_mean_input
  } else {
    checkmate::assert_number(covariate_mean, finite = TRUE)
    covariate_mean <- rep(covariate_mean, length.out = covariate_number)
    names(covariate_mean) <- covariate_names
  }
  return(covariate_mean)
}

check_covariate_sd <- function(
    covariate_sd, choice_formula, choice_alternatives, delimiter
) {
  checkmate::assert_numeric(
    covariate_sd, finite = TRUE, lower = 0, any.missing = FALSE
  )
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_named(covariate_sd, type = "strict")) {
    covariate_sd <- covariate_spec_sugar(
      covariate_spec = covariate_sd, choice_formula = choice_formula,
      choice_alternatives = choice_alternatives, delimiter = delimiter,
      named_vector = TRUE
    )
    checkmate::assert_subset(names(covariate_sd), covariate_names)
    covariate_sd_input <- covariate_sd
    covariate_sd <- rep(1, length.out = covariate_number)
    names(covariate_sd) <- covariate_names
    covariate_sd[names(covariate_sd_input)] <- covariate_sd_input
  } else {
    checkmate::assert_number(covariate_sd, lower = 0, finite = TRUE)
    covariate_sd <- rep(covariate_sd, length.out = covariate_number)
    names(covariate_sd) <- covariate_names
  }
  return(covariate_sd)
}

check_covariate_correlation <- function(
    covariate_correlation, choice_formula, choice_alternatives, delimiter
) {
  covariate_names <- covariate_names(
    choice_formula = choice_formula, choice_alternatives = choice_alternatives,
    delimiter = delimiter
  )
  covariate_number <- length(covariate_names)
  if (checkmate::test_number(covariate_correlation)) {
    if (covariate_correlation < -1 || covariate_correlation > 1) {
      stop("correlation must be between -1 and 1", call. = FALSE)
    }
    covariate_correlation_input <- covariate_correlation
    covariate_correlation <- diag(covariate_number)
    covariate_correlation[row(covariate_correlation) != col(covariate_correlation)] <- covariate_correlation_input
  }
  oeli::assert_correlation_matrix(covariate_correlation, dim = covariate_number)
  return(covariate_correlation)
}



















