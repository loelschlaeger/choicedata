#' Define choice model effects
#'
#' @description
#' This function constructs an object of class `choice_effects`, which
#' defines the effects of a choice model.
#'
#' @param choice_formula \[`choice_formula`\]\cr
#' A \code{\link{choice_formula}} object.
#'
#' @param choice_alternatives \[`choice_alternatives`\]\cr
#' A \code{\link{choice_alternatives}} object.
#'
#' @param choice_data \[`NULL` | `choice_data`\]\cr
#' A \code{\link{choice_data}} object.
#'
#' Required to resolve data-dependent elements in `choice_formula` (if any).
#'
#' @param delimiter \[`character(1)`\]\cr
#' The delimiter between covariate and alternative name.
#'
#' @return
#' A `choice_effects` object, which is a `data.frame`, where each row
#' is a model effect, and columns are
#'
#' 1. `"effect_name"`, the name for the effect which is composed of
#'    covariate and alternative name,
#' 2. `"generic_name"`, the generic effect name `"beta_<effect number>"`,
#' 3. `"covariate"`, the (transformed) covariate name connected to the effect,
#' 4. `"alternative"`, the alternative name connected to the effect (only
#'    if the effect is alternative-specific),
#' 5. `"as_covariate"`, indicator whether the covariate is
#'    alternative-specific,
#' 6. `"as_effect"`, indicator whether the effect is alternative-specific,
#' 7. `"mixing"`, a factor with levels in the order
#'
#'    1. `"cn"` (correlated normal distribution),
#'    2. `"cln"` (positively signed correlated log-normal distribution),
#'    3. `"cln-"` (negatively signed correlated log-normal distribution),
#'    4. `"n"` (uncorrelated normal distribution),
#'    5. `"ln"` (positively signed uncorrelated log-normal distribution),
#'    6. `"ln-"` (negatively signed uncorrelated log-normal distribution),
#'
#'    indicating the type of random effect.
#'
#' For identification, the choice effects are ordered according to the
#' following rules:
#'
#' 1. Non-random effects come before random effects.
#' 2. Otherwise, the order is determined by occurrence in `formula`.
#'
#' It contains the arguments `choice_formula`, `choice_alternatives`, and
#' `delimiter` as attributes.
#'
#' @export
#'
#' @keywords model
#'
#' @examples
#' ### construct choice effects
#' choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | I(comfort == 1),
#'     error_term = "probit",
#'     random_effects = c(
#'       "price" = "cn",
#'       "income" = "cn"
#'      )
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )

choice_effects <- function(
  choice_formula,
  choice_alternatives,
  choice_data = NULL,
  delimiter = "_"
) {

  ### input checks
  is.choice_formula(choice_formula, error = TRUE, var_name = "choice_formula")
  is.choice_alternatives(
    choice_alternatives, error = TRUE, var_name = "choice_alternatives"
  )
  delimiter <- check_delimiter(delimiter)

  ### resolve choice formula if data is available
  if (!is.null(choice_data)) {
    choice_formula <- resolve_choice_formula(
      choice_formula = choice_formula,
      x = choice_data,
      choice_alternatives = choice_alternatives
    )
  }

  ### extract information
  alt <- as.character(choice_alternatives)
  base <- attr(choice_alternatives, "base")
  ordered_alternatives <- isTRUE(attr(choice_alternatives, "ordered"))
  covariate_types <- choice_formula$covariate_types
  random_effects <- choice_formula$random_effects

  if (isTRUE(ordered_alternatives)) {
    if (choice_formula$ASC || length(covariate_types[[2]]) > 0 ||
        length(covariate_types[[3]]) > 0) {
      cli::cli_abort(
        c(
          "Ordered choice models only support covariates in the first part of
          {.var formula} without alternative-specific constants.",
          "i" = "Use, e.g., {.code choice ~ age + income | 0}."
        ),
        call = NULL
      )
    }
  }

  ### build choice model effects
  overview <- data.frame(
    effect_name = character(),
    generic_name = character(),
    covariate = character(),
    alternative = character(),
    as_covariate = logical(),
    as_effect = logical(),
    mixing = character(),
    stringsAsFactors = FALSE
  )
  for (var in covariate_types[[1]]) {
    overview <- append_effects(
      overview = overview,
      covariate = var,
      alternatives = NA_character_,
      as_covariate = TRUE,
      as_effect = FALSE,
      mixing = random_effects[var],
      delimiter = delimiter
    )
  }
  alternative_specific_covariates <- c(
    covariate_types[[2]],
    if (choice_formula$ASC) "ASC"
  )
  non_base_alternatives <- if (is.null(base) || isTRUE(is.na(base))) {
    alt
  } else {
    alt[alt != base]
  }
  for (var in alternative_specific_covariates) {
    overview <- append_effects(
      overview = overview,
      covariate = var,
      alternatives = non_base_alternatives,
      as_covariate = FALSE,
      as_effect = TRUE,
      mixing = random_effects[var],
      delimiter = delimiter,
      covariate_label = if (identical(var, "ASC")) NA_character_ else var
    )
  }
  for (var in covariate_types[[3]]) {
    overview <- append_effects(
      overview = overview,
      covariate = var,
      alternatives = alt,
      as_covariate = TRUE,
      as_effect = TRUE,
      mixing = random_effects[var],
      delimiter = delimiter
    )
  }
  oeli::input_check_response(
    check = checkmate::check_character(overview$effect_name, unique = TRUE),
    var_name = "effect names"
  )
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$mixing <- factor(
    overview$mixing,
    levels = c("cn", "cln", "cln-", "n", "ln", "ln-"),
    ordered = TRUE
  )

  ### sort effects
  effect_order <- order(
    !is.na(overview$mixing),        # non-random effects before random effects
    as.numeric(rownames(overview)), # otherwise sort by occurrence in formula
    decreasing = FALSE,
    na.last = FALSE
  )
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL

  ### add generic names
  P <- nrow(overview)
  overview$generic_name <- paste0("beta_", seq_len(P), recycle0 = TRUE)

  ### return effects
  structure(
    overview,
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    delimiter = delimiter,
    class = c("choice_effects", "data.frame")
  )
}

#' @noRd

append_effects <- function(
  overview, covariate, alternatives, as_covariate, as_effect, mixing, delimiter,
  covariate_label = covariate
) {
  mixing <- if (length(mixing) == 0L || isTRUE(all(is.na(mixing)))) {
    NA_character_
  } else {
    unname(as.character(mixing))[1]
  }
  for (alternative in alternatives) {
    effect_name <- if (isTRUE(is.na(alternative))) {
      covariate
    } else {
      paste(covariate, alternative, sep = delimiter)
    }
    new_row <- data.frame(
      effect_name = effect_name,
      generic_name = NA_character_,
      covariate = if (isTRUE(is.na(covariate_label))) {
        NA_character_
      } else {
        covariate_label
      },
      alternative = if (isTRUE(is.na(alternative))) {
        NA_character_
      } else {
        alternative
      },
      as_covariate = as.logical(as_covariate),
      as_effect = as.logical(as_effect),
      mixing = if (isTRUE(is.na(mixing))) {
        NA_character_
      } else {
        mixing
      },
      stringsAsFactors = FALSE
    )
    rownames(new_row) <- NULL
    overview <- rbind(overview, new_row)
  }
  overview
}

#' @noRd

random_effect_distribution <- function(mixing) {
  sub("^c", "", as.character(mixing))
}

#' @noRd

is.choice_effects <- function(
    x, error = TRUE, var_name = oeli::variable_name(x)
  ) {
  check_choice_object(
    x = x,
    class_name = "choice_effects",
    error = error,
    var_name = var_name
  )
}

#' @rdname choice_effects
#'
#' @param x \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_effects <- function(x, ...) {
  is.choice_effects(x, error = TRUE)
  cli::cli_h3("Choice effects")
  print.data.frame(x)
}

#' Count model effects
#'
#' @description
#' `compute_P()` returns the total number of effects in a choice model.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' A \code{\link{choice_effects}} object.
#'
#' @return
#' An integer with the number of model effects.
#'
#' @keywords model
#'
#' @export

compute_P <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  P_d <- compute_P_d(choice_effects)
  P_r <- compute_P_r(choice_effects)
  as.integer(P_d + P_r)
}

#' @noRd

compute_P_d <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  as.integer(sum(is.na(choice_effects$mixing)))
}

#' @noRd

compute_P_r <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  as.integer(sum(!is.na(choice_effects$mixing)))
}
