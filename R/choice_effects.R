#' Define choice model effects
#'
#' @description
#' This function constructs an object of class `choice_effects`, which
#' defines the effects of a choice model.
#'
#' @param choice_formula \[`choice_formula`\]\cr
#' The \code{\link{choice_formula}} object that defines the choice effects.
#'
#' @param choice_alternatives \[`choice_alternatives`\]\cr
#' The \code{\link{choice_alternatives}} object that defines the choice effects.
#'
#' @param delimiter \[`character(1)`\]\cr
#' A delimiter between covariate and alternative name to build the effect name.
#'
#' @return
#' A `choice_effects` object, which is a `data.frame`, where each row
#' is a model effect, and columns are
#'
#' 1. `"effect_name"`, the name for the effect which is composed of
#'    covariate and alternative name,
#' 2. `"generic_name"`, the generic effect name (i.e., `"alpha_*"` for
#'    non-random effects and `"b_*"` for random effects),
#' 3. `"covariate"`, the covariate name connected to the effect,
#' 4. `"alternative"`, the alternative name connected to the effect (only
#'    if the effect is alternative-specific, i.e., varies across alternatives),
#' 5. `"as_covariate"`, indicator whether the covariate is alternative-specific,
#' 6. `"as_effect"`, indicator whether the effect is alternative-specific,
#' 7. `"mixing"`, a factor with levels
#'    - `"normal"` (normal distribution),
#'
#'    indicating the type of random effect.
#'
#' For identification, the effects are ordered according to the following rules:
#'
#' 1. Non-random effects come before random effects.
#' 2. Otherwise, the order is determined by occurrence in `formula`.
#'
#' It contains the arguments `choice_formula`, `choice_alternatives`, and
#' `delimiter` as attributes.
#'
#' @examples
#' choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | comfort,
#'     error_term = "probit",
#'     random_effects = c("price", "income")
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#'
#' @export

choice_effects <- function(
    choice_formula,
    choice_alternatives,
    delimiter = "_"
  ) {

  ### input checks
  is.choice_formula(choice_formula, error = TRUE, var_name = "choice_formula")
  is.choice_alternatives(
    choice_alternatives, error = TRUE, var_name = "choice_alternatives"
  )
  delimiter <- check_delimiter(delimiter)

  ### extract information
  alt <- as.character(choice_alternatives)
  J <- attr(choice_alternatives, "J")
  base <- attr(choice_alternatives, "base")
  var_types <- choice_formula$var_types
  mixing_types <- choice_formula$mixing_types

  ### build choice model effects
  overview <- data.frame(matrix(ncol = 7, nrow = 0))
  colnames(overview) <- c(
    "effect_name", "generic_name", "covariate", "alternative", "as_covariate",
    "as_effect", "mixing"
  )
  for (var in var_types[[1]]) {
    n <- nrow(overview)
    overview[n + 1, "effect_name"] <- var
    overview[n + 1, "generic_name"] <- NA_character_
    overview[n + 1, "covariate"] <- var
    overview[n + 1, "alternative"] <- NA_character_
    overview[n + 1, "as_covariate"] <- TRUE
    overview[n + 1, "as_effect"] <- FALSE
    overview[n + 1, "mixing"] <- mixing_types[var]
  }
  for (var in c(var_types[[2]], if (choice_formula$ASC) "ASC")) {
    for (j in (1:J)[-which(alt == base)]) {
      n <- nrow(overview)
      overview[n + 1, "effect_name"] <- paste(var, alt[j], sep = delimiter)
      overview[n + 1, "generic_name"] <- NA_character_
      overview[n + 1, "covariate"] <- if (var == "ASC") NA_character_ else var
      overview[n + 1, "alternative"] <- alt[j]
      overview[n + 1, "as_covariate"] <- FALSE
      overview[n + 1, "as_effect"] <- TRUE
      overview[n + 1, "mixing"] <- mixing_types[var]
    }
  }
  for (var in var_types[[3]]) {
    for (j in 1:J) {
      n <- nrow(overview)
      overview[n + 1, "effect_name"] <- paste(var, alt[j], sep = delimiter)
      overview[n + 1, "generic_name"] <- NA_character_
      overview[n + 1, "covariate"] <- var
      overview[n + 1, "alternative"] <- alt[j]
      overview[n + 1, "as_covariate"] <- TRUE
      overview[n + 1, "as_effect"] <- TRUE
      overview[n + 1, "mixing"] <- mixing_types[var]
    }
  }
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$mixing <- factor(
    overview$mixing, levels = c("normal"), ordered = TRUE
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
  P_f <- sum(is.na(overview$mixing))
  P_r <- nrow(overview) - P_f
  overview$generic_name <- c(
    paste0("alpha_", seq_len(P_f), recycle0 = TRUE),
    paste0("b_", seq_len(P_r), recycle0 = TRUE)
  )

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

is.choice_effects <- function(
    x,
    error = TRUE,
    var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_effects")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_effects}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_effects
#'
#' @param x \[`choice_effects`\]\cr
#' The `choice_effects` object to be printed.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_effects <- function(
    x,
    ...
  ) {
  is.choice_effects(x, error = TRUE)
  cli::cli_h3("Choice effects")
  print.data.frame(x)
}

#' Number of model effects
#'
#' @description
#' These helper functions count the number of model effects:
#'
#' - `compute_P()` returns the total number `P` of model effects.
#' - `compute_P_f()` returns the number `P_f` of non-random effects.
#' - `compute_P_r()` returns the number `P_r` of random effects.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @inheritSection choice_formula Choice models
#'
#' @return
#' An `integer`, the number of model effects.

compute_P <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  as.integer(P_f + P_r)
}

#' @rdname compute_P

compute_P_f <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  as.integer(sum(is.na(choice_effects$mixing)))
}

#' @rdname compute_P

compute_P_r <- function(choice_effects) {
  is.choice_effects(choice_effects, error = TRUE)
  as.integer(sum(!is.na(choice_effects$mixing)))
}

