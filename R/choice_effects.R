#' Define choice model effects
#'
#' @description
#' This function constructs an object of class \code{choice_effects}, which
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
#' A \code{choice_effects} object, which is a \code{data.frame}, where each row
#' is a model effect, and columns are
#' 1. \code{"effect_name"}, the name for the effect which is composed of
#'    covariate and alternative name,
#' 2. \code{"generic_name"}, the generic effect name, i.e. \code{"alpha_*"}
#'    for non-random effects and \code{"b_*"} for random effects,
#' 3. \code{"covariate"}, the covariate name connected to the effect,
#' 4. \code{"alternative"}, the alternative name connected to the effect (only
#'    if the effect is alternative-specific, i.e., varies across alternatives),
#' 5. \code{"as_covariate"}, indicator whether the covariate is
#'    alternative-specific,
#' 6. \code{"as_effect"}, indicator whether the effect is alternative-specific,
#' 7. \code{"lc_effect"}, indicator whether the effect has latent classes,
#' 8. \code{"mixing"}, a factor with levels \code{"none"}, \code{"normal"},
#'    and \code{"log-normal"}, indicating the type of random effect.
#'
#' For identification, the effects are ordered as follows:
#'
#' - Non-random effects come before random effects.
#' - Normal random effects come before log-normal random effects.
#' - Otherwise, the order is determined by occurrence in \code{formula}.
#'
#' It contains the arguments `choice_formula` and `choice_alternatives`
#' as attributes.
#'
#' @examples
#' choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | comfort, error_term = "probit",
#'     random_effects = c("price+", "income")
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#'
#' @export

choice_effects <- function(
    choice_formula, choice_alternatives, delimiter = "_"
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
  ordered <- attr(choice_alternatives, "ordered")
  var_types <- choice_formula$var_types
  mixing_types <- choice_formula$mixing_types
  latent_classes <- choice_formula$latent_classes

  ### build choice model effects
  overview <- data.frame(matrix(ncol = 8, nrow = 0))
  if (ordered) {
    check_choice_formula_ordered_valid(choice_formula)
    for (var in var_types[[2]]) {
      overview <- rbind(
        overview,
        c(
          var, NA_character_, var, NA_character_, FALSE, FALSE,
          var %in% latent_classes, mixing_types[var]
        )
      )
    }
  } else {
    for (var in var_types[[1]]) {
      overview <- rbind(
        overview,
        c(
          var, NA_character_, var, NA_character_, TRUE, FALSE,
          var %in% latent_classes, mixing_types[var]
        )
      )
    }
    for (var in c(var_types[[2]], if (choice_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alt == base)]) {
        var_name <- paste(var, alt[j], sep = delimiter)
        overview <- rbind(
          overview,
          c(
            var_name, NA_character_, if (var == "ASC") NA_character_ else var,
            alt[j], FALSE, TRUE, var %in% latent_classes, mixing_types[var]
          )
        )
      }
    }
    for (var in var_types[[3]]) {
      for (j in 1:J) {
        var_name <- paste(var, alt[j], sep = delimiter)
        overview <- rbind(
          overview,
          c(
            var_name, NA_character_, var, alt[j], TRUE, TRUE,
            var %in% latent_classes, mixing_types[var]
          )
        )
      }
    }
  }
  colnames(overview) <- c(
    "effect_name", "generic_name", "covariate", "alternative", "as_covariate",
    "as_effect", "lc_effect", "mixing"
  )
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$lc_effect <- as.logical(overview$lc_effect)
  overview$mixing <- factor(
    overview$mixing, levels = c("normal", "log-normal"), ordered = TRUE
  )

  ### sort effects:
  ### - random effects last, log-normal behind normal
  ### - otherwise sort by occurrence in formula
  effect_order <- order(
    overview$mixing,
    as.numeric(rownames(overview)),
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
    x, error = TRUE, var_name = oeli::variable_name(x)
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

print.choice_effects <- function(x, ...) {
  is.choice_effects(x, error = TRUE)
  cli::cli_h3("Choice effects")
  print.data.frame(x)
}

#' Number of model effects
#'
#' @description
#' These helper functions count the number of model effects:
#'
#' - \code{compute_P()} returns the total number \code{P} of model effects.
#' - \code{compute_P_f()} returns the number \code{P_f} of non-random effects.
#' - \code{compute_P_r()} returns the number \code{P_r} of random effects.
#'
#' @param choice_effects \[`choice_effects`\]\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @inheritSection choice_formula The probit and logit model
#'
#' @return
#' An \code{integer}, the number of model effects.

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

