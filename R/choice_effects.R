#' Define choice model effects
#'
#' @description
#' This function constructs an object of class \code{\link{choice_effects}},
#' which defines the choice model effects.
#'
#' @inheritParams doc-helper
#' @param delimiter
#' A \code{character}, the delimiter between covariate and alternative name
#' to build the effect name.
#' By default, \code{delimiter = "_"}.
#'
#' @inheritSection choice_formula Model formula
#' @inheritSection choice_formula Random effects
#' @inheritSection choice_alternatives Base alternative
#'
#' @return
#' A \code{\link{choice_effects}} object, which is a \code{data.frame}, where
#' each row is an effect, and columns are
#' 1. \code{"name"}, the effect name (composed of covariate and alternative
#'    name),
#' 2. \code{"covariate"}, the covariate name,
#' 3. \code{"alternative"}, the alternative name (only if the effect is
#'    alternative-specific, i.e., varies across alternatives),
#' 4. \code{"as_covariate"}, indicator whether the covariate is
#'    alternative-specific,
#' 5. \code{"as_effect"}, indicator whether the effect is alternative-specific,
#' 6. \code{"mixing"}, a factor with levels \code{"none"}, \code{"normal"},
#'    and \code{"log-normal"}, indicating the type (if any) of the mixing
#'
#' The effects are ordered as follows: Fixed effects come before random effects,
#' and log-normal random effects are last random effects. Otherwise, the order
#' is determined by occurrence in \code{formula}.
#'
#' @examples
#' choice_effects(
#'   choice_formula = choice_formula(
#'     formula = choice ~ price | income | comfort,
#'     re = c("price+", "income")
#'   ),
#'   choice_alternatives = choice_alternatives(J = 3)
#' )
#'
#' @export

choice_effects <- function(
    choice_formula, choice_alternatives, delimiter = "_"
  ) {

  ### input checks
  is.choice_formula(choice_formula, error = TRUE)
  is.choice_alternatives(choice_alternatives, error = TRUE)
  check_delimiter(delimiter)

  ### extract information
  J <- choice_alternatives$J
  alt <- choice_alternatives$alternatives
  base <- choice_alternatives$base
  ordered <- choice_alternatives$ordered
  var_types <- choice_formula$var_types

  ### helper function to determine mixing type
  mixing_type <- function(var) {
    if (var %in% choice_formula$re_n) {
      "normal"
    } else if (var %in% choice_formula$re_ln) {
      "log-normal"
    } else {
      "none"
    }
  }

  ### build choice model effects
  overview <- data.frame(matrix(ncol = 6, nrow = 0))
  if (ordered){
    for (var in var_types[[2]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, FALSE, FALSE, mixing_type(var))
      )
    }
  } else {
    for (var in var_types[[1]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, TRUE, FALSE, mixing_type(var))
      )
    }
    for (var in c(var_types[[2]], if (choice_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alt == base)]) {
        overview <- rbind(
          overview,
          c(paste0(var, delimiter, alt[j]),
            if (var == "ASC") NA_character_ else var,
            alt[j], FALSE, TRUE, mixing_type(var))
        )
      }
    }
    for (var in var_types[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(paste0(var, delimiter, alt[j]), var, alt[j],
            TRUE, TRUE, mixing_type(var))
        )
      }
    }
  }
  colnames(overview) <- c(
    "name", "covariate", "alternative", "as_covariate", "as_effect", "mixing"
  )
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$mixing <- factor(
    overview$mixing, levels = c("none", "normal", "log-normal"), ordered = TRUE
  )

  ### sort effects
  effect_order <- order(
    overview$mixing,                 ### re last, log-normal behind normal
    as.numeric(rownames(overview)),  ### otherwise sort by occurrence in formula
    decreasing = FALSE
  )
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL

  ### return effects
  structure(
    overview,
    class = c("choice_effects", "data.frame")
  )
}

#' Compute number of (fixed and random) model effects
#'
#' @description
#' These functions compute the number of fixed and random model effects.
#'
#' \code{compute_P()} computes the total number \code{P} of model effects.
#' \code{compute_P_f()} computes the number \code{P_f} of fixed model effects.
#' \code{compute_P_r()} computes the number \code{P_r} of random model effects.
#'
#' @inheritParams choice_formula
#' @inheritParams choice_parameters
#' @inheritParams choice_effects
#' @inheritParams choice_alternatives
#'
#' @inheritSection choice_formula Model formula
#' @inheritSection choice_formula Random effects
#'
#' @return
#' An \code{integer}, the number of model effects.
#'
#' @examples
#' formula <- choice ~ A | B | C + D
#' re <- c("A", "D+")
#' J <- 3
#' compute_P(formula, re, J)
#' compute_P_f(formula, re, J)
#' compute_P_r(formula, re, J)
#'
#' @export

compute_P <- function(formula, re, J, ordered = FALSE) {
  compute_P_f(formula = formula, re = re, J = J, ordered = ordered) +
    compute_P_r(formula = formula, re = re, J = J, ordered = ordered)
}

#' @rdname compute_P
#' @export

compute_P_f <- function(formula, re, J, ordered = FALSE) {
  effects <- choice_effects(
    choice_formula = choice_formula(formula = formula, re = re, ordered = ordered),
    choice_alternatives = choice_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(effects$mixing == "none"))
}

#' @rdname compute_P
#' @export

compute_P_r <- function(formula, re, J, ordered = FALSE) {
  effects <- choice_effects(
    choice_formula = choice_formula(formula = formula, re = re, ordered = ordered),
    choice_alternatives = choice_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(effects$mixing != "none"))
}

#' @rdname choice_formula
#' @export

is.choice_effects <- function(x, error = TRUE) {
  check_not_missing(x)
  check <- inherits(x, "choice_effects")
  if (isTRUE(error) && !isTRUE(check)) {
    var_name <- oeli::variable_name(x)
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_effects}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_formula
#' @exportS3Method

print.choice_effects <- function(x, ...) {
  is.choice_effects(x, error = TRUE)
  cli::cli_h3("Choice effects")
  print.data.frame(x)
}

