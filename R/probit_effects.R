#' Define probit model effects
#'
#' @description
#' This function constructs an object of class
#' \code{\link{probit_effects}}, which defines the model effects.
#'
#' @param probit_formula
#' A \code{\link{probit_formula}} object.
#' @param probit_alternatives
#' A \code{\link{probit_alternatives}} object.
#' @param delimiter
#' A \code{character}, the delimiter between covariate and alternative name
#' to build the effect name.
#' By default, \code{delimiter = "_"}.
#'
#' @inheritSection probit_formula Model formula
#' @inheritSection probit_formula Random effects
#' @inheritSection probit_alternatives Base alternative
#'
#' @return
#' A \code{\link{probit_effects}} object, which is a \code{data.frame}, where
#' each row is an effect, and columns are
#' 1. \code{"name"}, the effect name (composed of covariate and alternative
#'    name),
#' 2. \code{"covariate"}, the covariate name,
#' 3. \code{"alternative"}, the alternative name (only if the effect is
#'    alternative-specific, i.e., varies across alternatives),
#' 4. \code{"as_covariate"}, indicator whether the covariate is
#'    alternative-specific,
#' 5. \code{"as_effect"}, indicator whether the effect is alternative-specific,
#' 6. \code{"random"}, indicator whether the effect is a random effect,
#' 7. and \code{"log_normal"}, indicator whether the random effect is
#'    log-normal.
#'
#' The effects are ordered as follows: Fixed effects come before random effects,
#' and log-normal random effects are last random effects. Otherwise, the order
#' is determined by occurrence in \code{formula}.
#'
#' @examples
#' probit_effects(
#'   probit_formula = probit_formula(
#'     formula = choice ~ price | income | comfort,
#'     re = c("price+", "income")
#'   ),
#'   probit_alternatives = probit_alternatives(J = 3)
#' )
#'
#' @export

probit_effects <- function(
    probit_formula, probit_alternatives, delimiter = "_"
) {

  ### input checks
  if (missing(probit_formula)) {
    stop("Please specify the input 'probit_formula'.")
  }
  checkmate::assert_class(probit_formula, "probit_formula")
  if (missing(probit_alternatives)) {
    stop("Please specify the input 'probit_alternatives'.")
  }
  checkmate::assert_class(probit_alternatives, "probit_alternatives")
  checkmate::assert_string(delimiter, n.chars = 1)

  ### build probit effects
  J <- probit_alternatives$J
  alt <- probit_alternatives$alternatives
  base <- probit_alternatives$base
  ordered <- probit_alternatives$ordered
  var_types <- probit_formula$var_types
  re_n <- probit_formula$re_n
  re_ln <- probit_formula$re_ln
  re <- c(re_n, re_ln)
  overview <- data.frame(matrix(ncol = 7, nrow = 0))
  if (ordered){
    for (var in var_types[[2]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, FALSE, FALSE, var %in% re, var %in% re_ln)
      )
    }
  } else {
    for (var in var_types[[1]]) {
      overview <- rbind(
        overview,
        c(var, var, NA_character_, TRUE, FALSE, var %in% re, var %in% re_ln)
      )
    }
    for (var in c(var_types[[2]], if (probit_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alt == base)]) {
        overview <- rbind(
          overview,
          c(paste0(var, delimiter, alt[j]),
            if (var == "ASC") NA_character_ else var,
            alt[j], FALSE, TRUE, var %in% re, var %in% re_ln)
        )
      }
    }
    for (var in var_types[[3]]) {
      for (j in 1:J) {
        overview <- rbind(
          overview,
          c(paste0(var, delimiter, alt[j]), var, alt[j],
            TRUE, TRUE, var %in% re, var %in% re_ln)
        )
      }
    }
  }
  colnames(overview) <- c("name", "covariate", "alternative", "as_covariate",
                          "as_effect", "random", "log_normal")
  overview$as_covariate <- as.logical(overview$as_covariate)
  overview$as_effect <- as.logical(overview$as_effect)
  overview$random <- as.logical(overview$random)
  overview$log_normal <- as.logical(overview$log_normal)

  ### sort effects
  effect_order <- order(
    as.numeric(overview$random),     ### put random effects last
    as.numeric(overview$log_normal), ### log-normal effects are last random
    as.numeric(rownames(overview)),  ### otherwise sort by occurrence in formula
    decreasing = FALSE
  )
  overview <- overview[effect_order, ]
  rownames(overview) <- NULL

  ### return effects
  structure(
    overview,
    class = c("probit_effects", "data.frame")
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
#' @inheritParams probit_formula
#' @inheritParams probit_parameter
#' @inheritParams probit_effects
#'
#' @inheritSection probit_formula Model formula
#' @inheritSection probit_formula Random effects
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
  effects <- probit_effects(
    probit_formula = probit_formula(
      formula = formula, re = re, ordered = ordered
    ),
    probit_alternatives = probit_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(!effects$random))
}

#' @rdname compute_P
#' @export

compute_P_r <- function(formula, re, J, ordered = FALSE) {
  effects <- probit_effects(
    probit_formula = probit_formula(formula = formula, re = re,
                                        ordered = ordered),
    probit_alternatives = probit_alternatives(J = J, ordered = ordered)
  )
  as.integer(sum(effects$random))
}

