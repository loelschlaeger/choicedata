#' Define choice model effects
#'
#' @description
#' This function constructs an object of class \code{choice_effects}, which
#' defines the effects of a choice model.
#'
#' @param choice_formula (`choice_formula`)\cr
#' The \code{\link{choice_formula}} object that defines the choice effects.
#'
#' @param choice_alternatives (`choice_alternatives`)\cr
#' The \code{\link{choice_alternatives}} object that defines the choice effects.
#'
#' @param delimiter (`character(1)`)\cr
#' A delimiter between covariate name, alternative, and class number (if any)
#' to build the effect name.
#'
#' @return
#' A \code{choice_effects} object, which is a \code{data.frame}, where each row
#' is a model effect, and columns are
#' 1. \code{"name"}, the effect name, composed of covariate name, alternative
#'    name, and class number (if any),
#' 2. \code{"covariate"}, the covariate name connected to the effect,
#' 3. \code{"alternative"}, the alternative name connected to the effect (only
#'    if the effect is alternative-specific, i.e., varies across alternatives),
#' 4. \code{"as_covariate"}, indicator whether the covariate is
#'    alternative-specific,
#' 5. \code{"as_effect"}, indicator whether the effect is alternative-specific,
#' 6. \code{"lc_effect"}, indicator whether the effect has latent classes,
#' 7. \code{"mixing"}, a factor with levels \code{"none"}, \code{"normal"},
#'    and \code{"log-normal"}, indicating the type of random effect.
#'
#' For identification, the effects are ordered as follows:
#'
#' - Non-random effects come before random effects.
#' - Normal random effects come before log-normal random effects.
#' - Otherwise, the order is determined by occurrence in \code{formula}.
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
  C <- check_C(C = C, latent_classes = choice_formula$latent_classes)
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
      if (var %in% latent_classes) {
        for (class in seq_len(C)) {
          var_name <- paste(var, class, sep = delimiter)
          overview <- rbind(
            overview,
            c(
              var_name, var, NA_character_, class,
              FALSE, FALSE, TRUE, mixing_types[var]
            )
          )
        }
      } else {
        overview <- rbind(
          overview,
          c(
            var, var, NA_character_, NA_integer_,
            FALSE, FALSE, FALSE, mixing_types[var]
          )
        )
      }
    }
  } else {
    for (var in var_types[[1]]) {
      if (var %in% latent_classes) {
        for (class in seq_len(C)) {
          var_name <- paste(var, class, sep = delimiter)
          overview <- rbind(
            overview,
            c(
              var_name, var, NA_character_, class,
              TRUE, FALSE, TRUE, mixing_types[var]
            )
          )
        }
      } else {
        overview <- rbind(
          overview,
          c(
            var, var, NA_character_, NA_integer_,
            TRUE, FALSE, FALSE, mixing_types[var]
          )
        )
      }
    }
    for (var in c(var_types[[2]], if (choice_formula$ASC) "ASC")) {
      for (j in (1:J)[-which(alt == base)]) {
        if (var %in% latent_classes) {
          for (class in seq_len(C)) {
            var_name <- paste(var, alt[j], class, sep = delimiter)
            overview <- rbind(
              overview,
              c(
                var_name, if (var == "ASC") NA_character_ else var,
                alt[j], class, FALSE, TRUE, TRUE, mixing_types[var]
              )
            )
          }
        } else {
          var_name <- paste(var, alt[j], sep = delimiter)
          overview <- rbind(
            overview,
            c(
              var_name, if (var == "ASC") NA_character_ else var,
              alt[j], NA_integer_, FALSE, TRUE, FALSE, mixing_types[var]
            )
          )
        }
      }
    }
    for (var in var_types[[3]]) {
      for (j in 1:J) {
        if (var %in% latent_classes) {
          for (class in seq_len(C)) {
            var_name <- paste(var, alt[j], class, sep = delimiter)
            overview <- rbind(
              overview,
              c(
                var_name, var, alt[j], class, TRUE, TRUE, TRUE,
                mixing_types[var]
              )
            )
          }
        } else {
          var_name <- paste(var, alt[j], sep = delimiter)
          overview <- rbind(
            overview,
            c(
              var_name, var, alt[j], NA_integer_, TRUE, TRUE, FALSE,
              mixing_types[var]
            )
          )
        }
      }
    }
  }
  colnames(overview) <- c(
    "name", "covariate", "alternative", "class", "as_covariate", "as_effect",
    "lc_effect", "mixing"
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

  ### return effects
  structure(
    overview,
    class = c("choice_effects", "data.frame"),
    choice_formula = choice_formula,
    choice_alternatives = choice_alternatives,
    delimiter = delimiter
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
#' @param x (`choice_effects`)\cr
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

#' Compute number of model effects
#'
#' @description
#' These helper functions count the number of model effects:
#'
#' - \code{compute_P()} returns the total number \code{P} of model effects.
#' - \code{compute_P_f()} returns the number \code{P_f} of non-random effects.
#' - \code{compute_P_r()} returns the number \code{P_r} of random effects.
#'
#' @param choice_effects (`choice_effects`)\cr
#' The \code{\link{choice_effects}} object that defines the choice effects.
#'
#' @inheritSection choice_formula Specifying the model formula
#' @inheritSection choice_alternatives Ordered choice alternatives
#'
#' @inheritParams choice_formula
#' @inheritParams choice_alternatives
#'
#' @return
#' An \code{integer}, the number of model effects.

compute_P <- function(choice_effects) {
  P_f <- compute_P_f(choice_effects)
  P_r <- compute_P_r(choice_effects)
  as.integer(P_f + P_r)
}

#' @rdname compute_P

compute_P_f <- function(choice_effects) {
  as.integer(sum(is.na(choice_effects$mixing)))
}

#' @rdname compute_P

compute_P_r <- function(choice_effects) {
  as.integer(sum(!is.na(choice_effects$mixing)))
}

