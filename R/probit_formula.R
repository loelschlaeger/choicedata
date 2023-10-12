#' Define probit model formula
#'
#' @description
#' This function constructs an object of class \code{\link{probit_formula}},
#' which defines the formula for a probit model.
#'
#' @param formula
#' A \code{\link[stats]{formula}}, a symbolic description of the probit model,
#' see details.
#' @param re
#' A \code{character}, the vector of names of covariates in \code{formula} that
#' should have a random effect, see details.
#' By default, \code{re = NULL}, i.e., no random effects.
#' @inheritParams probit_alternatives
#'
#' @return
#' A \code{\link{probit_formula}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{formula}}{The model formula.}
#'   \item{\code{re}}{The names of covariates with random effects.}
#'   \item{\code{ordered}}{Are the choice alternatives ordered?}
#'   \item{\code{choice}}{The name of the dependent variable.}
#'   \item{\code{var_types}}{The three different types of covariates.}
#'   \item{\code{ASC}}{Does the model have alternative specific constants?}
#'   \item{\code{re_n}}{The covariates with normal mixing distribution.}
#'   \item{\code{re_ln}}{The covariates with log-normal mixing distribution.}
#' }
#'
#' @section Model formula:
#' The structure of \code{formula} should be
#' \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the dependent variable (the choices),
#'   \item \code{A} are names of \strong{alternative specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item \code{B} are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and \code{C} are names of \strong{alternative specific covariates}
#'   with \strong{alternative specific coefficients}.
#' }
#'
#' Multiple covariates of one type are separated by a \code{+} sign, i.e.,
#' \code{choice ~ A1 + A2}.
#'
#' By default, alternative specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+ 0} in the second spot, i.e.,
#' \code{choice ~ A | B + 0 | C}. To not include any covariates of
#' the second category but to estimate ASCs, add \code{1} in the second
#' spot, e.g., \code{choice ~ A | 1 | C}. The expression
#' \code{choice ~ A | 0 | C} is interpreted as no covariates of the second
#' category and no ASCs.
#'
#' In the ordered probit model (\code{ordered = TRUE}), covariates are not
#' alternative specific, i.e., there exists only one type of covariate.
#' Therefore, the \code{formula} object does not need the special
#' separation form via \code{|}, and hence has the simple structure
#' \code{choice ~ A + B + C}.
#' ASCs cannot be estimated in the ordered case.
#'
#' @section Random effects:
#' Covariates can have random effects, i.e., their coefficients can follow a
#' random distribution. Per default, the distribution is normal. The log-normal
#' distribution (e.g., for sign-restriction) can be specified via appending
#' \code{+} to the corresponding name in \code{re}.
#' To have random effects for the ASCs, add \code{ASC} (or \code{ASC+}) to
#' \code{re}.
#'
#' @export

probit_formula <- function(formula, re = NULL, ordered = FALSE) {

  ### input checks
  if (missing(formula)) {
    stop("Please specify the input 'formula'.")
  }
  checkmate::assert_formula(formula)
  if (is.null(re)) {
    re <- character()
  }
  checkmate::assert_character(re, any.missing = FALSE, unique = TRUE)
  checkmate::assert_flag(ordered)

  ### read formula
  formula_parts <- as.character(formula)
  if (length(formula_parts) != 3) {
    stop("'formula' should be in the form '<choice> ~ <covariates>'.")
  }
  var_types <- trimws(
    strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]]
  )
  if (length(var_types) > 3) {
    stop("'formula' should have no more than 2 of '|' separators.")
  }
  while (length(var_types) < 3) {
    var_types <- c(var_types, NA_character_)
  }
  var_types <- lapply(strsplit(var_types, split = "+", fixed = TRUE), trimws)
  ASC <- if (ordered) FALSE else ifelse(0 %in% var_types[[2]], FALSE, TRUE)
  var_types <- lapply(var_types, function(x) x[!x %in% c(0, 1, NA)])
  if (ordered) {
    ### in the ordered case, 'var_types' has only variables in second position
    if (grepl("|", formula[3], fixed = TRUE)) {
      stop("Vertical bars in 'formula' are not allowed in the ordered case.")
    }
    var_types <- list(character(), unlist(var_types[1:3]), character())
  }
  if (any(duplicated(unlist(var_types)))) {
    dup_ind <- which(duplicated(unlist(var_types)))[1]
    cov_dup <- unlist(var_types)[dup_ind]
    stop("'formula' contains covariate '", cov_dup, "' multiple times.")
  }
  choice <- formula_parts[2]
  if (choice %in% unlist(var_types)) {
    stop("Variable '", choice, "' cannot occur on both sides of 'formula'.")
  }
  re_n <- re[!endsWith(re, "+")]
  re_ln <- sub(".{1}$", "", re[endsWith(re, "+")])
  if (length(intersect(re_n, re_ln)) != 0) {
    re_double <- intersect(re_n, re_ln)[1]
    stop("'re' cannot include both '", re_double, "' and '", re_double, "+'.")
  }
  for (re_val in c(re_n, re_ln)) {
    if (!re_val %in% c(unlist(var_types), if(ASC) "ASC")) {
      stop(
        "'re' contains '", re_val,
        "', but it's not on the right side of 'formula'."
      )
    }
  }

  ### build object
  structure(
    list(
      formula = formula,
      re = re,
      ordered = ordered,
      choice = choice,
      var_types = var_types,
      ASC = ASC,
      re_n = re_n,
      re_ln = re_ln
    ),
    class = "probit_formula"
  )
}

#' @rdname probit_formula
#' @param x
#' An \code{\link{probit_formula}} object.
#' @export

is.probit_formula <- function(x) {
  inherits(x, "probit_formula")
}

#' @rdname probit_formula
#' @exportS3Method
#' @param ...
#' Currently not used.

print.probit_formula <- function(x, ...) {
  checkmate::assert_class(x, "probit_formula")
  cat("Model formula:", deparse1(x$formula))
  if (length(x$re) > 0) {
    cat("\nRandom effects:", x$re)
  }
  cat("\n")
}

