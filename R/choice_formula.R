#' Define choice formula
#'
#' @description
#' This function constructs an object of class \code{\link{choice_formula}},
#' which defines the formula for a choice model.
#'
#' @param formula (`formula`)\cr
#' A symbolic description of the choice model, see details.
#'
#' @param re (`character()`)\cr
#' The names of covariates in \code{formula} that have a random effect,
#' see details.
#'
#' @inheritParams choice_alternatives
#'
#' @return
#' An object of class \code{\link{choice_formula}}, which is a \code{list} with
#' the following elements:
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
#' The structure of \code{formula} is
#' \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the discrete response variable,
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
#' In the ordered model case (\code{ordered = TRUE}), covariates are not
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

choice_formula <- function(formula, re = NULL, ordered = FALSE) {

  ### input checks
  formula <- check_formula(formula)
  re <- check_re(re)
  ordered <- check_ordered(ordered)

  ### read formula
  formula_parts <- as.character(formula)
  if (length(formula_parts) != 3) {
    cli::cli_abort(
      "Input {.var formula} should be of the form {.val <choice> ~ <covariates>}",
      call = NULL
    )
  }
  var_types <- trimws(
    strsplit(formula_parts[3], split = "|", fixed = TRUE)[[1]]
  )
  if (length(var_types) > 3) {
    cli::cli_abort(
      "Input {.var formula} should have no more than two '|' separators",
      call = NULL
    )
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
      cli::cli_abort(
        "Vertical bars in {.var formula} are not allowed in the ordered case",
        call = NULL
      )
    }
    var_types <- list(character(), unlist(var_types[1:3]), character())
  }
  if (any(duplicated(unlist(var_types)))) {
    dup_ind <- which(duplicated(unlist(var_types)))[1]
    cov_dup <- unlist(var_types)[dup_ind]
    cli::cli_abort(
      "Input {.var formula} contains covariate {.val {cov_dup}} multiple times",
      call = NULL
    )
  }
  choice <- formula_parts[2]
  if (choice %in% unlist(var_types)) {
    cli::cli_abort(
      "Variable {.val {choice}} cannot occur on both sides of {.var formula}",
      call = NULL
    )
  }
  re_n <- re[!endsWith(re, "+")]
  re_ln <- sub(".{1}$", "", re[endsWith(re, "+")])
  if (length(intersect(re_n, re_ln)) != 0) {
    re_double <- intersect(re_n, re_ln)[1]
    cli::cli_abort(
      "Input {.var re} cannot include both {.val {re_double}} and {.val {re_double}+}",
      call = NULL
    )
  }
  for (re_val in c(re_n, re_ln)) {
    if (!re_val %in% c(unlist(var_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var re} contains {.val {re_val}}, but it is not on the right
        hand side of {.var formula}",
        call = NULL
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
    class = c("choice_formula", "list")
  )
}

#' @noRd

is.choice_formula <- function(
    x, error = TRUE, var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_formula")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class {.cls choice_formula}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_formula
#' @inheritParams doc-helper
#' @exportS3Method

print.choice_formula <- function(x, ...) {
  is.choice_formula(x, error = TRUE)
  cli::cli_h3("Choice formula")
  cli::cat_line(deparse1(x$formula))
  if (length(x$re) > 0) {
    cli::cat_line("with random effects")
    cli::cat_bullet(x$re)
  }
  invisible(x)
}

