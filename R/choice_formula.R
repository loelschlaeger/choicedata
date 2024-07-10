#' Define choice formula
#'
#' @description
#' This function constructs an object of class \code{choice_formula}, which
#' defines the formula for a choice model.
#'
#' @param formula (`formula`)\cr
#' A symbolic description of the choice model, see details.
#'
#' @param error_term (`character()`)\cr
#' Defines the model's error term. Current options are:
#' - `"logit"`: errors are assumed to be iid standard Gumbel distributed
#' - `"probit"`: errors are assumed to be multivariate normally distributed
#'
#' @param re (`character()`)\cr
#' The names of covariates in \code{formula} with a random effect, see details.
#'
#' @return
#' An object of class \code{choice_formula}, which is a \code{list} with the
#' following elements:
#' \describe{
#'   \item{\code{formula}}{The model formula.}
#'   \item{\code{error_term}}{The model's error term.}
#'   \item{\code{choice}}{The name of the discrete response variable.}
#'   \item{\code{var_types}}{The three different types of covariates.}
#'   \item{\code{ASC}}{Does the model have alternative-specific constants?}
#'   \item{\code{mixing_types}}{The types of random effects (if any).}
#'   \item{\code{ordered_valid}}{Formula valid for ordered case (see details)?}
#' }
#'
#' @section Model formula:
#' The structure of \code{formula} is
#' \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the discrete response variable,
#'   \item \code{A} are names of \strong{alternative-specific covariates} with
#'   \strong{a coefficient that is constant across alternatives},
#'   \item \code{B} are names of \strong{covariates that are constant across
#'   alternatives},
#'   \item and \code{C} are names of \strong{alternative-specific covariates}
#'   with \strong{alternative-specific coefficients}.
#' }
#'
#' Multiple covariates of one type are separated by a \code{+} sign, e.g.,
#' \code{choice ~ A1 + A2}.
#'
#' By default, alternative-specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+ 0} in the second spot, e.g.,
#' \code{choice ~ A | B + 0 | C}. To not include any covariates of
#' the second type but to estimate ASCs, add \code{1} in the second
#' spot, e.g., \code{choice ~ A | 1 | C}. The expression
#' \code{choice ~ A | 0 | C} is interpreted as no covariates of the second
#' type and no ASCs.
#'
#' Some parts of the formula can be omitted when there is no ambiguity. For
#' example, `choice ~ A` is equivalent to `choice ~ A | 1 | 0`.
#'
#' In the ordered case, since only a single utility is modeled, no ASCs and no
#' alternative-specific covariates can be included. Hence, in the ordered case,
#' \code{formula} must be of the form, e.g.,  \code{choice ~ 0 | A + B + 0}.
#'
#' @section Random effects:
#' Covariates can have random effects, i.e., their coefficients can follow a
#' random distribution (a so-called mixing distribution).
#' Per default, the mixing distribution is normal. In addition, the log-normal
#' distribution (e.g., for sign-restriction) can be specified via appending
#' \code{+} to the corresponding name in \code{re}.
#' To have random effects for the ASCs, add \code{ASC} (or \code{ASC+}) to
#' \code{re}.
#'
#' @examples
#' choice_formula(
#'   formula = choice ~ A | B | C,
#'   error_term = "probit",
#'   re = c("A", "B+")
#' )
#'
#' @export

choice_formula <- function(formula, error_term = "probit", re = NULL) {

  ### input checks
  check_formula(formula)
  check_error_term(error_term)
  check_re(re)

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
  ASC <- ifelse(0 %in% var_types[[2]], FALSE, TRUE)
  var_types <- lapply(var_types, function(x) x[!x %in% c(0, 1, NA)])
  if ("ASC" %in% unlist(var_types)) {
    cli::cli_abort(
      "Covariates named {.val ASC} in {.var formula} are not allowed",
      call = NULL
    )
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

  ### determine mixing types
  mixing_types <- character()
  for (re_entry in re) {
    if (endsWith(re_entry, "+")) {
      re_entry <- sub(".{1}$", "", re_entry)
      mixing_type <- "log-normal"
    } else {
      mixing_type <- "normal"
    }
    if (re_entry %in% names(mixing_types)) {
      cli::cli_abort(
        "Multiple random effects specifications for {.val {re_entry}} detected",
        call = NULL
      )
    } else if (!re_entry %in% c(unlist(var_types), if(ASC) "ASC")) {
      cli::cli_abort(
        "Input {.var re} contains {.val {re_entry}}, but it is not on the right
        hand side of {.var formula}",
        call = NULL
      )
    } else {
      mixing_types[re_entry] <- mixing_type
    }
  }

  ### check if formula is valid for the ordered case
  ordered_valid <- length(var_types[[1]]) == 0 & length(var_types[[3]]) == 0 & !ASC

  ### build object
  structure(
    list(
      formula = formula,
      error_term = error_term,
      choice = choice,
      var_types = var_types,
      ASC = ASC,
      mixing_types = mixing_types,
      ordered_valid = ordered_valid
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
      "Input {.var {var_name}} must be an object of class
      {.cls choice_formula}",
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
  ul <- cli::cli_ul()
  cli::cli_li(deparse1(x$formula))
  cli::cli_li(paste("error term:", x$error_term))
  if (length(x$mixing_types) > 0) {
    cli::cli_li("random effects:")
    cli::cli_ol(paste0(names(x$mixing_types), ": ", x$mixing_types))
  }
  cli::cli_end(ul)
  invisible(x)
}

