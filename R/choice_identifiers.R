#' Define choice identifiers
#'
#' @description
#' The \code{choice_identifiers} object defines identifiers for the deciders and
#' the choice occasions.
#'
#' @inheritParams choice_data
#'
#' @param column_decider \[`character(1)`\]\cr
#' The name of the \code{data.frame} column with identifier for the deciders.
#'
#' @param column_occasion \[`character(1)` | `NULL`\]\cr
#' The name of the \code{data.frame} column with identifier for the choice
#' occasions.
#'
#' Can also be \code{NULL} in the cross-sectional case.
#'
#' @param as_cross_section \[`logical(1)`\]\cr
#' Treat choice data as cross-sectional?
#'
#' @return
#' An object of class \code{choice_identifiers} object, which is a
#' \code{data.frame} with two columns:
#'
#' 1. \code{column_decider} contains the decider ids,
#' 2. \code{column_occasion} contains the choice occasion ids.
#'
#' @examples
#' generate_choice_identifiers(N = 2, Tp = 2)
#'
#' @export

choice_identifiers <- function(
  data_frame,
  column_decider = "deciderID",
  column_occasion = "occasionID",
  as_cross_section = FALSE
) {
  as_cross_section <- check_as_cross_section(as_cross_section)
  column_occasion <- check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = as_cross_section
  )
  data_frame <- check_data_frame(
    data_frame, required_columns = c(column_decider, column_occasion)
  )
  decider_ids <- as.character(data_frame[[column_decider]])
  if (anyNA(decider_ids)) {
    cli::cli_abort(
      "Column {.val {column_decider}} of {.var data_frame} must not have NAs"
    )
  }
  if (is.null(column_occasion)) {
    if (anyDuplicated(decider_ids)) {
      cli::cli_abort(
        "Column {.val {column_decider}} of {.var data_frame} must not have
        duplicated values if there are no identifiers for the choice occasions"
      )
    }
    occasion_ids <- rep("1", length(decider_ids))
    column_occasion <- "occasionID"
  } else {
    occasion_ids <- as.character(data_frame[[column_occasion]])
    if (anyNA(occasion_ids)) {
      cli::cli_abort(
        "Column {.val {column_occasion}} of {.var data_frame} must not have NAs"
      )
    }
    for (decider_id in unique(decider_ids)) {
      if (anyDuplicated(occasion_ids[which(decider_ids == decider_id)])) {
        cli::cli_abort(
          "Column {.val {column_occasion}} of {.var data_frame} must have unique
          values for any decider, but decider {.val {decider_id}} has duplicates"
        )
      }
    }
  }
  if (as_cross_section) {
    if (anyDuplicated(decider_ids)) {
      decider_ids <- paste(decider_ids, occasion_ids, sep = ".")
    }
    occasion_ids <- rep("1", length(decider_ids))
  }
  structure(
    data.frame(as.factor(decider_ids), as.factor(occasion_ids)),
    "names" = c(column_decider, column_occasion),
    "class" = c("choice_identifiers", "data.frame")
  )
}

#' @noRd

is.choice_identifiers <- function(
    x, error = FALSE, var_name = oeli::variable_name(x)
  ) {
  check_not_missing(x, var_name = var_name)
  check <- inherits(x, "choice_identifiers")
  if (isTRUE(error) && !isTRUE(check)) {
    cli::cli_abort(
      "Input {.var {var_name}} must be an object of class
      {.cls choice_identifiers}",
      call = NULL
    )
  } else {
    isTRUE(check)
  }
}

#' @rdname choice_identifiers
#'
#' @param x \[`choice_identifiers`\]\cr
#' The `choice_identifiers` object to be printed.
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_identifiers <- function(x, ...) {
  is.choice_identifiers(x, error = TRUE)
  print.data.frame(x)
  invisible(x)
}

#' @rdname choice_identifiers
#' @inheritParams expand_Tp

generate_choice_identifiers <- function(
  N, Tp = 1, column_decider = "deciderID", column_occasion = "occasionID"
) {
  Tp <- expand_Tp(N = N, Tp = Tp)
  column_occasion <- check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = all(Tp == 1)
  )
  if (is.null(column_occasion)) {
    column_occasion <- "occasionID"
  }
  identifiers <- structure(
    data.frame(
      rep(1:N, times = Tp),                          # decider ids
      unlist(sapply(Tp, seq.int, simplify = FALSE))  # choice occasion ids
    ),
    "names" = c(column_decider, column_occasion)
  )
  choice_identifiers(
    data_frame = identifiers,
    column_decider = column_decider,
    column_occasion = column_occasion,
    as_cross_section = is.null(column_occasion)
  )
}

#' Expand \code{Tp}
#'
#' @description
#' This helper function expands the number of choice occasions \code{Tp} to a
#' \code{vector} of length \code{N}.
#'
#' @param N \[`integer(1)`\]\cr
#' The number of deciders.
#'
#' @param Tp \[`integer(1)` | `integer(N)`\]\cr
#' The constant number of choice occasions per decider.
#'
#' Can also be of length \code{N} for a variable number of choice occasions per
#' decider.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.
#'
#' @keywords internal

expand_Tp <- function(N, Tp = 1) {
  check_not_missing(N)
  N <- check_N(N)
  Tp <- check_Tp(Tp, N = N)
  checkmate::assert_int(N, lower = 1)
  checkmate::assert_numeric(Tp)
  if (length(Tp) == 1) {
    Tp <- rep(Tp, N)
  }
  checkmate::assert_integerish(Tp, lower = 1, len = N, any.missing = FALSE)
  as.integer(Tp)
}
