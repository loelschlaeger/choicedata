#' Define choice identifiers
#'
#' @description
#' The \code{choice_identifiers} object defines identifiers for the deciders and
#' the choice occasions.
#'
#' @inheritParams choice_data
#'
#' @param column_decider \[`character(1)`\]\cr
#' The name of the \code{data_frame} column with identifier for the deciders.
#'
#' @param column_occasion \[`character(1)` | `NULL`\]\cr
#' The name of the \code{data_frame} column with identifier for the choice
#' occasions.
#'
#' Can also be \code{NULL} for the cross-sectional case.
#'
#' @param as_cross_section \[`logical(1)`\]\cr
#' Treat choice data as cross-sectional?
#'
#' @return
#' An object of class \code{choice_identifiers}, which is a \code{data.frame}
#' with columns:
#'
#' 1. \code{column_decider} contains the decider ids,
#' 2. \code{column_occasion} contains the choice occasion ids
#'    (only if `column_occasion` is not `NULL` and `as_cross_section = FALSE`).
#'
#' @examples
#' ### general (panel) case
#' generate_choice_identifiers(N = 2, Tp = 2)
#'
#' ### cross-sectional case
#' generate_choice_identifiers(N = 5, column_occasion = NULL)
#'
#' @export

choice_identifiers <- function(
  data_frame,
  column_decider = "deciderID",
  column_occasion = "occasionID",
  as_cross_section = FALSE
) {

  ### input checks
  as_cross_section <- check_as_cross_section(as_cross_section)
  column_occasion <- check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = as_cross_section
  )
  data_frame <- check_data_frame(
    data_frame, required_columns = c(column_decider, column_occasion)
  )

  ### id checks
  decider_ids <- as.character(data_frame[[column_decider]])
  if (anyNA(decider_ids)) {
    cli::cli_abort(
      "Column {.val {column_decider}} of {.var data_frame} must not have NAs",
      call = NULL
    )
  }
  if (is.null(column_occasion)) {
    if (anyDuplicated(decider_ids)) {
      cli::cli_abort(
        "Column {.val {column_decider}} of {.var data_frame} must not have
        duplicated values if there are no identifiers for the choice occasions",
        call = NULL
      )
    }
    occasion_ids <- rep("1", length(decider_ids))
    column_occasion <- "occasionID"
  } else {
    occasion_ids <- as.character(data_frame[[column_occasion]])
    if (anyNA(occasion_ids)) {
      cli::cli_abort(
        "Column {.val {column_occasion}} of {.var data_frame} must not have NAs",
        call = NULL
      )
    }
    for (decider_id in unique(decider_ids)) {
      if (anyDuplicated(occasion_ids[which(decider_ids == decider_id)])) {
        cli::cli_abort(
          "Column {.val {column_occasion}} of {.var data_frame} must have unique
          values for any decider, but decider {.val {decider_id}} has duplicates",
          call = NULL
        )
      }
    }
  }

  ### build identifiers
  if (as_cross_section) {
    ### transform to cross-section
    decider_ids <- decider_ids_to_cross_section(
      decider_ids = decider_ids, occasion_ids = occasion_ids, delimiter = "."
    )
    structure(
      data.frame(as.factor(decider_ids)),
      "names" = c(column_decider),
      "class" = c("choice_identifiers", "data.frame"),
      column_decider = column_decider,
      as_cross_section = TRUE
    )
  } else {
    structure(
      data.frame(as.factor(decider_ids), as.factor(occasion_ids)),
      "names" = c(column_decider, column_occasion),
      "class" = c("choice_identifiers", "data.frame"),
      column_decider = column_decider,
      column_occasion = column_occasion,
      as_cross_section = FALSE
    )
  }
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
#' @inheritParams oeli::print_data.frame
#'
#' @param ...
#' Currently not used.
#'
#' @exportS3Method

print.choice_identifiers <- function(x, rows = NULL, row.names = FALSE, ...) {
  is.choice_identifiers(x, error = TRUE)
  oeli::print_data.frame(x, rows = rows, digits = NULL, row.names = row.names)
  invisible(x)
}

#' @rdname choice_identifiers
#' @inheritParams expand_Tp

generate_choice_identifiers <- function(
  N,
  Tp = 1,
  column_decider = "deciderID",
  column_occasion = "occasionID"
) {

  ### input checks
  Tp <- expand_Tp(N = N, Tp = Tp)
  column_occasion <- check_column_occasion(
    column_occasion, column_decider = column_decider, null.ok = TRUE
  )
  as_cross_section <- is.null(column_occasion)
  if (is.null(column_occasion)) {
    column_occasion <- "occasionID"
  }

  ### generate identifiers
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
    as_cross_section = as_cross_section
  )

}

#' Unique decider ids in cross-sectional case
#'
#' @description
#' This helper function makes unique decider ids for a given combination of
#' decider and occasion ids.
#'
#' @param decider_ids,occasion_ids \[`atomic()`\]\cr
#' An `atomic` `vector` of ids.
#'
#' @return
#' An `atomic` `vector` of unique ids.
#'
#' @keywords internal

decider_ids_to_cross_section <- function(
    decider_ids, occasion_ids, delimiter = "."
  ) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_atomic_vector(decider_ids),
    var_name = "decider_ids"
  )
  oeli::input_check_response(
    check = checkmate::check_atomic_vector(
      occasion_ids, len = length(decider_ids)
    ),
    var_name = "occasion_ids"
  )
  check_delimiter(delimiter)

  ### transform to cross-section
  if (anyDuplicated(decider_ids)) {
    decider_ids <- paste(decider_ids, occasion_ids, sep = delimiter)
  }
  decider_ids

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

#' Read \code{Tp}
#'
#' @description
#' This helper function reads the number of choice occasions \code{Tp} from a
#' \code{\link{choice_identifiers}} object.
#'
#' @param choice_identifiers \[`choice_identifiers`\]\cr
#' The \code{\link{choice_identifiers}} object that defines the choice
#' occasions.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}, where \code{N} is the
#' number of deciders.
#'
#' @keywords internal

read_Tp <- function(choice_identifiers) {

  ### input checks
  is.choice_identifiers(choice_identifiers, error = TRUE)
  column_decider <- attr(choice_identifiers, "column_decider")
  column_occasion <- attr(choice_identifiers, "column_occasion")

  ### read Tp
  if (is.null(column_occasion)) {
    rep(1L, times = nrow(choice_identifiers))
  } else {
    as.integer(table(choice_identifiers[, column_decider]))
  }

}
